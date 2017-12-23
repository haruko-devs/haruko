package controllers

import java.net.{CookieManager, URL}
import java.sql.Timestamp
import java.time.format.TextStyle
import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneId}
import java.util.concurrent.TimeUnit
import java.util.{Date, Locale, UUID}
import javax.inject.Inject

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.{Future, blocking}
import scala.util.control.NonFatal

import play.api.Logger
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Writes._
import play.api.libs.json._
import play.api.libs.ws.{WSClient, WSRequest, WSResponse}
import play.api.mvc.Results.EmptyContent
import play.api.mvc._
import play.core.j.JavaHelpers
import play.libs.concurrent.HttpExecutionContext

import com.blueconic.browscap.UserAgentParser
import com.google.common.net.InetAddresses
import net.dean.jraw.RedditClient
import net.dean.jraw.http._
import net.dean.jraw.http.oauth.OAuthData
import net.dean.jraw.http.oauth.OAuthHelper.AuthStatus
import net.dean.jraw.paginators.{Paginator, UserSubredditsPaginator}
import net.dv8tion.jda.core.entities._
import net.dv8tion.jda.core.{JDA, Permission}
import okhttp3.OkHttpClient
import org.pac4j.core.client.IndirectClient
import org.pac4j.core.config.Config
import org.pac4j.core.context.Pac4jConstants
import org.pac4j.core.credentials.Credentials
import org.pac4j.core.profile.{CommonProfile, ProfileManager, UserProfile}
import org.pac4j.core.redirect.RedirectAction
import org.pac4j.oauth.profile.OAuth20Profile
import org.pac4j.play.PlayWebContext
import org.pac4j.play.http.DefaultHttpActionAdapter
import org.pac4j.play.scala.Security
import org.pac4j.play.store.PlaySessionStore

import bot.JDAExtensions._
import bot._
import modules._
import pac4j.DiscordProfile
import verification.{FireholNetsets, GeoIP}
import verification.InetAddressExtensions._

/**
  * @param config Used by pac4j Security trait.
  * @param playSessionStore Used by pac4j Security trait.
  * @param ec Used by pac4j Security trait.
  */
class HomeController @Inject() (
  protected override val config: Config,
  protected override val playSessionStore: PlaySessionStore,
  override protected val ec: HttpExecutionContext,
  val messagesApi: MessagesApi,
  botConfig: BotConfig,
  verificationSteps: VerificationSteps,
  jdaLauncher: JDALauncher,
  ws: WSClient,
  userAgentConfig: UserAgentConfig,
  userAgentParser: UserAgentParser,
  geoIP: GeoIP,
  fireholNetsets: FireholNetsets
) extends Controller with Security[OAuth20Profile] with I18nSupport {

  val jda: JDA = jdaLauncher.jda

  /**
    * PAC4J construct for getting PAC4J profiles out of Play's storage.
    */
  def newPlayWebContext(request: Request[AnyContent]): PlayWebContext = {
    new PlayWebContext(
      JavaHelpers.createJavaContext(request),
      playSessionStore
    )
  }

  def index: Action[AnyContent] = Action {
    val env = mutable.LinkedHashMap.empty[String, String]
    env("status") = "OK"
    env("authed") = "false"
    Ok(views.html.index(env))
  }

  def indexAuthed: Action[AnyContent] = {
    Secure(SecurityModule.all.map(_.name).mkString(","), authorizers = null, multiProfile = true) {
      profiles => Action {
        val env = mutable.LinkedHashMap.empty[String, String]
        env("status") = "OK"
        env("authed") = "true"
        profiles.foreach { profile =>
          env += (profile.getClientName -> "---")
          profile.getAttributes.asScala.foreach { case (key, value) =>
            env += (s"${profile.getClientName} $key" -> value.toString)
          }
        }
        Ok(views.html.index(env))
      }
    }
  }

  /**
    * Require the user to authenticate with a specific client.
    */
  def login(clientName: String): Action[AnyContent] = Action { request =>
    // TODO: redirect back to the verification hub, which doesn't exist yet.
    val client = config.getClients.findClient(clientName)
    val playWebContext = newPlayWebContext(request)
    val httpAction = client.redirect(playWebContext)
    val httpActionAdapter = config.getHttpActionAdapter.asInstanceOf[DefaultHttpActionAdapter]
    val javaResult = httpActionAdapter.adapt(httpAction.getCode, playWebContext)
    JavaHelpers.createResult(playWebContext.getJavaContext, javaResult)
  }

  /**
    * Show a list of time zones recognized by the time zone commands.
    */
  def timezones: Action[AnyContent] = Action {
    val sortedZoneIDs = ZoneId.getAvailableZoneIds.asScala.toIndexedSeq
      // Drop obviously legacy zone IDs in the default Java TZDB.
      .filter(_.contains("/"))
      .filterNot(id => Seq("Etc/", "SystemV/", "US/").exists(prefix => id.startsWith(prefix)))
      // Drop zone IDs that are alternate names for offset-based zones.
      .filter { id =>
        val zoneid = ZoneId.of(id)
        val normalized = zoneid.normalized()
        zoneid == normalized
      }
      .sortBy { id =>
        id.split("/") match {
          case Array(continent, region, sub) => (continent, region, sub)
          case Array(continent, region) => (continent, region, "")
          case Array(continent) => (continent, "", "")
          case _ => ("", "", "")
        }
      }

    val zoneDescs = mutable.LinkedHashMap.empty[String, String]
    sortedZoneIDs.foreach { id =>
      val zoneid = ZoneId.of(id)
      zoneDescs(id) = zoneid.getDisplayName(TextStyle.FULL, Locale.US)
    }
    Ok(views.html.timezones(zoneDescs))
  }

  /**
    * Play form mapper for [[TOSFormData]].
    */
  val tosForm = Form(
    mapping(
      "agreed" -> checked("You must agree to the terms of service to continue."),
      "fingerprint" -> text,
      "componentsJSON" -> text,
      "timeZone" -> text
    )(TOSFormData.apply)(TOSFormData.unapply)
  )

  def profileToStepData[T <: CommonProfile](profile: T): JsValue = {
    val profileAttrs = Seq.newBuilder[(String, JsValue)]
    profileAttrs += "id" -> JsString(profile.getId)
    profileAttrs ++= profile.getAttributes.asScala.mapValues {
      // TODO: there has to be a prettier way to do this.
      case null => JsNull
      case x: java.lang.Boolean => JsBoolean(x)
      case x: java.math.BigDecimal => JsNumber(x)
      case x: java.lang.Double => JsNumber(BigDecimal(x))
      case x: java.lang.Float => JsNumber(BigDecimal(x.toDouble))
      case x: java.lang.Integer => JsNumber(BigDecimal(x))
      case x: java.lang.Long => JsNumber(BigDecimal(x))
      case x: String => JsString(x)
      case x => JsString(x.toString)
    }
    JsObject(profileAttrs.result())
  }

  /**
    * @note Doesn't use JDA because JDA doesn't have a way to initialize with an OAuth2 token and no web socket.
    *
    * @param method GET, POST, etc. method on WSRequest.
    * @param path URL relative to the Discord API base.
    * @param accessToken OAuth2 access token for a user.
    * @return A JSON document.
    */
  def discordCall(method: WSRequest => Future[WSResponse], path: String, accessToken: String): Future[JsValue] = {
    method(
      ws
        .url(s"https://discordapp.com/api/$path")
        .withHeaders(
          "Authorization" -> s"Bearer $accessToken",
          "User-Agent" -> userAgentConfig.userAgent
        )
    )
      .map(_.json)
  }

  /**
    * Accept an invite on behalf of a user.
    *
    * @param accessToken OAuth2 access token for that user.
    * @param invite Invite object (we only use the code).
    * @return Invite data returned by server.
    */
  def acceptInvite(accessToken: String, invite: Invite): Future[JsValue] = {
    discordCall(
      _.post(EmptyContent()),
      s"invites/${invite.getCode}",
      accessToken
    )
  }

  /**
    * Get a user's guilds (aka servers).
    *
    * @param accessToken OAuth2 access token for that user.
    * @return Guild data returned by server.
    */
  def getDiscordGuilds(accessToken: String): Future[JsValue] = {
    discordCall(
      _.get(),
      "users/@me/guilds",
      accessToken
    ).recover {
      case NonFatal(e) => Json.obj("error" -> e.getMessage)
    }
  }

  /**
    * Get a user's guilds (aka servers).
    *
    * @param accessToken OAuth2 access token for that user.
    * @return Guild data returned by server.
    */
  def getDiscordConnections(accessToken: String): Future[JsValue] = {
    discordCall(
      _.get(),
      "users/@me/connections",
      accessToken
    ).recover {
      case NonFatal(e) => Json.obj("error" -> e.getMessage)
    }
  }

  /**
    * @note Exists because JRAW doesn't have a way to provide a token directly.
    *
    * @param accessToken Reddit OAuth2 access token we already have from a user login.
    * @return JRAW Reddit client forced to use that token.
    */
  def getRedditClientForUser(accessToken: String): RedditClient = {

    /**
      * @note Hack to deal with Reddit's double encoding of strings in JSON:
      * @see https://www.reddit.com/dev/api#response_body_encoding about raw_json=1
      * @see https://github.com/mattbdean/JRAW/pull/166 for PR that didn't make it into JRAW
      *
      * TODO: make this a generic class so we can use it on other HttpAdapter implementations.
      */
    val rawJsonHttpAdapter = new HttpAdapter[OkHttpClient]() {

      val wrapped = new OkHttpAdapter()

      override def execute(request: HttpRequest): RestResponse = {
        val rawJSON = "raw_json=1"
        val origQuery = Option(request.getUrl.getQuery)
        if (origQuery.exists(_.contains(rawJSON))) {
          // This shouldn't happen unless JRAW finally adds support, but just in case…
          wrapped.execute(request)
        } else {
          val patchedURL = if (origQuery.isDefined) {
            new URL(s"${request.getUrl}&$rawJSON")
          } else {
            new URL(s"${request.getUrl}?$rawJSON")
          }
          val requestBuilder = HttpRequest.Builder
            .from(request.getMethod, patchedURL)
            .basicAuth(request.getBasicAuthData)
            .expected(request.getExpectedType)
            .sensitiveArgs(request.getSensitiveArgs: _*)
          request.getHeaders.toMultimap.asScala.foreach { case (name, values) =>
            values.asScala.foreach { value =>
              requestBuilder.header(name, value)
            }
          }
          val patchedRequest = requestBuilder.build()
          wrapped.execute(patchedRequest)
        }
      }

      override def getConnectTimeout: Int = wrapped.getConnectTimeout
      override def getCookieManager: CookieManager = wrapped.getCookieManager
      override def getDefaultHeaders: java.util.Map[String, String] = wrapped.getDefaultHeaders
      override def getNativeClient: OkHttpClient = wrapped.getNativeClient
      override def getProxy: java.net.Proxy = wrapped.getProxy
      override def getReadTimeout: Int = wrapped.getReadTimeout
      override def getWriteTimeout: Int = wrapped.getWriteTimeout
      override def isFollowingRedirects: Boolean = wrapped.isFollowingRedirects
      override def setConnectTimeout(timeout: Long, unit: TimeUnit): Unit = wrapped.setConnectTimeout(timeout, unit)
      override def setCookieManager(manager: CookieManager): Unit = wrapped.setCookieManager(manager)
      override def setFollowRedirects(flag: Boolean): Unit = wrapped.setFollowRedirects(flag)
      override def setProxy(proxy: java.net.Proxy): Unit = wrapped.setProxy(proxy)
      override def setReadTimeout(timeout: Long, unit: TimeUnit): Unit = wrapped.setReadTimeout(timeout, unit)
      override def setWriteTimeout(timeout: Long, unit: TimeUnit): Unit = wrapped.setWriteTimeout(timeout, unit)
    }

    val redditClient = new RedditClient(
      UserAgent.of(userAgentConfig.userAgent),
      rawJsonHttpAdapter
    )

    val oauthHelper = redditClient.getOAuthHelper
    val authStatus = oauthHelper.getClass.getDeclaredField("authStatus")
    authStatus.setAccessible(true)
    authStatus.set(oauthHelper, AuthStatus.AUTHORIZED)

    redditClient.authenticate(new OAuthData(AuthenticationMethod.WEBAPP, null) {
      // Set in pac4j config.
      override def getScopes: Array[String] = RedditHelper.scopes.toArray
      // Got this from pac4j login.
      override def getAccessToken: String = accessToken
      // Always "bearer" according to the docs for this class. Not appparently used anywhere anyway.
      override def getTokenType: String = "bearer"
      // Most Reddit tokens only last for an hour. We're going to use this for a few seconds.
      override def getExpirationDate: Date = Date.from(Instant.now().plus(1, ChronoUnit.HOURS))
      // Don't have a refresh token and don't need it.
      override def getRefreshToken: String = null
    })
    redditClient.ensuring(_.hasActiveUserContext)
  }

  /**
    * Get the subreddits that a user's subscribed to.
    * TODO: do you need to be subscribed to be a contributor or moderator?
    */
  def getSubreddits(redditClient: RedditClient): Future[JsValue] = {
    Future {
      blocking {
        val pager = new UserSubredditsPaginator(redditClient, "subscriber")
        pager.setLimit(Paginator.RECOMMENDED_MAX_LIMIT)
        JsArray(
          pager.asScala.flatMap(_.asScala).toStream.map { subreddit =>
            JsonNodeWrites.writes(subreddit.getDataNode)
          }
        )
      }
    }.recover {
      case NonFatal(e) => Json.obj("error" -> e.getMessage)
    }
  }

  def userProfile(guildShortName: String, userID: String): Action[AnyContent] = {
    Secure(DiscordHelper.name) { profiles =>
      Action {
        val guildConfig = botConfig.guilds(guildShortName)
        // TODO: various fetching of data
        Ok(views.html.user_profile(guildConfig.shortName))
      }
    }
  }

  def verify(guildShortName: String): Action[AnyContent] = Action.async { implicit request =>

    val playWebContext: PlayWebContext = newPlayWebContext(request)
    val profileManager = new ProfileManager[CommonProfile](playWebContext)
    val guildConfig: GuildConfig = botConfig.guilds(guildShortName)
    val guildID: String = guildConfig.id
    val guild: Guild = jda.getGuildById(guildID)

    def newVerificationStep(verifySessionUUID: String)(name: String, data: JsValue): VerificationStep = {
      VerificationStep(
        guildID = guildID,
        verifySessionUUID = verifySessionUUID,
        name = if (VerificationSteps.names.contains(name)) name else {
          throw new IllegalArgumentException(s"Unknown verification step name: $name")
        },
        ts = Timestamp.from(Instant.now()),
        data = Json.toJson(data).toString()
      )
    }

    def showNextStep(step: VerificationStep)(f: Seq[VerificationStep] => Result): Future[Result] = {
      for {
        _ <- verificationSteps.insert(step)
        steps <- verificationSteps.all(guildID, step.verifySessionUUID)
      } yield {
        f(steps)
      }
    }

    def sendVerificationMessage(guildConfig: GuildConfig, guild: Guild, logText: String): Future[Message] = {
      guild
        .getTextChannelsByName(guildConfig.verificationChannelName, false)
        .asScala
        .headOption
        .getOrElse {
          throw new RuntimeException(s"#${guildConfig.verificationChannelName} doesn't exist yet!")
        }
        .sendMessage(logText)
        .future()
    }

    def writeVerificationLog(stepName: String, verifySessionUUID: String, jsonData: JsValue): Future[Message] = {
      // DEBUG builds only, or write to console
      Future.successful(null)

//      val jsonDataText = Json.prettyPrint(jsonData)
//      val fullLogText = s"*UUID*: `$verifySessionUUID`\n*step*: `$stepName`\n```\n$jsonDataText\n```"
//      val discordMaxMsgLength = 2000
//      val logText = if (fullLogText.length > discordMaxMsgLength) {
//        val shortenBy = fullLogText.length - discordMaxMsgLength + 1 // for ellipsis
//        val shortJsonDataText = jsonDataText.substring(0, jsonDataText.length - shortenBy)
//        s"*UUID*: `$verifySessionUUID`\n*step*: `$stepName`\n```\n$shortJsonDataText…\n```"
//      } else {
//        fullLogText
//      }
//
//      sendVerificationMessage(guildConfig, guild, logText)
    }

    def writeVerificationFinal(discordID: String, verifySessionUUID: String): Future[Message] = {
      sendVerificationMessage(
        guildConfig,
        guild,
        s"Verified <@$discordID>: ${botConfig.baseURL}${routes.HomeController.verifyLog(guildShortName, verifySessionUUID)}"
      )
    }

    /**
      * Prepare for authenticating against an external IdP.
      *
      * @note Sets a pac4j session attribute that will return the user back here.
      * @return URL to the IdP's authentication page with our auth callback.
      */
    def prepExternalAuth(helper: ApiConfigHelper)(resultFromLoginURL: String => Result): Result = {
      // Redirect to /verify/:guildShortName after authenticating.
      playWebContext.setSessionAttribute(
        Pac4jConstants.REQUESTED_URL,
        routes.HomeController.verify(guildShortName = guildShortName).toString
      )

      // Get the IdP redirect URL. Also sets the pac4j session.
      val client = config
        .getClients
        .findClient(helper.name)
        .asInstanceOf[IndirectClient[_ <: Credentials, _ <: CommonProfile]]
      val redirect = client.getRedirectAction(playWebContext)
      assert(redirect.getType == RedirectAction.RedirectType.REDIRECT)
      assert(redirect.getLocation != null)

      // Setting REQUESTED_URL in the PlayWebContext means we have to now
      // get any new session contents out of the underlying JavaContext.
      var result = resultFromLoginURL(redirect.getLocation)
      val javaContext = playWebContext.getJavaContext
      if (javaContext.session.isDirty) {
        result = result.withSession(Session(javaContext.session.asScala.toMap))
      }
      if (javaContext.flash.isDirty) {
        result = result.flashing(Flash(javaContext.flash.asScala.toMap))
      }

      result
    }

    def getProfileFromSession[T <: UserProfile](helper: ApiConfigHelper): Option[T] = {
      val readFromSession = true
      profileManager.getAll(readFromSession).asScala
        .find(_.getClientName == helper.name)
        .map(_.asInstanceOf[T])
    }

    val verifySessionUUIDKey = "verifySessionUUID"

    def restartVerificationFlow(warning: String)(implicit request: RequestHeader): Future[Result] = {
      Future.successful(
        Redirect(routes.HomeController.verify(guildShortName = guildShortName))
          .removingFromSession(verifySessionUUIDKey)
          .flashing("warning" -> warning)
      )
    }

    request.session.get(verifySessionUUIDKey) match {
      case None =>
        val stepName = VerificationSteps.names(0)

        // Start verification flow by generating a verify session UUID and persisting it.
        val verifySessionUUID = UUID.randomUUID().toString

        val dataFields = Seq.newBuilder[(String, JsValue)]
        dataFields += ("ip" -> JsString(request.remoteAddress))

        request.getQueryString("source").foreach(x => dataFields += ("source" -> JsString(x)))

        val headers = request.headers

        headers.get("Referer").foreach(x => dataFields += ("referrer" -> JsString(x)))

        headers.get("User-Agent").foreach { userAgent =>
          dataFields += ("user_agent" -> JsString(userAgent))
          dataFields += ("browser" -> JsObject(
              userAgentParser
                .parse(userAgent)
                .getValues
                .asScala
                .map { case (field, value) =>
                  field.toString.toLowerCase -> JsString(value)
                }
            )
          )
        }

        headers.get("Accept-Language").foreach(x => dataFields += ("accept_language" -> JsString(x)))

        val xForwardedForList = headers.getAll("X-Forwarded-For")
        if (xForwardedForList.nonEmpty) {
          dataFields += ("x_forwarded_for" -> JsArray(xForwardedForList.map(JsString)))
        }

        val forwardedList = headers.getAll("Forwarded")
        if (forwardedList.nonEmpty) {
          dataFields += ("forwarded" -> JsArray(forwardedList.map(JsString)))
        }

        val allRelatedIPs: Set[String] = Set(request.remoteAddress) ++ xForwardedForList ++ forwardedList
        dataFields += ("ip_metadata" -> JsObject(allRelatedIPs.toSeq.map { ipStr =>
          val ip = InetAddresses.forString(ipStr)
          val ipMetadata = Json.obj(
            "firehol" -> fireholNetsets.json(ip),
            "is_special" -> ip.isSpecial
          ) ++ geoIP.json(ip)
          ipStr -> ipMetadata
        }))

        val jsonData = JsObject(dataFields.result())
        val step = newVerificationStep(verifySessionUUID)(stepName, jsonData)
        for {
          _ <- writeVerificationLog(stepName, verifySessionUUID, jsonData)
          result <- showNextStep(step) { steps =>
            Ok(views.html.verify_010_tos(guildShortName, steps, tosForm))
              .withSession(verifySessionUUIDKey -> verifySessionUUID)
          }
        } yield result

      case Some(verifySessionUUID) =>
        val newVerificationStepWithUUID = newVerificationStep(verifySessionUUID) _

        verificationSteps.latest(guildID, verifySessionUUID).flatMap { lastStep =>
          VerificationSteps.nextStepName(lastStep.map(_.name)) match {
            case Some(stepName) if stepName == VerificationSteps.names(0) =>
              Logger.error("No last step in database but has a verifySessionUUID.")
              restartVerificationFlow("Something went wrong. Restarting the verification process.")

            case Some(stepName) if stepName == VerificationSteps.names(1) =>
              tosForm.bindFromRequest()(request).fold(
                (formWithErrors: Form[TOSFormData]) => {
                  for {
                    // TODO: and show error
                    steps <- verificationSteps.all(guildID, verifySessionUUID)
                  } yield {
                    BadRequest(views.html.verify_010_tos(guildShortName, steps, formWithErrors))
                      .flashing("warning" -> "The form was missing something.")
                  }
                },
                (tosFormData: TOSFormData) => {
                  // TODO: https://www.playframework.com/documentation/2.5.x/ScalaJsonAutomated
                  val jsonData = Json.obj(
                    "agreed" -> tosFormData.agreed,
                    "fingerprint" -> tosFormData.fingerprint,
                    "components" -> Json.parse(tosFormData.componentsJSON),
                    "timezone" -> tosFormData.timeZone
                  )
                  val step = newVerificationStepWithUUID(stepName, jsonData)
                  for {
                    _ <- writeVerificationLog(stepName, verifySessionUUID, jsonData)
                    result <- showNextStep(step) { steps =>
                      // Show Discord auth page.
                      prepExternalAuth(DiscordHelper) { loginURL =>
                        Ok(views.html.verify_020_discord(guildShortName, steps, loginURL))
                      }
                    }
                  } yield result
                }
              )

            case Some(stepName) if stepName == VerificationSteps.names(2) =>
              Logger.trace("Store Discord profile.")
              getProfileFromSession[DiscordProfile](DiscordHelper) match {
                case None =>
                  restartVerificationFlow(
                    "Something went wrong with your Discord login." +
                      " Restarting the verification process.")

                case Some(profile) =>
                  val profileData = profileToStepData(profile)
                  val guildsTask = getDiscordGuilds(profile.getAccessToken)
                  val connectionsTask = getDiscordConnections(profile.getAccessToken)
                  for {
                    guildsData <- guildsTask
                    connectionsData <- connectionsTask
                    jsonData = Json.obj(
                      "profile" -> profileData,
                      "guilds" -> guildsData,
                      "connections" -> connectionsData
                    )
                    step = newVerificationStepWithUUID(stepName, jsonData)
                    _ <- writeVerificationLog(stepName, verifySessionUUID, jsonData)
                    result <- showNextStep(step) { steps =>
                      Logger.trace("Show Reddit auth page.")
                      prepExternalAuth(RedditHelper) { loginURL =>
                        Ok(views.html.verify_030_reddit(guildShortName, steps, loginURL))
                      }
                    }
                  } yield result
              }

            case Some(stepName) if stepName == VerificationSteps.names(3) =>
              Logger.trace("Store Reddit profile.")
              getProfileFromSession[OAuth20Profile](RedditHelper) match {
                case None =>
                  restartVerificationFlow(
                    "Something went wrong with your Reddit login." +
                      " Restarting the verification process.")

                case Some(profile) =>
                  val profileData = profileToStepData(profile)
                  val redditClient = getRedditClientForUser(profile.getAccessToken)
                  val subredditsTask = getSubreddits(redditClient)
                  // TODO: Scan recent post history to build a per-subreddit karma breakdown.
                  for {
                    subredditsData <- subredditsTask
                    jsonData = Json.obj(
                      "profile" -> profileData,
                      "subreddits" -> subredditsData
                    )
                    step = newVerificationStepWithUUID(stepName, jsonData)
                    _ <- writeVerificationLog(stepName, verifySessionUUID, jsonData)
                    result <- showNextStep(step) { steps =>
                      val loginURL = routes.HomeController.verify(guildShortName = guildShortName).toString
                      Ok(views.html.verify_040_invite(guildShortName, steps, loginURL))
                    }
                  } yield result
              }

            // TODO: insert gatekeeper step that runs all the offline analysis.

            case Some(stepName) if stepName == VerificationSteps.names(4) =>
              getProfileFromSession[DiscordProfile](DiscordHelper) match {
                case None =>
                  restartVerificationFlow(
                    "Something went wrong with your Discord login." +
                      " Restarting the verification process.")

                case Some(profile) =>
                  val reason = s"Joining <@${profile.getId}> to $guildShortName"

                  val createInvite: Future[Invite] = guild
                    .getTextChannelsByName(guildConfig.inviteChannelName, false)
                    .asScala
                    .headOption
                    .getOrElse {
                      throw new RuntimeException(s"#${guildConfig.inviteChannelName} doesn't exist yet!")
                    }
                    .createInvite()
                    .setUnique(true)
                    .setMaxUses(1)
                    .setMaxAge(1L, TimeUnit.MINUTES)
                    .setTemporary(true)
                    .reason(reason)
                    .future()

                  val adminRole: Role = guild
                    .getRolesByName(guildConfig.adminRoleName, false)
                    .asScala
                    .headOption
                    .getOrElse {
                      throw new RuntimeException(s"#${guildConfig.inviteChannelName} doesn't exist yet!")
                    }

                  def elevate(member: Member): Future[JsObject] = {
                    if (guildConfig.adminIDs.contains(member.getUser.getId)) {
                      guild
                        .getController
                        .addSingleRoleToMember(member, adminRole)
                        .future()
                        .map(_ => Json.obj("elevatedTo" -> guildConfig.adminRoleName))
                        .recover {
                          case NonFatal(e) => Json.obj("error" -> e.getMessage)
                        }
                    } else {
                      Future.successful(Json.obj("elevatedTo" -> JsNull))
                    }
                  }

                  for {
                    invite <- createInvite
                    acceptResult <- acceptInvite(profile.getAccessToken, invite)
                    // If this is empty, the invite failed, probably because the user is banned.
                    member = Option(guild.getMemberById(profile.getId))
                    isBanned = member.isEmpty
                    elevateResult <- member
                      .map(elevate)
                      .getOrElse {
                        Future.successful(Json.obj("error" -> "Invite failed!"))
                      }
                    jsonData = Json.obj(
                      "discord_id" -> profile.getId,
                      "accept" -> acceptResult,
                      "elevate" -> elevateResult,
                      "is_banned" -> isBanned
                    )
                    _ <- writeVerificationLog(stepName, verifySessionUUID, jsonData)
                    step = newVerificationStepWithUUID(stepName, jsonData)
                    result <- showNextStep(step) { steps =>
                      if (isBanned) {
                        Forbidden(views.html.verify_banned(guildShortName, steps))
                      } else {
                        val profileURL = routes.HomeController.userProfile(
                          guildShortName = guildConfig.shortName,
                          userID = profile.getId
                        )
                          .toString
                        Ok(views.html.verify_050_complete(guildShortName, steps, profileURL))
                          // Clear verifySessionUUID. The user is fully verified at this point.
                          .removingFromSession(verifySessionUUIDKey)
                      }
                    }
                    _ <- writeVerificationFinal(profile.getId, verifySessionUUID)
                  } yield result
              }

            case unknown => throw new IllegalStateException(s"Last step can't be $unknown!")
          }
        }
    }
  }

  def verifyLog(guildShortName: String, verifySessionUUID: String): Action[AnyContent] = {
    Secure(DiscordHelper.name) { profiles =>
      val guildConfig = botConfig.guilds(guildShortName)
      val guild = jda.getGuildById(guildConfig.id)
      val isGuildAdmin = profiles
        .exists {
          case discordProfile: DiscordProfile =>
            val member = guild.getMemberById(discordProfile.getId)
            member.hasPermission(Permission.ADMINISTRATOR) && !member.getUser.isBot
          case _ => false
        }
      if (!isGuildAdmin) {
        Action {
          Forbidden(
            Json.obj(
              "error" -> s"You must be a $guildShortName admin to see this page!"
            )
          )
        }
      } else {
        Action.async {
          verificationSteps.all(guildConfig.id, verifySessionUUID).map { steps =>
            Ok(
              Json.obj(
                "steps" -> JsArray(steps.map { step: VerificationStep =>
                  Json.obj(
                    "step" -> step.name,
                    "time" -> step.ts.toInstant.toString,
                    "data" -> Json.parse(step.data)
                  )
                })
              )
            )
          }
        }
      }
    }
  }
}

case class TOSFormData(
  agreed: Boolean,
  fingerprint: String,
  componentsJSON: String,
  timeZone: String
)
