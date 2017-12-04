package controllers

import java.sql.Timestamp
import java.time.format.TextStyle
import java.time.{Instant, ZoneId}
import java.util.concurrent.TimeUnit
import java.util.{Locale, UUID}
import javax.inject.Inject

import bot.JDAExtensions._
import bot._
import modules.{ApiConfigHelper, DiscordHelper, RedditHelper, SecurityModule}
import net.dv8tion.jda.core.JDA
import net.dv8tion.jda.core.entities._
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
import pac4j.DiscordProfile
import play.api.Logger
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc.Results.EmptyContent
import play.api.mvc._
import play.core.j.JavaHelpers
import play.libs.concurrent.HttpExecutionContext

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.control.NonFatal

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
  ws: WSClient
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
    profileAttrs ++= profile.getAttributes.asScala.mapValues(x => JsString(s"[${x.getClass}] $x")) // TODO: Map Java types to JSON types
    Json.obj(
      "profile" -> JsObject(profileAttrs.result())
    )
  }

  /**
    * Accept an invite on behalf of a user.
    *
    * @note Doesn't use JDA because JDA doesn't have a way to initialize with an OAuth2 token and no web socket.
    *
    * @param accessToken OAuth2 access token for that user.
    * @param invite Invite object (we only use the code).
    * @return Invite data returned by server.
    */
  def acceptInvite(accessToken: String, invite: Invite): Future[JsValue] = {
    ws
      .url(s"https://discordapp.com/api/invites/${invite.getCode}")
      .withHeaders("Authorization" -> s"Bearer $accessToken")
      .post(EmptyContent())
      .map(_.json)
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

  def verify(guildShortName: String): Action[AnyContent] = Action.async {


    implicit request =>

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

    def writeVerificationLog(stepName: String, verifySessionUUID: String, jsonData: JsValue): Future[Message] = {
      guild
        // DEBUG: create temporary new channels for each verified user instead of reusing this one.
        .getTextChannelsByName(guildConfig.verificationChannelName, false)
        .asScala
        .headOption
        .getOrElse {
          throw new RuntimeException(s"#${guildConfig.verificationChannelName} doesn't exist yet!")
        }
        .sendMessage(s"*UUID*: `$verifySessionUUID`\n*step*: `$stepName`\n```\n${Json.prettyPrint(jsonData)}\n```")
        .future()
    }

    /**
      * Prepare for authenticating against an external IdP.
      *
      * @note Sets a pac4j session attribute that will return the user back here.
      *
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

    request.session.get("verifySessionUUID") match {
      case None =>
        val stepName = VerificationSteps.names(0)

        // Start verification flow by generating a verify session UUID and persisting it.
        val verifySessionUUID = UUID.randomUUID().toString

        val dataFields = Seq.newBuilder[(String, JsValue)]
        dataFields += ("ip" -> JsString(request.remoteAddress))
        val headers = request.headers
        headers.get("Referer").foreach(x => dataFields += ("referrer" -> JsString(x)))
        headers.get("User-Agent").foreach(x => dataFields += ("user_agent" -> JsString(x)))
        headers.get("Accept-Language").foreach(x => dataFields += ("accept_language" -> JsString(x)))
        val xForwardedForList = headers.getAll("X-Forwarded-For")
        if (xForwardedForList.nonEmpty) {
          dataFields += ("x_forwarded_for" -> JsArray(xForwardedForList.map(JsString)))
        }
        val forwardedList = headers.getAll("Forwarded")
        if (forwardedList.nonEmpty) {
          dataFields += ("forwarded" -> JsArray(forwardedList.map(JsString)))
        }
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
                  val jsonData = profileToStepData(profile)
                  val step = newVerificationStepWithUUID(stepName, jsonData)
                  for {
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
                  val jsonData = profileToStepData(profile)
                  val step = newVerificationStepWithUUID(stepName, jsonData)
                  for {
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
                          case NonFatal(e) => Json.obj("elevateError" -> e.getMessage)
                        }
                    } else {
                      Future.successful(Json.obj("elevatedTo" -> JsNull))
                    }
                  }

                  for {
                    invite <- createInvite
                    acceptResult <- acceptInvite(profile.getAccessToken, invite)
                    member = guild.getMemberById(profile.getId)
                    elevateResult <- elevate(member)
                    jsonData = Json.obj("accept" -> acceptResult, "elevate" -> elevateResult)
                    _ <- writeVerificationLog(stepName, verifySessionUUID, jsonData)
                    step = newVerificationStepWithUUID(stepName, jsonData)
                    result <- showNextStep(step) { steps =>
                      val profileURL = routes.HomeController.userProfile(
                        guildShortName = guildConfig.shortName,
                        userID = profile.getId
                      )
                        .toString
                      Ok(views.html.verify_050_complete(guildShortName, steps, profileURL))
                        // Clear verifySessionUUID. The user is fully verified at this point.
                        .removingFromSession(verifySessionUUIDKey)
                    }
                  } yield result
              }

            case unknown => throw new IllegalStateException(s"Last step can't be $unknown!")
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
