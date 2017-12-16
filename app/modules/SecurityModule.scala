package modules

import java.nio.charset.StandardCharsets
import java.time.Instant

import com.github.scribejava.core.builder.api.DefaultApi20
import com.github.scribejava.core.model.{AbstractRequest, OAuthConfig, OAuthConstants, Token}
import com.github.scribejava.core.oauth.{OAuth20Service, OAuthService}
import com.github.scribejava.core.services.Base64Encoder
import org.pac4j.core.client.{Clients, IndirectClient}
import org.pac4j.core.config.Config
import org.pac4j.core.context.WebContext
import org.pac4j.core.credentials.Credentials
import org.pac4j.core.profile.CommonProfile
import org.pac4j.core.profile.converter.{AbstractAttributeConverter, Converters}
import org.pac4j.oauth.client.OAuth20Client
import org.pac4j.oauth.config.{OAuth20Configuration, OAuthConfiguration}
import org.pac4j.oauth.profile.OAuth20Profile
import org.pac4j.oauth.profile.definition.OAuthProfileDefinition
import org.pac4j.oauth.profile.generic.GenericOAuth20ProfileDefinition
import org.pac4j.play.CallbackController
import org.pac4j.play.http.DefaultHttpActionAdapter
import org.pac4j.play.store.{PlayCacheSessionStore, PlaySessionStore}
import org.pac4j.scribe.builder.api.GenericApi20
import pac4j.{DiscordApi, DiscordProfileDefinition}
import play.api.inject._
import play.api.{Configuration, Environment}

/**
  * Plumbing necessary to authenticate against Discord's OAuth2 implementation.
  *
  * @note The callback URL must have ?client_name=discord appended to it when you put it in the Discord console,
  *       or ?client_name=reddit for the Reddit console.
  *       This is how pac4j multi-provider support tells providers apart.
  *
  * @see https://discordapp.com/developers/docs/topics/oauth2
  * @see https://github.com/pac4j/play-pac4j
  * @see http://www.pac4j.org/docs/clients/oauth.html
  */
class SecurityModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = {
    val baseURL = configuration.getString("baseURL").getOrElse {
      throw new RuntimeException("No baseURL in Play config!")
    }

    // Used for access token flow. Without setting a distinctive user agent, Discord and Reddit will refuse authentication.
    val userAgent = s"Haruko/${haruko.BuildInfo.version} (+$baseURL)"

    val oauth2Clients = SecurityModule.all.map(_(configuration, userAgent)).toSeq

    val clients = new Clients(s"$baseURL/callback", oauth2Clients: _*)
    val config = new Config(clients)
    config.setHttpActionAdapter(new DefaultHttpActionAdapter())

    Seq(
      bind(classOf[UserAgentConfig]).toInstance(UserAgentConfig(userAgent)),
      bind(classOf[Config]).toInstance(config),
      bind(classOf[PlaySessionStore]).to(classOf[PlayCacheSessionStore]),
      bind(classOf[CallbackController]).to[MultiProfileCallbackController]
    )
  }
}

// DI holder class for the user agent string. I know, it's ugly. Should use binding keys instead.
case class UserAgentConfig(userAgent: String)

object SecurityModule {
  val all = Set(DiscordHelper, RedditHelper)
}

class MultiProfileCallbackController extends CallbackController {
  setMultiProfile(true)
}

class ApiConfigHelper(
  val name: String,
  val scopes: Set[String],
  val api: DefaultApi20,
  val profileDefinition: OAuthProfileDefinition[_ <: CommonProfile, _ <: Token, _ <: OAuthConfiguration[_ <: IndirectClient[_ <: Credentials, _ <: CommonProfile], _ <: OAuthService[_ <: Token], _ <: Token]]
) {
  /**
    * Create a named pac4j OAuth 2 client from the Play config.
    */
  def apply(configuration: Configuration, userAgent: String): OAuth20Client[OAuth20Profile] = {
    val configBlock = configuration.getConfig(name).getOrElse {
      throw new RuntimeException(s"No $name block in Play config!")
    }
    val oauth2Config = new OAuth20ConfigurationExtra(userAgent: String)
    oauth2Config.setApi(api)
    oauth2Config.setProfileDefinition(profileDefinition)
    oauth2Config.setWithState(true)
    oauth2Config.setScope(scopes.mkString(" "))
    oauth2Config.setKey(configBlock.getString("clientID").getOrElse {
      throw new RuntimeException(s"No clientID in $name config block!")
    })
    oauth2Config.setSecret(configBlock.getString("clientSecret").getOrElse {
      throw new RuntimeException(s"No clientSecret in $name config block!")
    })
    oauth2Config.setTokenAsHeader(true)
    val oauth2Client = new OAuth20Client[OAuth20Profile]()
    oauth2Client.setConfiguration(oauth2Config)
    oauth2Client.setName(name)

    oauth2Client
  }
}

// TODO: upgrade these into first-class pac4j OAuth20 API classes and send them upstream.
// TODO: may be simpler to use JDA and JRAW respectively than do much with the pac4j profile definition classes.

case object DiscordHelper extends ApiConfigHelper(
  name = "discord",
  // See https://discordapp.com/developers/docs/topics/oauth2#shared-resources-oauth2-scopes for full list of scopes.
  scopes = Set("identify", "email", "connections", "guilds", "guilds.join"),
  api = DiscordApi.instance(),
  profileDefinition = new DiscordProfileDefinition()
)

/**
  * @note Hack to deal with Reddit's double encoding of strings in JSON:
  * @see https://www.reddit.com/dev/api#response_body_encoding about raw_json=1
  */
case object RedditHelper extends ApiConfigHelper(
  name = "reddit",
  // See https://www.reddit.com/api/v1/scopes for full list of scopes.
  scopes = Set("identity", "mysubreddits", "read", "history"),
  api = new GenericApi20(
    "https://www.reddit.com/api/v1/authorize",
    "https://www.reddit.com/api/v1/access_token"
  ) {
    // Reddit doesn't like it when you send the client ID and secret in the request body,
    // and <https://tools.ietf.org/html/rfc6749#section-2.3.1> doesn't recommend that anyway.
    // See <https://github.com/reddit/reddit/wiki/OAuth2#token-retrieval-code-flow>.
    override def createService(config: OAuthConfig): OAuth20Service = new OAuth20Service(this, config) {
      override def createAccessTokenRequest[T <: AbstractRequest](code: String, request: T): T = {
        val config = getConfig
        request.addHeader(
          OAuthConstants.HEADER,
          OAuthConstants.BASIC + ' ' +
            Base64Encoder.getInstance.encode(
              String.format(
                "%s:%s",
                config.getApiKey,
                config.getApiSecret
              )
                .getBytes(StandardCharsets.UTF_8)
            )
        )
        request.addParameter(OAuthConstants.CODE, code)
        request.addParameter(OAuthConstants.REDIRECT_URI, config.getCallback)
        val scope = config.getScope
        if (scope != null) request.addParameter(OAuthConstants.SCOPE, scope)
        request.addParameter(OAuthConstants.GRANT_TYPE, OAuthConstants.AUTHORIZATION_CODE)
        request.addParameter("raw_json", "1")
        request
      }
    }
  },
  profileDefinition = new GenericOAuth20ProfileDefinition() {
    setProfileUrl("https://oauth.reddit.com/api/v1/me?raw_json=1")
    primary("id", Converters.STRING)
    primary("name", Converters.STRING)
    secondary("link_karma", Converters.INTEGER)
    secondary("comment_karma", Converters.INTEGER)
    secondary("created_utc", EpochSecondsConverter)
    secondary("gold_expiration", EpochSecondsConverter)
    secondary("suspension_expiration_utc", EpochSecondsConverter)
    secondary("is_employee", Converters.BOOLEAN)
    secondary("is_suspended", Converters.BOOLEAN)
    secondary("is_gold", Converters.BOOLEAN)
    secondary("is_mod", Converters.BOOLEAN)
    secondary("over_18", Converters.BOOLEAN)
    secondary("verified", Converters.BOOLEAN)
    secondary("has_verified_email", Converters.BOOLEAN)
  }
)

/**
  * Eat a number, int or float, of seconds since the epoch, and produce an Instant.
  */
object EpochSecondsConverter extends AbstractAttributeConverter[Instant](classOf[Instant]) {
  override def internalConvert(attribute: Any): Instant = {
    attribute match {
      case x: java.lang.Number => Instant.ofEpochMilli((x.doubleValue() * 1000).toLong)
      case _ => null
    }
  }
}

/**
  * Make it possible to set the user agent used by an OAuth 2 client.
  */
class OAuth20ConfigurationExtra(userAgent: String) extends OAuth20Configuration {

  override def buildOAuthConfig(context: WebContext, state: String): OAuthConfig = {
    val default = super.buildOAuthConfig(context, state)
    new OAuthConfig(
      default.getApiKey,
      default.getApiSecret,
      default.getCallback,
      default.getSignatureType,
      default.getScope,
      null, // Can't get at original debug stream.
      default.getState,
      default.getResponseType,
      userAgent,
      default.getConnectTimeout,
      default.getReadTimeout,
      default.getHttpClientConfig,
      default.getHttpClient
    )
  }
}
