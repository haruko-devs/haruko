package modules

import org.pac4j.core.client.Clients
import org.pac4j.core.config.Config
import org.pac4j.oauth.client.OAuth20Client
import org.pac4j.oauth.config.OAuth20Configuration
import org.pac4j.oauth.profile.OAuth20Profile
import org.pac4j.oauth.profile.generic.GenericOAuth20ProfileDefinition
import org.pac4j.play.http.DefaultHttpActionAdapter
import org.pac4j.play.store.{PlayCacheSessionStore, PlaySessionStore}
import org.pac4j.scribe.builder.api.GenericApi20
import play.api.inject._
import play.api.{Configuration, Environment}

/**
  * Plumbing necessary to authenticate against Discord's OAuth2 implementation.
  *
  * @note The callback URL must have ?client_name=Discord appended to it when you put it in the Discord console.
  *       This has something to do with pac4j multi-provider support, which we're not using yet.
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

    val discordConfigBlock = configuration.getConfig("discord").getOrElse {
      throw new RuntimeException("No discord block in Play config!")
    }
    val discordOAuth2Config = new OAuth20Configuration()
    val discordOAuth2API = new GenericApi20(
      "https://discordapp.com/api/oauth2/authorize",
      "https://discordapp.com/api/oauth2/token"
    )
    discordOAuth2Config.setApi(discordOAuth2API)
    discordOAuth2Config.setProfileDefinition(new GenericOAuth20ProfileDefinition()
      // Try not to think too hard about why this cast is necessary.
      .asInstanceOf[
        org.pac4j.oauth.profile.definition.OAuthProfileDefinition[
          _ <: org.pac4j.core.profile.CommonProfile,
          _ <: com.github.scribejava.core.model.Token,
          _ <: org.pac4j.oauth.config.OAuthConfiguration[
            _ <: org.pac4j.core.client.IndirectClient[
              _ <: org.pac4j.core.credentials.Credentials,
              _ <: org.pac4j.core.profile.CommonProfile
            ],
            _ <: com.github.scribejava.core.oauth.OAuthService[
              _ <: com.github.scribejava.core.model.Token
            ],
            _ <: com.github.scribejava.core.model.Token
          ]
        ]
      ]
    )
    discordOAuth2Config.setScope("bot identify")
    discordOAuth2Config.setKey(discordConfigBlock.getString("clientID").getOrElse {
      throw new RuntimeException("No clientID in Play discord config block!")
    })
    discordOAuth2Config.setSecret(discordConfigBlock.getString("clientSecret").getOrElse {
      throw new RuntimeException("No clientSecret in Play discord config block!")
    })
    val discordOAuth2Client = new OAuth20Client[OAuth20Profile]()
    discordOAuth2Client.setConfiguration(discordOAuth2Config)
    discordOAuth2Client.setName(SecurityModule.DiscordOAuth2Client)

    val clients = new Clients(s"$baseURL/callback", discordOAuth2Client)
    val config = new Config(clients)
    config.setHttpActionAdapter(new DefaultHttpActionAdapter())

    Seq(
      bind(classOf[Config]).toInstance(config),
      bind(classOf[PlaySessionStore]).to(classOf[PlayCacheSessionStore])
    )
  }
}

object SecurityModule {
  val DiscordOAuth2Client = "Discord"
}
