package modules

import java.util.concurrent.TimeUnit

import bot.{BotConfig, JDALauncher}
import play.api.{Configuration, Environment}
import play.api.inject._

import scala.concurrent.duration.Duration

class DiscordModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = {
    val discordConfigBlock = configuration.getConfig("discord").getOrElse {
      throw new RuntimeException("No discord block in Play config!")
    }

    val botConfig = BotConfig(
      baseURL = configuration
        .getString("baseURL")
        .getOrElse {
          throw new RuntimeException("No baseURL in Play config!")
        },
      botToken = discordConfigBlock
        .getString("botToken")
        .getOrElse {
          throw new RuntimeException("No botToken in Play discord config block!")
        },
      cmdPrefix = discordConfigBlock
        .getString("cmdPrefix")
        .getOrElse {
          throw new RuntimeException("No cmdPrefix in Play discord config block!")
        },
      colorRolePrefix = discordConfigBlock
        .getString("colorRolePrefix")
        .getOrElse {
          throw new RuntimeException("No colorRolePrefix in Play discord config block!")
        },
      pronounRoleNames = discordConfigBlock
        .getStringSeq("pronounRoleNames")
        .getOrElse {
          throw new RuntimeException("No pronounRoleNames in Play discord config block!")
        }
        .toSet,
      guildIDs = discordConfigBlock
        .getStringSeq("guildIDs")
        .getOrElse {
          throw new RuntimeException("No guildIDs in Play discord config block!")
        }
        .toSet,
      dbTimeout = configuration
        .getMilliseconds("dbTimeout")
        .map(Duration.create(_, TimeUnit.MILLISECONDS))
        .getOrElse {
          throw new RuntimeException("No baseURL in Play config!")
        }
    )

    Seq(
      bind[BotConfig].toInstance(botConfig),
      bind[JDALauncher].toSelf.eagerly()
    )
  }
}
