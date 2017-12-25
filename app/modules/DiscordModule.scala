package modules

import java.time.Clock
import java.util.concurrent.TimeUnit

import bot.{BotConfig, GuildConfig, JDALauncher}
import play.api.{Configuration, Environment}
import play.api.inject._
import scala.collection.JavaConverters._

import scala.concurrent.duration.Duration

class DiscordModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = {
    val discordConfigBlock = configuration.getConfig("discord").getOrElse {
      throw new RuntimeException("No discord block in Play config!")
    }

    val guildsBlock = discordConfigBlock.getConfig("guilds").getOrElse {
      throw new RuntimeException(s"No guilds block in Play discord config block!")
    }
    val guilds = guildsBlock.subKeys.map { shortName =>
      val guildBlock = guildsBlock.getConfig(shortName).get
      shortName -> GuildConfig(
        shortName = shortName,
        id = guildBlock.getString("id").getOrElse {
          throw new RuntimeException(s"No id in Play discord guild config block for guild $shortName!")
        },
        inviteChannelName = guildBlock.getString("inviteChannelName"),
        verificationChannelName = guildBlock.getString("verificationChannelName"),
        adminRoleName = guildBlock.getString("adminRoleName"),
        adminIDs = guildBlock.getStringList("adminIDs").map(_.asScala.toSet).getOrElse(Set.empty)
      )
    }.toMap

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
      timezoneRolePrefix = discordConfigBlock
        .getString("timezoneRolePrefix")
        .getOrElse {
          throw new RuntimeException("No timezoneRolePrefix in Play discord config block!")
        },
      guilds = guilds,
      dbTimeout = configuration
        .getMilliseconds("dbTimeout")
        .map(Duration.create(_, TimeUnit.MILLISECONDS))
        .getOrElse {
          throw new RuntimeException("No dbTimeout in Play config!")
        }
    )

    Seq(
      bind[Clock].toInstance(Clock.systemUTC()),
      bind[BotConfig].toInstance(botConfig),
      bind[JDALauncher].toSelf.eagerly()
    )
  }
}
