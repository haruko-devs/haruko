package modules

import bot.BotListener
import net.dv8tion.jda.core.{AccountType, JDA, JDABuilder}
import play.api.{Configuration, Environment}
import play.api.inject._

class DiscordModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = {
    val baseURL = configuration.getString("baseURL").getOrElse {
      throw new RuntimeException("No baseURL in Play config!")
    }

    val discordConfigBlock = configuration.getConfig("discord").getOrElse {
      throw new RuntimeException("No discord block in Play config!")
    }
    val botToken = discordConfigBlock.getString("botToken").getOrElse {
      throw new RuntimeException("No clientID in Play discord config block!")
    }
    val cmdPrefix = discordConfigBlock.getString("cmdPrefix").getOrElse {
      throw new RuntimeException("No cmdPrefix in Play discord config block!")
    }
    val colorRolePrefix = discordConfigBlock.getString("colorRolePrefix").getOrElse {
      throw new RuntimeException("No colorRolePrefix in Play discord config block!")
    }
    val guildIDs = discordConfigBlock.getStringSeq("guildIDs").getOrElse {
      throw new RuntimeException("No guildIDs in Play discord config block!")
    }

    val bot = BotListener(cmdPrefix, colorRolePrefix, guildIDs.toSet, baseURL)

    val jda = new JDABuilder(AccountType.BOT)
      .setToken(botToken)
      .addListener(bot)
      .buildAsync()

    Seq(
      bind[JDA].toInstance(jda).eagerly()
    )
  }
}
