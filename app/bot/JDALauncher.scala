package bot

import javax.inject.{Inject, Singleton}

import net.dv8tion.jda.core.{AccountType, JDA, JDABuilder}

/**
  * Creates a JDA connection from injectible config.
  */
@Singleton
class JDALauncher @Inject() (
  config: BotConfig,
  bot: BotListener
) {
  val jda: JDA = new JDABuilder(AccountType.BOT)
    .setToken(config.botToken)
    .addListener(bot)
    .buildAsync()
}
