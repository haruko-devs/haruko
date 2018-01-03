package bot

import javax.inject.{Inject, Singleton}

import scala.concurrent.Future

import play.api.inject.ApplicationLifecycle

import net.dv8tion.jda.core.{AccountType, JDA, JDABuilder}

/**
  * Creates a JDA connection from injectible config.
  */
@Singleton
case class JDALauncher @Inject() (
  config: BotConfig,
  bot: BotListener,
  applicationLifecycle: ApplicationLifecycle
) {
  val jda: JDA = new JDABuilder(AccountType.BOT)
    .setToken(config.botToken)
    .addEventListener(bot)
    .buildAsync()

  applicationLifecycle.addStopHook(() => Future.successful(jda.shutdown()))
}
