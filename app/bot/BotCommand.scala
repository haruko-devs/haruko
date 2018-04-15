package bot

import java.util.UUID

import scala.concurrent.{ExecutionContext, Future}

import net.dv8tion.jda.core.MessageBuilder
import net.dv8tion.jda.core.events.message.guild.GuildMessageReceivedEvent

import bot.JDAExtensions._

trait BotCommand {
  def shortDescs: Seq[String]
  def accept: PartialFunction[Seq[String], BotCommandContext => Future[Unit]]
}

case class BotCommandContext
(
  config: CombinedGuildConfig,
  event: GuildMessageReceivedEvent
)(
  implicit val ec: ExecutionContext
) {
  lazy val words: Seq[String] = {
    event.getMessage.getContentRaw.stripPrefix(config.cmdPrefix).split(' ').toSeq
  }
  lazy val guildID: String = event.getGuild.getId
  lazy val uuid: UUID = UUID.randomUUID()

  /**
    * Reply to a user, mentioning them.
    */
  def reply(text: String): Future[Unit] = {
    val message = new MessageBuilder()
      .append(event.getMember)
      .append(": ")
      .append(text)
      .build()
    event.getChannel.sendMessage(message)
      .future()
      .map(_ => ())
  }
}
