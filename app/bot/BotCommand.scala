package bot

import java.time.{ZoneId, ZoneOffset}
import java.util.{Locale, UUID}

import scala.concurrent.{ExecutionContext, Future}

import net.dv8tion.jda.core.entities.{Guild, Member}
import net.dv8tion.jda.core.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.core.{JDA, MessageBuilder}

import ext._

trait BotCommand {
  def shortDescs: Seq[String]
  def accept: PartialFunction[Seq[String], BotCommandContext => Future[Unit]]
  val help: String = "help"
}

/**
  * @param botListener Temporary access to a huge class that contains a lot of useful code.
  */
case class BotCommandContext
(
  config: CombinedGuildConfig,
  event: GuildMessageReceivedEvent,
  botListener: BotListener
)(
  implicit val ec: ExecutionContext
) {
  /** @note Normalizes whitespace in the HTML sense.
    * @todo Commands that need full whitespace should use a different parser.
    */
  lazy val words: Seq[String] = {
    event.getMessage.getContentRaw.stripPrefix(config.cmdPrefix).split("""\s+""").toSeq
  }
  lazy val jda: JDA = event.getJDA
  lazy val guild: Guild = event.getGuild
  lazy val guildID: GuildID = GuildID(guild)
  lazy val uuid: UUID = UUID.randomUUID()
  // These always refer to the user who messaged Haruko.
  lazy val member: Member = event.getMember
  lazy val userID: UserID = UserID(member.getUser)
  lazy val timeZone: ZoneId = getTimeZone(userID)
  lazy val pronoun: Pronoun = getPronoun(userID)
  lazy val locale: Locale = Locale.US // TODO
  lazy val isAdmin: Boolean = botListener.checkAdmin(member)

  def getTimeZone(userID: UserID): ZoneId = {
    val member: Member = guild.getMemberById(userID.id)
    botListener
      .getTimezoneRoles(member)
      .headOption
      .map(tzRole => ZoneId.of(tzRole.getName.stripPrefix(config.timezoneRolePrefix)))
      .getOrElse(ZoneOffset.UTC)
  }

  def getPronoun(userID: UserID): Pronoun = {
    val member: Member = guild.getMemberById(userID.id)
    botListener.getAnyPronouns(member)
  }

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
