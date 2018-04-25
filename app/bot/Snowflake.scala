package bot

import scala.util.Try
import scala.util.matching.Regex

import net.dv8tion.jda.core.JDA
import net.dv8tion.jda.core.entities._
import slick.lifted.MappedTo

/**
  * Snowflakes are alternate implementations of JDA entities that contain only a snowflake ID.
  * They come with convenient extractors, and can be stored as a column type in a Slick DB.
  */
trait Snowflake[T <: ISnowflake] extends ISnowflake with MappedTo[Long] {
  val id: Long
  def resolve(jda: JDA): Option[T]
  override def value: Long = id
  override def getIdLong: Long = id
}

/**
  * Their extractors will accept a bare numeric snowflake ID.
  */
trait SnowflakeExtractor[T <: ISnowflake, S <: Snowflake[T]] {
  def apply(id: Long): S
  def apply(entity: T): S = apply(entity.getIdLong)
  def unapply(id: Long): Option[S] = Some(apply(id))
  def unapply(id: String): Option[S] = Try(apply(id.toLong)).toOption
}

/**
  * Some Snowflakes are mentionable and their extractors will also accept a Discord mention.
  */
trait MentionableSnowflakeExtractor[T <: ISnowflake with IMentionable, S <: Snowflake[T]] extends SnowflakeExtractor[T, S] {
  protected val mentionPattern: Regex

  override def unapply(idOrMention: String): Option[S] = {
    super.unapply(idOrMention).orElse {
      idOrMention match {
        case mentionPattern(id) => super.unapply(id)
        case _ => None
      }
    }
  }
}

//noinspection NotImplementedCode
case class UserID(id: Long) extends Snowflake[User] with IMentionable {
  override def resolve(jda: JDA): Option[User] = Option(jda.getUserById(id))
  override def getAsMention: String = s"<@$id>"
  def getAsNicknameMention: String = s"<@!$id>"
}

object UserID extends MentionableSnowflakeExtractor[User, UserID] {
  override protected val mentionPattern: Regex = """<@!?(\d+)>""".r
}


//noinspection NotImplementedCode
case class TextChannelID(id: Long) extends Snowflake[TextChannel] with IMentionable {
  override def resolve(jda: JDA): Option[TextChannel] = Option(jda.getTextChannelById(id))
  override def getAsMention: String = s"<#$id>"
}

object TextChannelID extends MentionableSnowflakeExtractor[TextChannel, TextChannelID] {
  override protected val mentionPattern: Regex = """<#(\d+)>""".r
}


case class RoleID(id: Long) extends Snowflake[Role] with IMentionable {
  override def resolve(jda: JDA): Option[Role] = Option(jda.getRoleById(id))
  override def getAsMention: String = s"<@&$id>"
}

object RoleID extends MentionableSnowflakeExtractor[Role, RoleID] {
  override protected val mentionPattern: Regex = """<@&(\d+)>""".r
}


case class EmoteID(id: Long) extends Snowflake[Emote] with IMentionable  {
  override def resolve(jda: JDA): Option[Emote] = Option(jda.getEmoteById(id))
  override def getAsMention: String = s"<@&$id>"
}

object EmoteID extends MentionableSnowflakeExtractor[Emote, EmoteID] {
  override protected val mentionPattern: Regex = """<@&(\d+)>""".r
}


//noinspection NotImplementedCode
case class GuildID(id: Long) extends Snowflake[Guild] {
  override def resolve(jda: JDA): Option[Guild] = Option(jda.getGuildById(id))
}

object GuildID extends SnowflakeExtractor[Guild, GuildID] {}