package bot

import java.util.Locale

import net.dv8tion.jda.core.MessageBuilder
import net.dv8tion.jda.core.entities._
import net.dv8tion.jda.core.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.core.hooks.ListenerAdapter

import scala.collection.JavaConverters._

/**
  * Receives messages from Discord and then does stuff with them.
  */
case class BotListener(
  cmdPrefix: String,
  colorRolePrefix: String,
  guildIDs: Set[String],
  baseURL: String
) extends ListenerAdapter {
  override def onGuildMessageReceived(event: GuildMessageReceivedEvent): Unit = {
    if (guildIDs.contains(event.getGuild.getId) && event.getMessage.getRawContent.startsWith(cmdPrefix)) {
      cmd(event)
    }
  }

  def cmd(event: GuildMessageReceivedEvent): Unit = {
    val message = event.getMessage
    val channel = event.getChannel
    val user = message.getAuthor
    val guild = event.getGuild

    message.getRawContent.stripPrefix(cmdPrefix).split(' ') match {
      case Array("help") => reply(channel, user,
        "Available commands: help, source, issues, home, color list (also accepts colour), color me, bleach me")

      case Array("source") => reply(channel, user,
        "My source code is available from https://github.com/haruko-devs/haruko")

      case Array("issues") => reply(channel, user,
        "Yeah, I've got issues, so what? You do too, or you wouldn't be here." +
          " But *my* issue tracker is at https://github.com/haruko-devs/haruko/issues for bug reports and feature requests." +
          " Where's *yours*, and what's your excuse?")

      case Array("home") => reply(channel, user,
        s"You can manage your profile through my web interface at $baseURL (NOT IMPLEMENTED YET)") // TODO

      case Array("color", "list") => colorList(channel, user, Locale.US)
      case Array("colour", "list") => colorList(channel, user, Locale.UK)

      case Array("color", "me", colorName) => colorMe(channel, user, guild, colorName, Locale.US)
      case Array("colour", "me", colorName) => colorMe(channel, user, guild, colorName, Locale.UK)

      case Array("bleach", "me") => bleachMe(channel, user, guild)

      case _ =>
        reply(channel, user, Sass.randomResponse)
    }
  }

  def colorWord(locale: Locale): String = locale match {
    case Locale.UK => "colour"
    case _ => "color"
  }

  /**
    * Display a link to the list of legal colors.
    */
  def colorList(channel: TextChannel, user: User, locale: Locale): Unit = {
    reply(channel, user, s"You can pick any named ${colorWord(locale)} from https://drafts.csswg.org/css-color/#named-colors.")
  }

  /**
    * Set a color role for a user. Create it if it didn't exist. Reply to them.
    */
  def colorMe(channel: TextChannel, user: User, guild: Guild, colorName: String, locale: Locale): Unit = {
    if (CSSColors.named contains colorName) {
      // Remove all previous colors.
      // TODO: doesn't work as part of this action, but bleachMe works fine.
      val member = guild.getMember(user)
      bleachCore(member, guild)

      // Set a new color, creating the role if it didn't exist before.
      val colorRole = ensureColorRole(guild, colorName)
      setRole(guild, user, colorRole)
      reply(channel, user, s"You are now $colorName, and you look stunning.")
    } else {
      reply(channel, user, s"Please pick a ${colorWord(locale)} from https://drafts.csswg.org/css-color/#named-colors.")
    }
  }

  /**
    * Get a user's color roles.
    */
  def getColorRoles(member: Member): Seq[Role] = {
    member.getRoles.asScala.filter(_.getName.startsWith(colorRolePrefix))
  }

  /**
    * Remove all color roles from a user.
    */
  def bleachCore(member: Member, guild: Guild): Unit = {
    val colorRoles = getColorRoles(member)
    guild
      .getController
      .removeRolesFromMember(member, colorRoles.asJava)
      .complete()
  }

  /**
    * Bleach a user and then reply to them.
    */
  def bleachMe(channel: TextChannel, user: User, guild: Guild): Unit = {
    val member = guild.getMember(user)
    bleachCore(member, guild)
    reply(channel, user, "You are now colorless. Unless you're British, in which case you're colourless.")
  }

  /**
    * Reply to a user, mentioning them.
    *
    * @note Synchronous.
    */
  def reply(channel: TextChannel, user: User, text: String): Unit = {
    val message = new MessageBuilder()
      .append(user)
      .append(": ")
      .append(text)
      .build()
    channel.sendMessage(message).queue()
  }

  /**
    * TODO: JDA uses Java futures, we'd rather use Scala futures.
    * Honestly, this whole thing should be Akka.
    *
    * @note Synchronous.
    */
  def ensureColorRole(guild: Guild, colorName: String): Role = {
    val colorRoleName = s"color-$colorName"
    guild.getRoles.asScala.find(_.getName == colorRoleName)
      .getOrElse {
        guild
          .getController
          .createRole()
          .setName(colorRoleName)
          .setColor(CSSColors.named(colorName))
          .setHoisted(false)
          .setMentionable(false)
          .setPermissions() // no permissions
          .complete()
      }
  }

  /**
    * Set a role on a user.
    *
    * @note Synchronous.
    */
  def setRole(guild: Guild, user: User, role: Role): Unit = {
    val member = guild.getMember(user)
    guild
      .getController
      .addRolesToMember(member, role)
      .complete()
  }
}
