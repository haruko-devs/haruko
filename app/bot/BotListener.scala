package bot

import java.awt.Color
import java.util.Locale

import net.dv8tion.jda.core.MessageBuilder
import net.dv8tion.jda.core.entities._
import net.dv8tion.jda.core.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.core.hooks.ListenerAdapter
import net.dv8tion.jda.core.requests.restaction.RoleAction

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

/**
  * Receives messages from Discord and then does stuff with them.
  *
  * @param cmdPrefix Haruko will only listen for commands that start with this string.
  * @param colorRolePrefix Prepended to every color name so Haruko knows which roles are colors it can manage.
  * @param pronounRoleNames Whitelist of pronoun roles Haruko will manage.
  * @param guildIDs Haruko will only listen for commands from these servers.
  * @param baseURL Fully qualified URL for Haruko's web interface.
  */
case class BotListener(
  cmdPrefix: String,
  colorRolePrefix: String,
  pronounRoleNames: Set[String],
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

    try {
      message.getRawContent.stripPrefix(cmdPrefix).split(' ') match {
        case Array("help") => reply(channel, user,
          "Available commands: help, source, issues, home, " +
            "color list (also accepts colour), color me, bleach me, " +
            "pronoun list, pronoun me, depronoun me, "
            + SearchEngine.engines.keys.toSeq.sorted.mkString(", "))

        case Array("help", cmd) => cmd match {
          case shortcut if SearchEngine.engines contains shortcut =>
            reply(channel, user, s"Search ${SearchEngine.engines(shortcut).desc}")

          case _ => reply(channel, user, s"The $cmd command isn't documented yet. Please ask an adult.")
        }

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

        case Array("pronoun", "list") => reply(channel, user,
          s"You can pick one or more of: ${pronounRoleNames.mkString(", ")}")
        case Array("pronoun", "me", userPronounRoleNames @ _*) => pronounMe(channel, user, guild, userPronounRoleNames)
        case Array("depronoun", "me", userPronounRoleNames @ _*) => depronounMe(channel, user, guild, userPronounRoleNames)

        case Array(shortcut, queryParts @ _*) if SearchEngine.engines contains shortcut =>
          reply(channel, user, SearchEngine.engines(shortcut).url(queryParts.mkString(" ")))

        case _ =>
          reply(channel, user, Sass.randomResponse)
      }
    } catch {
      case NonFatal(e) =>
        reply(channel, user, "Something went wrong. Please let Mom know.")
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
      addRoles(guild, guild.getMember(user), Seq(colorRole))
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
    removeRoles(guild, member, getColorRoles(member))
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
    * Add one or more pronouns for a user.
    */
  def pronounMe(channel: TextChannel, user: User, guild: Guild, userPronounRoleNames: Seq[String]): Unit = {
    if (userPronounRoleNames.forall(pronounRoleNames.contains)) {
      val userPronounRoles = userPronounRoleNames.map(ensureFlairRole(guild, _))
      addRoles(guild, guild.getMember(user), userPronounRoles)
      reply(channel, user, s"Pronouns granted: ${userPronounRoleNames.mkString(", ")}! Use them with pride!")
    } else {
      reply(channel, user, s"Please pick one or more of the following pronouns: ${pronounRoleNames.mkString(", ")}")
    }
  }

  /**
    * Get a user's pronoun roles.
    */
  def getPronounRoles(member: Member): Seq[Role] = {
    member.getRoles.asScala.filter(role => pronounRoleNames.contains(role.getName))
  }

  /**
    * Remove one or more pronouns from a user.
    */
  def depronounMe(channel: TextChannel, user: User, guild: Guild, userPronounRoleNames: Seq[String]): Unit = {
    if (userPronounRoleNames.forall(pronounRoleNames.contains)) {
      val member = guild.getMember(user)
      val userPronounRoles = getPronounRoles(member).filter(role => userPronounRoleNames.contains(role.getName))
      removeRoles(guild, member, userPronounRoles)
      reply(channel, user, s"Pronouns removed: ${userPronounRoleNames.mkString(", ")}")
    } else {
      reply(channel, user, s"Please pick one or more of the following pronouns: ${pronounRoleNames.mkString(", ")}")
    }
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
    * Create or retrieve a color flair role.
    */
  def ensureColorRole(guild: Guild, colorName: String): Role = {
    ensureFlairRole(guild, s"color-$colorName", Some(CSSColors.named(colorName)))
  }

  /**
    * Create or retrieve a role with no permissions, display order, or mentionability.
    *
    * TODO: JDA uses Java futures, we'd rather use Scala futures.
    * Honestly, this whole thing should be Akka.
    *
    * @note Synchronous.
    */
  def ensureFlairRole(guild: Guild, roleName: String, color: Option[Color] = None): Role = {
    guild.getRoles.asScala.find(_.getName == roleName)
      .getOrElse {
        val roleAction: RoleAction = guild
          .getController
          .createRole()
          .setName(roleName)
          .setHoisted(false)
          .setMentionable(false)
          .setPermissions() // no permissions

        color.foreach(roleAction.setColor)

        roleAction.complete()
      }
  }

  /**
    * Add one or more roles to a user.
    *
    * @note Synchronous.
    */
  def addRoles(guild: Guild, member: Member, roles: Seq[Role]): Unit = {
    guild
      .getController
      .addRolesToMember(member, roles.asJava)
      .complete()
  }

  /**
    * Remove one or more roles from a user.
    *
    * @note Synchronous.
    */
  def removeRoles(guild: Guild, member: Member, roles: Seq[Role]): Unit = {
    guild
      .getController
      .removeRolesFromMember(member, roles.asJava)
      .complete()
  }
}
