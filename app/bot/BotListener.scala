package bot

import java.awt.Color
import java.util.Locale
import javax.inject.Inject

import net.dv8tion.jda.core.MessageBuilder
import net.dv8tion.jda.core.entities._
import net.dv8tion.jda.core.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.core.hooks.ListenerAdapter
import net.dv8tion.jda.core.requests.restaction.RoleAction
import play.api.Logger

import scala.collection.JavaConverters._
import scala.concurrent.{Await, blocking}
import scala.util.control.NonFatal

/**
  * Receives messages from Discord and then does stuff with them.
  */
case class BotListener @Inject() (
  config: BotConfig,
  memos: Memos
) extends ListenerAdapter {
  val logger = Logger(getClass)

  try {
    Await.result(
      blocking {
        memos.createTable()
      },
      config.dbTimeout
    )
  } catch {
    case NonFatal(e) if e.getMessage.contains("already exists") => // ignore
    case NonFatal(e) => logger.error("Unexpected exception while creating memos table", e)
  }

  override def onGuildMessageReceived(event: GuildMessageReceivedEvent): Unit = {
    if (config.guildIDs.contains(event.getGuild.getId) && event.getMessage.getRawContent.startsWith(config.cmdPrefix)) {
      cmd(event)
    }
  }

  def cmd(event: GuildMessageReceivedEvent): Unit = {
    val message = event.getMessage
    val channel = event.getChannel
    val user = message.getAuthor
    val guild = event.getGuild

    try {
      message.getRawContent.stripPrefix(config.cmdPrefix).split(' ') match {
        case Array("help") => reply(channel, user,
          "Available commands: help, source, issues, home, " +
            "color list (also accepts colour), color me, bleach me, " +
            "pronoun list, pronoun me, depronoun me, " +
            "memo get, memo set, memo clear, memo list, " +
            SearchEngine.engines.keys.toSeq.sorted.mkString(", "))

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
          s"You can manage your profile through my web interface at ${config.baseURL} (NOT IMPLEMENTED YET)") // TODO

        case Array("color", "list") => colorList(channel, user, Locale.US)
        case Array("colour", "list") => colorList(channel, user, Locale.UK)
        case Array("color", "me", colorName) => colorMe(channel, user, guild, colorName, Locale.US)
        case Array("colour", "me", colorName) => colorMe(channel, user, guild, colorName, Locale.UK)
        case Array("bleach", "me") => bleachMe(channel, user, guild)

        case Array("pronoun", "list") => reply(channel, user,
          s"You can pick one or more of: ${config.pronounRoleNames.mkString(", ")}")
        case Array("pronoun", "me", userPronounRoleNames @ _*) => pronounMe(channel, user, guild, userPronounRoleNames)
        case Array("depronoun", "me", userPronounRoleNames @ _*) => depronounMe(channel, user, guild, userPronounRoleNames)

        case Array("memo", "get", name) => memoGet(channel, user, guild, name)
        case Array("memo", "set", name, memoParts @ _*) => memoSet(channel, user, guild, name, memoParts.mkString(" "))
        case Array("memo", "clear", name) => memoClear(channel, user, guild, name)
        case Array("memo", "list") => memoList(channel, user, guild)


        case Array(shortcut, queryParts @ _*) if SearchEngine.engines contains shortcut =>
          reply(channel, user, SearchEngine.engines(shortcut).url(queryParts.mkString(" ")))

        case _ =>
          reply(channel, user, Sass.randomResponse)
      }
    } catch {
      case NonFatal(e) =>
        logger.error(s"Exception: message = $message", e)
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
    member.getRoles.asScala.filter(_.getName.startsWith(config.colorRolePrefix))
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
    if (userPronounRoleNames.forall(config.pronounRoleNames.contains)) {
      val userPronounRoles = userPronounRoleNames.map(ensureFlairRole(guild, _))
      addRoles(guild, guild.getMember(user), userPronounRoles)
      reply(channel, user, s"Pronouns granted: ${userPronounRoleNames.mkString(", ")}! Use them with pride!")
    } else {
      reply(channel, user, s"Please pick one or more of the following pronouns: ${config.pronounRoleNames.mkString(", ")}")
    }
  }

  /**
    * Get a user's pronoun roles.
    */
  def getPronounRoles(member: Member): Seq[Role] = {
    member.getRoles.asScala.filter(role => config.pronounRoleNames.contains(role.getName))
  }

  /**
    * Remove one or more pronouns from a user.
    */
  def depronounMe(channel: TextChannel, user: User, guild: Guild, userPronounRoleNames: Seq[String]): Unit = {
    if (userPronounRoleNames.forall(config.pronounRoleNames.contains)) {
      val member = guild.getMember(user)
      val userPronounRoles = getPronounRoles(member).filter(role => userPronounRoleNames.contains(role.getName))
      removeRoles(guild, member, userPronounRoles)
      reply(channel, user, s"Pronouns removed: ${userPronounRoleNames.mkString(", ")}")
    } else {
      reply(channel, user, s"Please pick one or more of the following pronouns: ${config.pronounRoleNames.mkString(", ")}")
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

  /**
    * Retrieve an existing memo if there's one by that name.
    */
  def memoGet(channel: TextChannel, user: User, guild: Guild, name: String): Unit = {
    Await.result(
      blocking {
        memos.get(guild.getId, name)
      },
      config.dbTimeout
    ) match {
      case Some(memo) => reply(channel, user, memo.text)
      case _ => reply(channel, user, s"I don't know anything about $name. Maybe you can teach me?")
    }
  }

  /**
    * Set or overwrite a memo by name.
    */
  def memoSet(channel: TextChannel, user: User, guild: Guild, name: String, text: String): Unit = {
    Await.result(
      blocking {
        memos.upsert(Memo(
          guildID = guild.getId,
          name = name,
          text = text
        ))
      },
      config.dbTimeout
    )
    reply(channel, user, s"I'll remember what you told me about $name.")
  }

  /**
    * Delete an existing memo if there's one by that name.
    */
  def memoClear(channel: TextChannel, user: User, guild: Guild, name: String): Unit = {
    Await.result(
      blocking {
        memos.delete(guild.getId, name)
      },
      config.dbTimeout
    )
    reply(channel, user, s"I've forgotten everything I ever knew about $name.")
  }

  /**
    * Show the names of all memos that have been set.
    */
  def memoList(channel: TextChannel, user: User, guild: Guild): Unit = {
    val memoLines = Await.result(
      blocking {
        memos.all(guild.getId)
      },
      config.dbTimeout
    )
      .map(_.name)
      .sorted
      .map(name => s"• $name")
    reply(channel, user, s"I've taken memos about: ${memoLines.mkString("\n")}")
  }
}
