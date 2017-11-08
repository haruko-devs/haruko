package bot

import java.awt.Color
import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.temporal.{ChronoUnit, TemporalAmount}
import java.time.{Clock, Duration, ZoneId}
import java.util.{Locale, UUID}
import javax.inject.Inject

import net.dv8tion.jda.core.{MessageBuilder, Permission}
import net.dv8tion.jda.core.entities._
import net.dv8tion.jda.core.events.ReadyEvent
import net.dv8tion.jda.core.events.message.guild.GuildMessageReceivedEvent
import net.dv8tion.jda.core.hooks.ListenerAdapter
import net.dv8tion.jda.core.requests.restaction.RoleAction
import play.api.Logger

import scala.collection.JavaConverters._
import scala.concurrent.{Await, Future, Promise, blocking}
import scala.util.control.NonFatal
import JDAExtensions._
import net.dv8tion.jda.core.exceptions.PermissionException

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
  * Receives messages from Discord and then does stuff with them.
  */
case class BotListener @Inject() (
  config: BotConfig,
  memos: Memos,
  searcher: Searcher,
  clock: Clock
) extends ListenerAdapter {
  val logger = Logger(getClass)

  // Currently used for search engines only.
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

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

  // Completed when bot is initialized.
  val readyPromise: Promise[Unit] = Promise()

  override def onReady(event: ReadyEvent): Unit = {
    readyPromise.success(())
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
        case Array("") => reply(channel, user, "Hello! Use the command help to find out more about what I do")
        case Array("help") => reply(channel, user,
          "Available commands: help, source, issues, home, " +
            "color list (also accepts colour), color me, bleach me, " +
            "pronoun list, pronoun me, depronoun me, " +
            "timezone list (also accepts tz), timezone me, detimezone me, " +
            "timefor @user, " +
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

        case Array("tz", "list") => timezoneList(channel, user)
        case Array("tz", "me", id) => timezoneMe(channel, user, guild, id)
        case Array("detz", "me") => detimezoneMe(channel, user, guild)
        case Array("timezone", "list") => timezoneList(channel, user)
        case Array("timezone", "me", id) => timezoneMe(channel, user, guild, id)
        case Array("detimezone", "me") => detimezoneMe(channel, user, guild)

        case Array("timefor", mention) => timeFor(channel, user, guild, mention)

        case Array("pronoun", "list") => reply(channel, user,
          s"You can pick one or more of: ${config.pronounRoleNames.mkString(", ")}")
        case Array("pronoun", "me", userPronounRoleNames @ _*) => pronounMe(channel, user, guild, userPronounRoleNames)
        case Array("depronoun", "me", userPronounRoleNames @ _*) => depronounMe(channel, user, guild, userPronounRoleNames)

        case Array("memo", "get", name) => memoGet(channel, user, guild, name)
        case Array("memo", "set", name, memoParts @ _*) => memoSet(channel, user, guild, name, memoParts.mkString(" "))
        case Array("memo", "clear", name) => memoClear(channel, user, guild, name)
        case Array("memo", "list") => memoList(channel, user, guild)

        case Array(shortcut, queryParts @ _*) if SearchEngine.engines contains shortcut =>
          searcher
            .search(shortcut, queryParts.mkString(" "))
            .map(resultURL => reply(channel, user, resultURL.getOrElse("No results found.")))
            .onFailure {
              case NonFatal(e) =>
                logger.error(s"Search exception: message = $message", e)
                reply(channel, user, "Something went wrong. Please let Mom know.")
            }

        // Admin commands. We don't even respond to these unless the user has the Administrator permission on this guild.
        // TODO: document admin commands.
        case Array("admin", adminCmdParts @ _*) if message.getMember.hasPermission(Permission.ADMINISTRATOR) => adminCmdParts match {
          case Seq("archive", channelIDMarkup) =>
            val channelToArchive = guild.getJDA.parseMentionable[TextChannel](channelIDMarkup)
            adminArchive(channelToArchive)

          case Seq("sleepers", "--kick", duration) =>
            adminSleepers(
              channel, user, guild,
              window = Duration.parse(duration),
              kick = true
            )
          case Seq("sleepers", "--kick") =>
            adminSleepers(
              channel, user, guild,
              kick = true
            )
          case Seq("sleepers", duration) =>
            adminSleepers(
              channel, user, guild,
              window = Duration.parse(duration)
            )
          case Seq("sleepers") =>
            adminSleepers(
              channel, user, guild
            )
        }

        case _ =>
          reply(channel, user, Sass.randomResponse)
      }
    } catch {
      case NonFatal(e) =>
        logger.error(s"Exception: message = $message", e)
        reply(channel, user, "Something went wrong. Please let Mom know.")
    }
  }

  def logResult(future: Future[_], reason: String, commandType: String = "User action"): Unit = {
    future.onComplete {
      case Success(_) => logger.info(s"$commandType successful: $reason")
      case Failure(t) => logger.error(s"$commandType failed: $reason", t)
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
    * Display a link to the list of supported time zones.
    */
  def timezoneList(channel: TextChannel, user: User): Unit = {
    reply(channel, user, s"You can pick any named time zone from ${config.baseURL}/timezones, `UTC`, or an offset from UTC like `UTC-0130`.")
  }

  /**
    * Get a user's timezone roles.
    */
  def getTimezoneRoles(member: Member): Seq[Role] = {
    member.getRoles.asScala.filter(_.getName.startsWith(config.timezoneRolePrefix))
  }

  /**
    * Add a timezone to a user.
    */
  def timezoneMe(channel: TextChannel, user: User, guild: Guild, id: String): Unit = {
    val member = guild.getMember(user)
    val memberTzRoles = getTimezoneRoles(member)

    val uuid = UUID.randomUUID()
    val reason = s"$uuid: timezoneMe($id) for ${member.getEffectiveName}"

    val allTasks: Future[Unit] = Try(ZoneId.of(id)) match {
      case Failure(_) =>
        replyAsync(channel, user, s"Please use a named time zone from ${config.baseURL}/timezones, `UTC`, or an offset from UTC like `UTC-0130`.")

      case Success(zoneid) =>
        val normalized = zoneid.normalized()

        // TODO: extract this into an ensureOnlySpecifiedRolesWithPrefixAsync()
        val tzRoleName = s"${config.timezoneRolePrefix}${normalized.getId}"

        val otherTzRoles = memberTzRoles.filterNot(_.getName == tzRoleName)
        val removeOtherTzRolesFromMember: Future[Unit] = removeRolesAsync(guild, member, otherTzRoles, reason)

        val memberAlreadyHasRole = memberTzRoles.exists(_.getName == tzRoleName)
        val addTzRoleToMember: Future[Unit] = if (memberAlreadyHasRole) {
          Future.successful(())
        } else {
          val getOrCreateTzRole = ensureFlairRoleAsync(guild, tzRoleName, reason)

          getOrCreateTzRole.flatMap { role =>
            addRolesAsync(guild, member, roles = Seq(role), reason)
          }
        }

        removeOtherTzRolesFromMember
          .zip(addTzRoleToMember)
          .flatMap { _ =>
            if (zoneid == normalized) {
              replyAsync(channel, user, s"Your time zone is now `${normalized.getId}`.")
            } else {
              replyAsync(channel, user, s"Your time zone is now `${normalized.getId}` (normalized from `${zoneid.getId}`).")
            }
          }
    }
    logResult(allTasks, reason)
  }

  /**
    * Remove any timezones from a user.
    */
  def detimezoneMe(channel: TextChannel, user: User, guild: Guild): Unit = {
    val member = guild.getMember(user)
    val memberTzRoles = getTimezoneRoles(member)

    val uuid = UUID.randomUUID()
    val reason = s"$uuid: detimezoneMe() for ${member.getEffectiveName}"

    val allTasks = if (memberTzRoles.nonEmpty) {
      removeRolesAsync(guild, member, memberTzRoles, reason)
        .flatMap(_ => replyAsync(channel, user, "Time zone removed."))
    } else {
      replyAsync(channel, user, "You don't have a time zone set.")
    }
    logResult(allTasks, reason)
  }

  /**
    * Show the local time for a user.
    */
  def timeFor(channel: TextChannel, user: User, guild: Guild, mention: String): Unit = {
    val member = guild.getMember(user)
    val uuid = UUID.randomUUID()
    val reason = s"$uuid: timefor($mention) for ${member.getEffectiveName}"

    // TODO: factor this out into parseUserMention(mention)
    val allTasks = Try(guild.getJDA.parseMentionable[User](mention)) match {
      case Failure(_) =>
        replyAsync(channel, user, "I don't know who that is.")

      case Success(targetUser) =>
        Option(guild.getMember(targetUser)) match {
          case None =>
            replyAsync(channel, user, s"They're not on this server.")

          case Some(targetMember) =>
            val targetPronoun = getAnyPronouns(targetMember)
            val targetNickname = targetMember.getEffectiveName
            getTimezoneRoles(targetMember).headOption match {
              case None =>
                replyAsync(channel, user, s"${targetPronoun.subject.toTitleCase} ${targetPronoun.contractionPossessesNegated} set ${targetPronoun.determiner} time zone.")

              case Some(tzRole) =>
                val zoneid = ZoneId.of(tzRole.getName.stripPrefix(config.timezoneRolePrefix))
                val zonedTime = clock.instant().atZone(zoneid).toLocalTime
                val formatter = DateTimeFormatter.ofLocalizedTime(FormatStyle.MEDIUM)
                replyAsync(channel, user, s"It's ${formatter.format(zonedTime)} where $targetNickname is.")
            }
        }

    }
    logResult(allTasks, reason)
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
    * Get one of a user's acceptable pronouns, or a fallback if they haven't configured any.
    */
  def getAnyPronouns(member: Member): Pronoun = {
    getPronounRoles(member)
      .headOption
      .map(_.getName)
      .map(Pronoun.lookup)
      .getOrElse(Pronoun.fallback)
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
    * Reply to a user, mentioning them.
    */
  def replyAsync(channel: TextChannel, user: User, text: String): Future[Unit] = {
    val message = new MessageBuilder()
      .append(user)
      .append(": ")
      .append(text)
      .build()
    channel.sendMessage(message)
      .future()
      .map(_ => ())
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
    * Create or retrieve a role with no permissions, display order, or mentionability.
    */
  def ensureFlairRoleAsync(guild: Guild, roleName: String, reason: String, color: Option[Color] = None): Future[Role] = {
    guild.getRoles.asScala.find(_.getName == roleName)
      .map(Future.successful)
      .getOrElse {
        val roleAction: RoleAction = guild
          .getController
          .createRole()
          .setName(roleName)
          .setHoisted(false)
          .setMentionable(false)
          .setPermissions() // no permissions

        color.foreach(roleAction.setColor)

        roleAction
          .reason(reason)
          .future()
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
    * Add one or more roles to a user. No-op if list is empty.
    */
  def addRolesAsync(guild: Guild, member: Member, roles: Seq[Role], reason: String): Future[Unit] = {
    if (roles.nonEmpty) {
      guild
        .getController
        .addRolesToMember(member, roles.asJava)
        .reason(reason)
        .future()
        .map(_ => ())
    } else {
      Future.successful(())
    }
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
    * Remove one or more roles from a user. No-op if list is empty.
    */
  def removeRolesAsync(guild: Guild, member: Member, roles: Seq[Role], reason: String): Future[Unit] = {
    if (roles.nonEmpty) {
      guild
        .getController
        .removeRolesFromMember(member, roles.asJava)
        .reason(reason)
        .future()
        .map(_ => ())
    } else {
      Future.successful(())
    }
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
    reply(channel, user, s"I've taken memos about:\n${memoLines.mkString("\n")}")
  }

  def adminArchive(oldChannel: TextChannel): Unit = {
    // Unique ID for this operation.
    val uuid = UUID.randomUUID()

    val channelName = oldChannel.getName
    val stagingChannelName = s"$channelName-staging-$uuid"
    val archivedChannelName = s"$channelName-archived-${System.currentTimeMillis() / 1000}"

    // Contains identifier for grouping multiple actions in audit logs.
    val reason = s"$uuid: Haruko archived #$channelName to #$archivedChannelName"

    val guild = oldChannel.getGuild

    // Copy the current channel to create the new one.
    val copyChannel = guild.getController
      .createCopyOfChannel(oldChannel)
      .setName(stagingChannelName)
      .future()

    // Rename old channel.
    val renameOldChannel = oldChannel.getManager
      .setName(archivedChannelName)
      .reason(reason)
      .future()

    // Replace it with the new channel.
    val renameAndRespositionNewChannel = copyChannel
      .zip(renameOldChannel)
      .flatMap { case (newChannel, _) =>

        // Change the name of the new channel.
        val renameNewChannel = newChannel.getManager
          .setName(channelName)
          .reason(reason)
          .future()

        // Change the position of the new channel.
        val repositionNewChannel = guild.getController
          .modifyTextChannelPositions()
          .selectPosition(oldChannel)
          .swapPosition(newChannel.asInstanceOf[TextChannel])
          .future()

        renameNewChannel.zip(repositionNewChannel)
      }

    // Hide the old channel from the @everyone role.
    val everyone = guild.getPublicRole
    val finalPermissions = Permission.MESSAGE_READ
    val hideFromEveryone = Option(oldChannel.getPermissionOverride(everyone)) match {

      case Some(permissionOverride) =>
        permissionOverride.getManager
          .deny(finalPermissions)
          .reason(reason)
          .future()

      case None =>
        oldChannel
          .createPermissionOverride(everyone)
          .setDeny(finalPermissions)
          .reason(reason)
          .future()
    }

    // Delete any permission overrides on it that aren't for @everyone.
    val otherOverrides = oldChannel.getPermissionOverrides.asScala
      .filter(_.getRole != everyone)
    val deleteOtherOverrides = Future.traverse(otherOverrides) { permissionOverride =>
      permissionOverride
        .delete()
        .reason(reason)
        .future()
    }

    // Revoke invites to the old channel.
    val revokeInvites = oldChannel.getInvites.future().flatMap { invites =>
      Future.traverse(invites.asScala) { invite =>
        invite
          .delete()
          .reason(reason)
          .future()
      }
    }

    val allTasks = hideFromEveryone
      .zip(deleteOtherOverrides)
      .zip(renameAndRespositionNewChannel)
      .zip(revokeInvites)

    logResult(allTasks, reason, commandType = "Admin action")
  }

  /**
    * List users who haven't spoken in text channels for a given window.
    * Optionally kick them as well.
    */
  def adminSleepers(
    replyChannel: TextChannel,
    replyUser: User,
    guild: Guild,
    window: TemporalAmount = Duration.of(30, ChronoUnit.DAYS),
    kick: Boolean = false
  ): Unit = {
    // Unique ID for this operation.
    val uuid = UUID.randomUUID()

    // Contains identifier for grouping multiple actions in audit logs.
    val actionDesc = if (kick) "kicked" else "listed"
    val reason = s"$uuid: Haruko $actionDesc users inactive for more than $window"

    val allTasks = Seq.newBuilder[Future[Unit]]

    // Look back this far.
    val cutoff = clock.instant().minus(window)

    // Get all members except for admins and bots.
    val members: mutable.Set[Member] = mutable.Set()
    members ++= guild.getMembers.asScala.view
      .filterNot(_.hasPermission(Permission.ADMINISTRATOR))
      .filterNot(_.getUser.isBot)

    // Scan each channel until the cutoff. Remove users that have sent messages.
    for (channel <- guild.getTextChannels.asScala) {
      try {
        for (message <- channel.getIterableHistory.asScala
          .view.takeWhile(_.getCreationTime.toInstant.isAfter(cutoff))) {
          members -= message.getMember
        }
      } catch {
        case p: PermissionException =>
          allTasks += replyAsync(replyChannel, replyUser,
            s"Couldn't scan ${channel.getAsMention}: need `${p.getPermission}`")
      }
    }

    // Skip users that have joined the server after the start of the window.
    val sleepers = members.filter(_.getJoinDate.toInstant.isBefore(cutoff))

    // Automatically kick inactive users.
    if (kick) {
      allTasks ++= sleepers.map { member =>
        guild
          .getController
          .kick(member, reason)
          .future()
          .map(_ => ())
          .recoverWith {
            case e: PermissionException =>
              val errorMessage = s"Haruko doesn't have the permissions to kick **${member.getEffectiveName}** `${member.getUser.getAsMention}`"
              logger.warn(errorMessage, e)
              replyAsync(replyChannel, replyUser, errorMessage)
            case NonFatal(e) =>
              val errorMessage = s"Haruko failed to kick **${member.getEffectiveName}** `${member.getUser.getAsMention}`"
              logger.warn(errorMessage, e)
              replyAsync(replyChannel, replyUser, errorMessage)
          }
      }
    }

    // Build a formatted list.
    val batchSize = 10
    val sleeperMessages: Iterator[String] = sleepers
      .toArray
      .sortBy(_.getJoinDate)
      .map { member =>
        val roles = member.getRoles.asScala.sortBy(_.getPosition)
          .map(role => s"`${role.getName}`")
          .mkString(", ")
        s"• **${member.getEffectiveName}** `${member.getUser.getAsMention}`, joined ${member.getJoinDate.toLocalDate}, roles: $roles"
      }
      .grouped(batchSize)
      .map(_.mkString("\n"))

    // Send messages in order.
    val headerMessageTask = replyAsync(replyChannel, replyUser,
      s"These users have been inactive for at least $window:")
    allTasks += sleeperMessages.foldLeft(headerMessageTask) { case (prevTask, message) =>
      prevTask.flatMap { _ =>
        replyAsync(replyChannel, replyUser, message)
      }
    }

    logResult(Future.sequence(allTasks.result()), reason, commandType = "Admin action")
  }
}
