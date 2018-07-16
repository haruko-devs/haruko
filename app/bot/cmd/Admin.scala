package bot.cmd

import java.time.{Instant, Duration => JDuration}

import scala.concurrent.Future

import play.api.Logger

import net.dv8tion.jda.core.entities.Member

import bot.{BotCommandContext, UserID}

/**
  * Functions used by several admin commands.
  */
object Admin {

  private val logger = Logger(getClass)

  /**
    * Check if the user is an admin before running a bot commmand.
    */
  def check(f: BotCommandContext => Future[Unit])(ctx: BotCommandContext): Future[Unit] = {
    if (ctx.isAdmin) {
      f(ctx)
    } else {
      logger.warn(s"User ${ctx.userID} tried to run an admin command  on guild ${ctx.guildID} (${ctx.guildConfig.shortName})!")
      ctx.reply("You must be an admin to run that command.")
    }
  }

  /**
    * @return User's nickname, quoted username, and quoted ID as a Discord Markdown string.
    */
  def displayNames(ctx: BotCommandContext, userID: UserID): String = {
    val member: Option[Member] = Option(ctx.guild.getMemberById(userID.id))
    member.map(m => s"**${m.getEffectiveName}** `@${m.getUser.getName}#${m.getUser.getDiscriminator}` `${userID.getAsMention}`")
      .getOrElse(s"**__can't resolve user ID__** `${userID.getAsMention}`")
  }

  /**
    * A user's absolute and relative join dates, in the admin's time zone.
    */
  def joinDate(ctx: BotCommandContext, member: Member, now: Instant): String = {
    val joinDateInAdminTZ = member.getJoinDate.toZonedDateTime
      .withZoneSameInstant(ctx.timeZone)
    val tenure = JDuration.between(member.getJoinDate.toInstant, now)
    s"${tenure.toDays} days ago, on ${joinDateInAdminTZ.toLocalDate}"
  }
}
