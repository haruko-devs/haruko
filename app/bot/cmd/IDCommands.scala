package bot.cmd

import java.time.Clock
import javax.inject.Inject

import scala.collection.JavaConverters._
import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import net.dv8tion.jda.core.entities.Member

import bot.{BotCommand, BotCommandContext, UserID}
import ext._

class IDCommands @Inject()
(
  clock: Clock
) extends BotCommand {

  // Canonical words for commands.
  protected val admin = "admin"
  protected val cmd = "id"
  protected val search = "search"
  protected val weird = "weird"

  override def shortDescs: Seq[String] = Seq(
    s"$admin $cmd",
    s"$admin $cmd $search",
    s"$admin $cmd $weird"
  )

  def helpCommands(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    "ID commands look up users by name: " +
      s"`$admin $cmd $search`, `$admin $cmd $weird`"
  )

  def helpSearch(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    s"`$admin $cmd $search [username or nickname]`: Fuzzy search for a user ID by username or nickname. " +
      "Search text may contain spaces."
  )

  def helpWeird(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    s"`$admin $cmd $weird`: Show all users that may be using lookalike characters in their nicknames, " +
      s"and what they may be trying to look like."
  )

  override def accept: PartialFunction[Seq[String], BotCommandContext => Future[Unit]] = {
    val cmdMap: PartialFunction[Seq[String], BotCommandContext => Future[Unit]] = {
      // Backticks indicate a "stable identifier" instead of a pattern binding: https://stackoverflow.com/a/7078077
      case Seq(`admin`, `cmd`, `search`, first, rest @ _*) => searchCmd((first +: rest).mkString(" "))
      case Seq(`admin`, `cmd`, `search`) | Seq(`help`, `admin`, `cmd`, `search`) => helpSearch

      case Seq(`admin`, `cmd`, `weird`) => weirdCmd
      case Seq(`admin`, `cmd`, `weird`, _*) | Seq(`help`, `admin`, `cmd`, `weird`) => helpWeird

      case Seq(`admin`, `cmd`, _*) | Seq(`help`, `admin`, `cmd`) => helpCommands
    }
    // Modifies all command functions to apply the adminCheck wrapper for them.
    cmdMap.andThen(Admin.check)
  }

  /**
    * Fuzzy search for a user ID by username or nickname.
    */
  def searchCmd(search: String)(ctx: BotCommandContext): Future[Unit] = {
    /** Exact matches by nickname or username.
      * Always displayed first if present. */
    val exactMatches: Seq[(Double, Member)] = Seq(
      ctx.guild.getMembersByNickname(search, true).asScala,
      ctx.guild.getMembersByName(search, true).asScala
    )
      .flatten
      .distinct
      .map(Double.PositiveInfinity -> _)

    /** Fuzzy matches by nickname or username,
      * sorted by lowest edit distance,
      * and accounting for Unicode lookalikes. */
    val fuzzyMatches: Seq[(Double, Member)] = {
      val numFuzzySearchResults: Int = 10 - exactMatches.length
      val searchSkeleton = search.skeleton

      /** Higher scores are better matches. Takes better of regular or skeleton matches. */
      def score(name: String): Double = {
        val n = 2 /** Jaccard N-gram size. */
        // Pad with character that should appear nowhere. Might be better to just assign these cases a zero?
        val paddedName = name.padTo(n, '�')
        search.jaccardSimilarity(paddedName, n) max searchSkeleton.jaccardSimilarity(paddedName.skeleton, n)
      }

      /** @return the higher score of this member's username or nickname. */
      def memberScore(member: Member): Double = {
        val usernameScore: Double = score(member.getUser.getName)
        val nicknameScore: Option[Double] = Option(member.getNickname).map(s => score(s))
        nicknameScore
          .map(usernameScore.max)
          .getOrElse(usernameScore)
      }

      ctx.guild.getMembers.asScala
        .map(m => memberScore(m) -> m)
        .sortBy { case (score, _) => -score }
        .filterNot { case (_, m) => exactMatches.map { case (_, e) => e }.contains(m) }
        .take(numFuzzySearchResults)
    }

    val now = clock.instant()
    val text = ("Search results:" +: (exactMatches ++ fuzzyMatches).map { case (score, member) =>
      val displayNames = Admin.displayNames(ctx, UserID(member.getUser))
      val joinDate = Admin.joinDate(ctx, member, now)
      val scoreText = if (score.isInfinity) { "exact" } else { f"$score%.3g"}
      s"• $scoreText: $displayNames (joined $joinDate)"
    }).mkString("\n")

    ctx.reply(text)
  }

  /**
    * Show all users that may be using lookalike characters in their nicknames, and what they may be trying to look like.
    *
    * TODO: this should probably be more conservative
    */
  def weirdCmd(ctx: BotCommandContext): Future[Unit] = {
    val now = clock.instant()

    val lookalikeLines = ctx.guild.getMembers.asScala
      .map(member => member.getEffectiveName.skeleton -> member)
      .filterNot { case (skeleton, member) => skeleton == member.getEffectiveName }
      .sortBy { case (skeleton, _) => skeleton }
      .map { case (skeleton, member) =>
        val displayNames = Admin.displayNames(ctx, UserID(member.getUser))
        val joinDate = Admin.joinDate(ctx, member, now)
        // Note: "`".skeleton is "'" so this won't escape from the backticks.
        s"• $displayNames (joined $joinDate) looks like `$skeleton`"
      }

    // Send messages in order.
    val batchSize = 10
    val header = ctx.reply(s"These users are using potentially lookalike characters in their nicknames:")
    lookalikeLines
      .grouped(batchSize)
      .foldLeft(header) { case (prevTask, batch) =>
        prevTask.flatMap(_ => ctx.reply(batch.mkString("\n")))
      }
  }
}
