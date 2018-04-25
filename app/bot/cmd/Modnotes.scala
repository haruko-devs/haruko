package bot.cmd

import java.sql.Timestamp
import java.time.format.{DateTimeFormatter, FormatStyle, TextStyle}
import java.time.{Clock, Instant}
import javax.inject.Inject

import scala.concurrent.Future
import scala.util.Try
import scala.util.control.NonFatal

import play.api.Logger
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.concurrent.Execution.Implicits._

import net.dv8tion.jda.core.entities.Member
import slick.driver.JdbcProfile
import slick.lifted.{MappedTo, ProvenShape}

import bot.{BotCommand, BotCommandContext, GuildID, UserID}

/**
  * List of user-customizable memos.
  */
class ModnoteCommands @Inject()
(
  modnoteStorage: ModnoteStorage,
  clock: Clock
) extends BotCommand {

  // Canonical words for commands.
  protected val admin = "admin"
  protected val cmd = "modnote"
  protected val add = "add"
  protected val get = "get"
  protected val remove = "remove"
  protected val list = "list"

  override def shortDescs: Seq[String] = Seq(
    s"$admin $cmd",
    s"$admin $cmd $get",
    s"$admin $cmd $add",
    s"$admin $cmd $remove",
    s"$admin $cmd $list"
  )

  def helpCommands(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    "Modnote commands store and retrieve timestamped notes for individual users: " +
      s"`$admin $cmd $get`, `$admin $cmd $add`, `$admin $cmd $remove`, `$admin $cmd $list`"
  )

  def helpGet(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    s"`$admin $cmd $get <@user>`: Get any modnotes for a user."
  )

  def helpAdd(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    s"`$admin $cmd $add <@user> [text]`: Add a modnote to a user, with an automatic timestamp. " +
      "The text can contain formatting."
  )

  def helpRemove(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    s"`$admin $cmd $remove <id>`: Delete a modnote by ID."
  )

  def helpList(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    s"`$admin $cmd $list`: List the names of all users with modnotes."
  )

  def adminCheck(f: BotCommandContext => Future[Unit])(ctx: BotCommandContext): Future[Unit] = {
    if (ctx.isAdmin) {
      f(ctx)
    } else {
      Future.failed(new Exception("Moderators only!"))
    }
  }

  override def accept: PartialFunction[Seq[String], BotCommandContext => Future[Unit]] = {
    val cmdMap: PartialFunction[Seq[String], BotCommandContext => Future[Unit]] = {
      // Backticks indicate a "stable identifier" instead of a pattern binding: https://stackoverflow.com/a/7078077
      case Seq(`admin`, `cmd`, `get`, UserID(user)) => getCmd(user)
      case Seq(`admin`, `cmd`, `get`, _) => _.reply("Couldn't parse user!")
      case Seq(`admin`, `cmd`, `get`, _*) | Seq(`help`, `admin`, `cmd`, `get`) => helpGet

      case Seq(`admin`, `cmd`, `add`, UserID(user), words @ _*) => addCmd(user, words.mkString(" "))
      case Seq(`admin`, `cmd`, `add`, _, _*) => _.reply("Couldn't parse user!")
      case Seq(`admin`, `cmd`, `add`, _*) | Seq(`help`, `admin`, `cmd`, `add`) => helpAdd

      case Seq(`admin`, `cmd`, `remove`, ModnoteID(id)) => removeCmd(id)
      case Seq(`admin`, `cmd`, `remove`, _*) | Seq(`help`, `admin`, `cmd`, `remove`) => helpRemove

      case Seq(`admin`, `cmd`, `list`) => listCmd
      case Seq(`admin`, `cmd`, `list`, _*) | Seq(`help`, `admin`, `cmd`, `list`) => helpList

      case Seq(`admin`, `cmd`, _*) | Seq(`help`, `admin`, `cmd`, _*) => helpCommands
    }
    // Modifies all command functions to apply the adminCheck wrapper for them.
    cmdMap.andThen(adminCheck)
  }

  def displayNames(ctx: BotCommandContext, userID: UserID): String = {
    val member: Option[Member] = Option(ctx.guild.getMemberById(userID.id))
    member.map(m => s"**${m.getEffectiveName}** `${userID.getAsMention}`")
      .getOrElse(s"**__can't resolve user ID__** `${userID.getAsMention}`")
  }

  /**
    * Retrieve any modnotes for a user.
    */
  def getCmd(userID: UserID)(ctx: BotCommandContext): Future[Unit] = {
    modnoteStorage
      .get(ctx.guildID, userID)
      .flatMap {
        case Seq() => ctx.reply(s"No modnotes for ${displayNames(ctx, userID)}.")
        case modnotes =>
          for {
            _ <- ctx.reply(s"There are ${modnotes.length} modnotes for ${displayNames(ctx, userID)} " +
              s"(displayed in ${ctx.timeZone.getDisplayName(TextStyle.FULL_STANDALONE, ctx.locale)})")
            header <- Future.traverse(modnotes) { modnote =>
              val localDateTime = modnote.ts.atZone(ctx.timeZone).toLocalDateTime
              val formatter = DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM)
              ctx.reply(s"• ID `${modnote.id.getOrElse("missing!")}` " +
                s"added on **${formatter.format(localDateTime)}** by ${displayNames(ctx, modnote.adminID)}:\n" +
                s"${modnote.text}")
            }
          } yield ()
      }
  }

  /**
    * Add a new modnote for a user.
    */
  def addCmd(userID: UserID, text: String)(ctx: BotCommandContext): Future[Unit] = {
    modnoteStorage
      .insert(Modnote(
        guildID = ctx.guildID,
        userID = userID,
        adminID = ctx.userID,
        ts = clock.instant(),
        text = text
      ))
      .flatMap { _ =>
        ctx.reply(s"Added modnote for ${displayNames(ctx, userID)}.")
      }
  }

  /**
    * Delete an existing modnote by PK.
    */
  def removeCmd(pk: ModnoteID)(ctx: BotCommandContext): Future[Unit] = {
    modnoteStorage
      .delete(ctx.guildID, pk)
      .flatMap { _ =>
        ctx.reply(s"Deleted modnote with ID $pk.")
      }
  }

  /**
    * List the names of all users with modnotes.
    */
  def listCmd(ctx: BotCommandContext): Future[Unit] = {
    // TODO: rewrite this for paging when we have more modnotes.
    modnoteStorage
      .list(ctx.guildID)
      .flatMap { userIDs =>
        val nameLines = userIDs.map(userID => s"• ${displayNames(ctx, userID)}")
        ctx.reply(s"The following users have modnotes:\n${nameLines.mkString("\n")}")
      }
  }
}

class ModnoteStorage @Inject()
(
  protected val dbConfigProvider: DatabaseConfigProvider
) extends HasDatabaseConfigProvider[JdbcProfile] {

  private val logger = Logger(getClass)

  import driver.api._

  implicit val instantColumnType: BaseColumnType[Instant] = MappedColumnType
    .base[Instant, Timestamp](Timestamp.from, _.toInstant)

  private class ModnotesTable(tag: Tag) extends Table[Modnote](tag, "modnote") {

    def id: Rep[ModnoteID] = column[ModnoteID]("id", O.AutoInc, O.PrimaryKey)
    def idx_pk = index("modnote_idx_id", id, unique = true)

    def guildID: Rep[GuildID] = column[GuildID]("guild_id")
    def userID: Rep[UserID] = column[UserID]("user_id")
    def adminID: Rep[UserID] = column[UserID]("admin_id")
    def ts: Rep[Instant] = column[Instant]("ts")
    def idx_get = index("modnote_idx_get", (guildID, userID, ts))

    def text: Rep[String] = column[String]("text")

    def * : ProvenShape[Modnote] = (id.?, guildID, userID, adminID, ts, text) <> (Modnote.tupled, Modnote.unapply)
  }

  private val query = TableQuery[ModnotesTable]

  private lazy val tableCreated: Future[Unit] = {
    db
      .run(query.schema.create)
      .recover {
        case NonFatal(e) if e.getMessage.contains("already exists") => ()
        case NonFatal(e) => logger.error("Unexpected exception while creating memos table", e)
      }
  }

  def insert(modnote: Modnote): Future[Unit] = {
    for {
      _ <- tableCreated
      _ <- db.run(query += modnote)
    } yield ()
  }

  def delete(guildID: GuildID, id: ModnoteID): Future[Unit] = {
    for {
      _ <- tableCreated
      _ <- db.run(query
        .filter(modnote => modnote.guildID === guildID && modnote.id === id)
        .delete
      )
    } yield ()
  }

  def get(guildID: GuildID, userID: UserID): Future[Seq[Modnote]] = {
    for {
      _ <- tableCreated
      result <- db.run(query
        .filter(modnote => modnote.guildID === guildID && modnote.userID === userID)
        .sortBy(_.ts.asc)
        .result
      )
    } yield result
  }

  def list(guildID: GuildID): Future[Seq[UserID]] = {
    for {
      _ <- tableCreated
      result <- db.run(query
        .filter(memo => memo.guildID === guildID)
        .map(_.userID)
        .result
      )
    } yield result
  }
}

/**
  * Modnote-specific PK type and extractor.
  */
case class ModnoteID(value: Long) extends MappedTo[Long] {
  override def toString: String = value.toString
}

object ModnoteID {
  def unapply(arg: String): Option[ModnoteID] = Try(ModnoteID(arg.toLong)).toOption
}

/**
  * Modnote for a user. A given user may have many modnotes.
  */
case class Modnote
(
  id: Option[ModnoteID] = None,
  guildID: GuildID,
  userID: UserID,
  adminID: UserID,
  ts: Instant,
  text: String
)
