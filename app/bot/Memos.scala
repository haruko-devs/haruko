package bot

import javax.inject.Inject

import scala.concurrent.Future
import scala.util.control.NonFatal

import play.api.Logger
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.concurrent.Execution.Implicits._

import slick.driver.JdbcProfile
import slick.lifted.{PrimaryKey, ProvenShape}

/**
  * List of user-customizable memos.
  */
class MemoCommands @Inject()
(
  memoStorage: MemoStorage
) extends BotCommand {

  override def shortDescs: Seq[String] = Seq(
    "memo",
    "memo get",
    "memo set",
    "memo clear",
    "memo list"
  )

  def help(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    "Memo commands store and retrieve named notes for the entire server: " +
      "`memo get`, `memo set`, `memo clear`, `memo list`"
  )

  def helpGet(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    "`memo get <name>`: Get a memo by name, if there's a memo with that name."
  )

  def helpSet(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    "`memo set <name> [text]`: Create a named memo or overwrite the previous one. " +
      "The name must be one word with no spaces, but the text can contain formatting."
  )

  def helpClear(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    "`memo clear <name>`: Delete a memo, if there's a memo with that name."
  )

  def helpList(ctx: BotCommandContext): Future[Unit] = ctx.reply(
    "`memo list`: List the names of all memos that exist."
  )

  override def accept: PartialFunction[Seq[String], BotCommandContext => Future[Unit]] = {
    case Seq("memo", "get", name) => get(name)
    case Seq("memo", "get", _*) | Seq("help", "memo", "get") => helpGet

    case Seq("memo", "set", name, memoWords @ _*) => set(name, memoWords.mkString(" "))
    case Seq("memo", "set", _*) | Seq("help", "memo", "set") => helpSet

    case Seq("memo", "clear", name) => clear(name)
    case Seq("memo", "clear", _*) | Seq("help", "memo", "clear") => helpClear

    case Seq("memo", "list") => list
    case Seq("memo", "list", _*) | Seq("help", "memo", "list") => helpList

    case Seq("memo", _*) | Seq("help", "memo", _*) => help
  }

  /**
    * Retrieve an existing memo if there's one by that name.
    */
  def get(name: String)(ctx: BotCommandContext): Future[Unit] = {
    memoStorage
      .get(ctx.guildID, name)
      .flatMap {
        case Some(memo) => ctx.reply(memo.text)
        case _ => ctx.reply(s"I don't know anything about $name. Maybe you can teach me?")
      }
  }

  /**
    * Set or overwrite a memo by name.
    */
  def set(name: String, text: String)(ctx: BotCommandContext): Future[Unit] = {
    memoStorage
      .upsert(Memo(
        guildID = ctx.guildID,
        name = name,
        text = text
      ))
      .flatMap { _ =>
        ctx.reply(s"I'll remember what you told me about $name.")
      }
  }

  /**
    * Delete an existing memo if there's one by that name.
    */
  def clear(name: String)(ctx: BotCommandContext): Future[Unit] = {
    memoStorage
      .delete(ctx.guildID, name)
      .flatMap { _ =>
        ctx.reply(s"I've forgotten everything I ever knew about $name.")
      }
  }

  /**
    * Show the names of all memos that have been set.
    */
  def list(ctx: BotCommandContext): Future[Unit] = {
    memoStorage
      .all(ctx.guildID)
      .flatMap { allMemos =>
        val memoLines = allMemos
          .map(_.name)
          .sorted
          .map(name => s"• $name")
        ctx.reply(s"I've taken memos about:\n${memoLines.mkString("\n")}")
      }
  }
}

class MemoStorage @Inject()(
  protected val dbConfigProvider: DatabaseConfigProvider
) extends HasDatabaseConfigProvider[JdbcProfile] {

  private val logger = Logger(getClass)

  import driver.api._

  private class MemosTable(tag: Tag) extends Table[Memo](tag, "memo") {
    def guildID: Rep[String] = column[String]("guild_id")
    def name: Rep[String] = column[String]("name")
    def text: Rep[String] = column[String]("text")

    def pk: PrimaryKey = primaryKey("memo_pk", (guildID, name))

    def * : ProvenShape[Memo] = (guildID, name, text) <> (Memo.tupled, Memo.unapply)
  }

  private val query = TableQuery[MemosTable]

  private lazy val tableCreated: Future[Unit] = {
    db
      .run(query.schema.create)
      .recover {
        case NonFatal(e) if e.getMessage.contains("already exists") => ()
        case NonFatal(e) => logger.error("Unexpected exception while creating memos table", e)
      }
  }

  def upsert(memo: Memo): Future[Unit] = {
    for {
      _ <- tableCreated
      _ <- db.run(query insertOrUpdate memo)
    } yield ()
  }

  def delete(guildID: String, name: String): Future[Unit] = {
    for {
      _ <- tableCreated
      _ <- db.run(query
        .filter(memo => memo.guildID === guildID && memo.name === name)
        .delete
      )
    } yield ()
  }

  def get(guildID: String, name: String): Future[Option[Memo]] = {
    for {
      _ <- tableCreated
      result <- db.run(query
        .filter(memo => memo.guildID === guildID && memo.name === name)
        .result
      )
    } yield result.headOption
  }

  def all(guildID: String): Future[Seq[Memo]] = {
    for {
      _ <- tableCreated
      result <- db.run(query
        .filter(memo => memo.guildID === guildID)
        .result
      )
    } yield result
  }
}

/**
  * Named memo.
  */
case class Memo(
  guildID: String,
  name: String,
  text: String
)
