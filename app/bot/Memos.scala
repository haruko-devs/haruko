package bot

import javax.inject.Inject

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.concurrent.Execution.Implicits._
import slick.driver.JdbcProfile
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.Future

/**
  * List of user-customizable memos.
  */
class Memos @Inject() (
  protected val dbConfigProvider: DatabaseConfigProvider
) extends HasDatabaseConfigProvider[JdbcProfile] {

  import driver.api._

  private class MemosTable(tag: Tag) extends Table[Memo](tag, "memo") {
    def guildID: Rep[String] = column[String]("guild_id")
    def name: Rep[String] = column[String]("name")
    def text: Rep[String] = column[String]("text")

    def pk: PrimaryKey = primaryKey("memo_pk", (guildID, name))

    def * : ProvenShape[Memo] = (guildID, name, text) <> (Memo.tupled, Memo.unapply)
  }

  private val MemosQuery = TableQuery[MemosTable]

  def createTable(): Future[Unit] = {
    db.run(MemosQuery.schema.create)
  }

  def upsert(memo: Memo): Future[Unit] = {
    db
      .run(MemosQuery insertOrUpdate memo)
      .map(_ => ())
  }

  def delete(guildID: String, name: String): Future[Unit] = {
    db
      .run(
        MemosQuery
          .filter(memo => memo.guildID === guildID && memo.name === name)
          .delete
      )
      .map(_ => ())
  }

  def get(guildID: String, name: String): Future[Option[Memo]] = {
    db
      .run(
        MemosQuery
          .filter(memo => memo.guildID === guildID && memo.name === name)
          .result
      )
      .map(_.headOption)
  }

  def all(guildID: String): Future[Seq[Memo]] = {
    db
      .run(
        MemosQuery
          .filter(memo => memo.guildID === guildID)
          .result
      )
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
