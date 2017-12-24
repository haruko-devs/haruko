package bot

import javax.inject.Inject

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.concurrent.Execution.Implicits._
import slick.driver.JdbcProfile
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.Future

/**
  * List of admin-customizable config entries.
  */
class OnlineGuildConfig @Inject()
(
  protected val dbConfigProvider: DatabaseConfigProvider
) extends HasDatabaseConfigProvider[JdbcProfile] {

  import driver.api._

  private class GuildConfigTable(tag: Tag) extends Table[ConfigEntry](tag, "guild_config") {
    def guildID: Rep[String] = column[String]("guild_id")
    def name: Rep[String] = column[String]("name")
    def text: Rep[String] = column[String]("text")

    def pk: PrimaryKey = primaryKey("guild_config_pk", (guildID, name))

    def * : ProvenShape[ConfigEntry] = (guildID, name, text) <> (ConfigEntry.tupled, ConfigEntry.unapply)
  }

  private val GuildConfigQuery = TableQuery[GuildConfigTable]

  def createTable(): Future[Unit] = {
    db.run(GuildConfigQuery.schema.create)
  }

  def upsert(entry: ConfigEntry): Future[Unit] = {
    db
      .run(GuildConfigQuery insertOrUpdate entry)
      .map(_ => ())
  }

  def delete(guildID: String, name: String): Future[Unit] = {
    db
      .run(
        GuildConfigQuery
          .filter(entry => entry.guildID === guildID && entry.name === name)
          .delete
      )
      .map(_ => ())
  }

  def get(guildID: String, name: String): Future[Option[ConfigEntry]] = {
    db
      .run(
        GuildConfigQuery
          .filter(entry => entry.guildID === guildID && entry.name === name)
          .result
      )
      .map(_.headOption)
  }

  def all(guildID: String): Future[Seq[ConfigEntry]] = {
    db
      .run(
        GuildConfigQuery
          .filter(entry => entry.guildID === guildID)
          .result
      )
  }
}

/**
  * Config entry.
  */
case class ConfigEntry
(
  guildID: String,
  name: String,
  text: String
)
