package bot

import javax.inject.Inject

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.concurrent.Execution.Implicits._

import slick.driver.JdbcProfile
import slick.lifted.{PrimaryKey, ProvenShape}
import scala.concurrent.Future
import scala.util.Try
import scala.util.control.NonFatal

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

object OnlineGuildConfig {

  val inviteChannelName: ConfigAccessor[String] = ConfigAccessor(
    name = "invite_channel",
    desc = "Channel that users are invited to by /:guild/invite and /:guild/verify. " +
      "Don't use a leading # when setting this.",
    read = identity
  )

  val inviteURL: ConfigAccessor[String] = ConfigAccessor(
    name = "invite_url",
    desc = "Public invite URL used by the /:guild/invite redirector. " +
      "Clear this to disable the redirector.",
    read = identity
  )

  val inviteAutoUpdate: ConfigAccessor[Boolean] = ConfigAccessor(
    name = "invite_auto_update",
    desc = "Update the public invite URL used by the /:guild/invite redirector when the invite channel is rotated. " +
      "Can be set to `true` or `false`.",
    read = _.toBoolean
  )

  val useTempInvites: ConfigAccessor[Boolean] = ConfigAccessor(
    name = "invite_use_temp",
    desc = "Use temporary invites when Haruko invites someone thru /:guild/verify. " +
      "If a temporary user closes their client before having a role assigned, they can't come back without a new invite. " +
      "Can be set to `true` or `false`.",
    read = _.toBoolean
  )

  val verificationChannelName: ConfigAccessor[String] = ConfigAccessor(
    name = "verification_channel",
    desc = "Channel where Haruko should post verification summaries. Should be readable only by moderators. " +
      "Don't use a leading # when setting this.",
    read = identity
  )

  val adminRoleName: ConfigAccessor[String] = ConfigAccessor(
    name = "admin_role",
    desc = "Name of the role Haruko should grant to admins when they log in thru /:guild/verify. " +
      "Don't use a leading @ when setting this.",
    read = identity
  )

  val adminIDs: ConfigAccessor[Set[String]] = ConfigAccessor(
    name = "admin_ids",
    desc = "List of snowflake IDs that should be automatically elevated to admin when they log in thru /:guild/verify. " +
      "Separate them with spaces or newlines when setting this.",
    read = _.trim.split("""\s+""").toSet
  )

  val changelogChannelName: ConfigAccessor[String] = ConfigAccessor(
    name = "changelog_channel",
    desc = "Channel where Haruko should post nickname changes, etc. Should be readable only by moderators. " +
      "Don't use a leading # when setting this.",
    read = identity
  )

  val all: Seq[ConfigAccessor[_]] = Seq(
    inviteChannelName,
    inviteURL,
    inviteAutoUpdate,
    useTempInvites,
    verificationChannelName,
    adminRoleName,
    adminIDs,
    changelogChannelName
  )
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

case class ConfigAccessor[T]
(
  name: String,
  desc: String,
  read: String => T
)

/**
  * @note All Future-typed accessors can fail if the entry is not present or the format is bad.
  */
case class CombinedGuildConfig
(
  offline: GuildConfig,
  online: OnlineGuildConfig
) {
  // These can't be updated online.
  def id: String = offline.id
  def shortName: String = offline.shortName

  // These are only available from the online config.
  def inviteURL: Future[String] = access(OnlineGuildConfig.inviteURL)
  def inviteAutoUpdate: Future[Boolean] = access(OnlineGuildConfig.inviteAutoUpdate)
  def useTempInvites: Future[Boolean] = access(OnlineGuildConfig.useTempInvites)
  def changelogChannelName: Future[String] = access(OnlineGuildConfig.changelogChannelName)

  // These access the online config first and then fall back to the offline config file.

  def access[T](configAccessor: ConfigAccessor[T]): Future[T] = {
    online.get(id, configAccessor.name).flatMap { entry =>
      Future.fromTry(Try(configAccessor.read(entry.get.text)))
    }
  }

  def inviteChannelName: Future[String] = {
    access(OnlineGuildConfig.inviteChannelName)
      .recoverWith {
        case NonFatal(_) => Future.fromTry(Try(offline.inviteChannelName.get))
      }
  }

  def verificationChannelName: Future[String] = {
    access(OnlineGuildConfig.verificationChannelName)
      .recoverWith {
        case NonFatal(_) => Future.fromTry(Try(offline.verificationChannelName.get))
      }
  }

  def adminRoleName: Future[String] = {
    access(OnlineGuildConfig.adminRoleName)
      .recoverWith {
        case NonFatal(_) => Future.fromTry(Try(offline.adminRoleName.get))
      }
  }

  def adminIDs: Future[Set[String]] = {
    access(OnlineGuildConfig.adminIDs)
      .recoverWith {
        case NonFatal(_) => Future.fromTry(Try(offline.inviteChannelName.get.trim.split("""\s+""").toSet))
      }
  }
}
