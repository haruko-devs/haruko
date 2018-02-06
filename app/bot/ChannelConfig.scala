package bot

import javax.inject.Inject
import java.time.{Duration => JDuration}

import scala.collection.mutable

import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.concurrent.Execution.Implicits._

import slick.driver.JdbcProfile
import slick.lifted.{PrimaryKey, ProvenShape}
import scala.concurrent.Future

/**
  * List of per-channel settings.
  */
class ChannelConfigs @Inject()
(
  protected val dbConfigProvider: DatabaseConfigProvider
) extends HasDatabaseConfigProvider[JdbcProfile] {

  import driver.api._

  private class ChannelConfigsTable(tag: Tag) extends Table[ChannelConfig](tag, "channel_config") {
    def guildID: Rep[String] = column[String]("guild_id")
    def channel: Rep[String] = column[String]("channel")
    def name: Rep[String] = column[String]("name")
    def text: Rep[String] = column[String]("text")

    def pk: PrimaryKey = primaryKey("channel_config_pk", (guildID, channel, name))

    def * : ProvenShape[ChannelConfig] = (guildID, channel, name, text) <> (ChannelConfig.tupled, ChannelConfig.unapply)
  }

  private val ChannelConfigsQuery = TableQuery[ChannelConfigsTable]

  def createTable(): Future[Unit] = {
    db.run(ChannelConfigsQuery.schema.create)
  }

  def upsert(channelConfig: ChannelConfig): Future[Unit] = {
    db
      .run(ChannelConfigsQuery insertOrUpdate channelConfig)
      .map(_ => ())
  }

  def delete(guildID: String, channel: String, name: String): Future[Unit] = {
    db
      .run(
        ChannelConfigsQuery
          .filter { channelConfig =>
            channelConfig.guildID === guildID &&
              channelConfig.channel === channel &&
              channelConfig.name === name
          }
          .delete
      )
      .map(_ => ())
  }

  def get(guildID: String, channel: String, name: String): Future[Option[ChannelConfig]] = {
    db
      .run(
        ChannelConfigsQuery
          .filter { channelConfig =>
            channelConfig.guildID === guildID &&
              channelConfig.channel === channel &&
              channelConfig.name === name
          }
          .result
      )
      .map(_.headOption)
  }

  def all(guildID: String): Future[Seq[ChannelConfig]] = {
    db
      .run(
        ChannelConfigsQuery
          .filter { channelConfig =>
            channelConfig.guildID === guildID
          }
          .result
      )
  }

  /**
    * The channel message TTL feature will need to know the configured TTLs to process every message,
    * so we cache those values here to avoid a DB lookup for every message.
    * Map of guilds to channel names to durations.
    */
  private val ttlCache = mutable.Map.empty[String, mutable.Map[String, JDuration]]
    .withDefault(_ => mutable.Map.empty[String, JDuration])

  private val ttlKey = "ttl"

  def getCachedTTL(guildID: String, channel: String): Option[JDuration] = {
    ttlCache
      .get(guildID)
      .flatMap(_.get(channel))
  }

  def getCachedGuildTTLs(guildID: String): Map[String, JDuration] = {
    ttlCache
      .get(guildID)
      .map(_.toMap)
      .getOrElse(Map.empty)
  }

  /**
    * Typed access for TTLs.
    */
  def setTTL(guildID: String, channel: String, duration: JDuration): Future[Unit] = {
    val channelConfig = ChannelConfig(
      guildID = guildID,
      channel = channel,
      name = ttlKey,
      text = duration.toString
    )
    upsert(channelConfig)
      .map { _ =>
        updateTTLCache(channelConfig)
      }
  }

  def clearTTL(guildID: String, channel: String): Future[Unit] = {
    delete(guildID, channel, ttlKey)
      .map { _ =>
        val guildTTLCache = ttlCache(guildID)
        ttlCache(guildID) = guildTTLCache
        guildTTLCache -= channel
      }
  }

  private def updateTTLCache(channelConfig: ChannelConfig): Unit = {
    val guildTTLCache = ttlCache(channelConfig.guildID)
    ttlCache(channelConfig.guildID) = guildTTLCache
    guildTTLCache(channelConfig.channel) = JDuration.parse(channelConfig.text)
  }

  /**
    * Should be called during startup to populate the TTL cache.
    */
  def loadTTLCache(): Future[Unit] = {
    ttlCache.clear()
    db
      .run(
        ChannelConfigsQuery
          .filter(_.name === ttlKey)
          .result
      )
      .map { channelConfigs =>
        for (channelConfig <- channelConfigs) {
          updateTTLCache(channelConfig)
        }
      }
  }
}

/**
  * Named channel config entry.
  *
  * @param channel Channel name, not ID.
  */
case class ChannelConfig
(
  guildID: String,
  channel: String,
  name: String,
  text: String
)
