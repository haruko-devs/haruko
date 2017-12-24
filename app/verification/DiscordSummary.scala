package verification

import java.time.Instant

import scala.collection.JavaConverters._

import play.api.libs.json.JsObject

import net.dv8tion.jda.core.Permission
import net.dv8tion.jda.core.utils.MiscUtil

import bot.GuildConfig

case class DiscordSummary
(
  id: String,
  username: String,
  discriminator: String,
  creationTime: Instant,
  isVerified: Boolean,
  isMFAEnabled: Boolean,
  otherGuilds: Option[Set[DiscordGuildSummary]]
) {
  def fullUsername: String = s"$username#$discriminator"
}

object DiscordSummary {
  def apply(guildConfig: GuildConfig)(stepData: JsObject): DiscordSummary = {
    DiscordSummary(
      id = (stepData \ "profile" \ "id").as[String],
      username = (stepData \ "profile" \ "username").as[String],
      discriminator = (stepData \ "profile" \ "discriminator").as[String],
      creationTime = (stepData \ "profile" \ "creation_time").as[Instant],
      isVerified = (stepData \ "profile" \ "verified").as[Boolean],
      isMFAEnabled = (stepData \ "profile" \ "mfa_enabled").as[Boolean],
      // Uses intrusive permission, so it's optional in case we ever turn that off.
      otherGuilds = (stepData \ "guilds").asOpt[Seq[JsObject]]
        .map { guilds =>
          guilds
            .map(DiscordGuildSummary.apply)
            .filter(_.id != guildConfig.id)
            .toSet
        }
    )
  }
}

case class DiscordGuildSummary
(
  id: String,
  name: String,
  creationTime: Instant,
  isOwner: Boolean,
  isAdmin: Boolean
)

object DiscordGuildSummary {
  def apply(guildData: JsObject): DiscordGuildSummary = {
    DiscordGuildSummary(
      id = (guildData \ "id").as[String],
      name = (guildData \ "name").as[String],
      creationTime = MiscUtil.getCreationTime(java.lang.Long.parseUnsignedLong((guildData \ "id").as[String])).toInstant,
      isOwner = (guildData \ "owner").as[Boolean],
      isAdmin = Permission.getPermissions((guildData \ "permissions").as[Long]).asScala.contains(Permission.ADMINISTRATOR)
    )
  }
}
