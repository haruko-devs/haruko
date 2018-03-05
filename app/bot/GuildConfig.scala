package bot

/**
  * Guild-specific configuration.
  * @param shortName Used to build URLs related to this guild, instead of the snowflake ID.
  * @param id The guild's snowflake ID.
  * @param inviteChannelName Name of the channel to invite new users to (without leading #).
  * @param verificationChannelName Name of the channel where Haruko should post verification summaries.
  *                                Should be readable only by moderators.
  * @param adminIDs Set of IDs which should be given administrator rights on joining the server.\
  */
@deprecated("Migrating this to CombinedGuildConfig", "since CombinedGuildConfig was introduced")
case class GuildConfig(
  shortName: String,
  id: String,
  inviteChannelName: Option[String] = None,
  verificationChannelName: Option[String] = None,
  adminRoleName: Option[String] = None,
  adminIDs: Set[String] = Set.empty
)
