package bot

/**
  * Guild-specific configuration.
  * @param shortName Used to build URLs related to this guild, instead of the snowflake ID.
  * @param id The guild's snowflake ID.
  * @param inviteChannelName Name of the channel to invite new users to (without leading #).
  * @param verificationChannelName Name of the template channel for verification info.
  *                                Haruko creates a new one for every user being verified.
  *                                Should be readable only by moderators.
  * @param adminIDs Set of IDs which should be given administrator rights on joining the server.
  */
case class GuildConfig(
  shortName: String,
  id: String,
  inviteChannelName: String,
  verificationChannelName: String, // TODO: not implemented as documented yet.
  adminRoleName: String,
  adminIDs: Set[String]
)
