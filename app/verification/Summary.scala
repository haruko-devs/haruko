package verification

import java.time.{Clock, Instant}

import play.api.libs.json.JsObject

import bot.GuildConfig

case class Summary
(
  guildID: String,
  verifySessionUUID: String,
  web: WebSummary,
  discord: Option[DiscordSummary],
  reddit: Option[RedditSummary]
)

object Summary {
  def apply(clock: Clock, guildConfig: GuildConfig)(allSteps: JsObject): Summary = {
    Summary(
      guildID = (allSteps \ "guild_id").as[String],
      verifySessionUUID = (allSteps \ "verify_session_uuid").as[String],
      web = WebSummary(mostRecentStepData(allSteps, "000_landing"), mostRecentStepData(allSteps, "010_tos")),
      discord = mostRecentStepData(allSteps, "020_discord").map(DiscordSummary(guildConfig)),
      reddit = mostRecentStepData(allSteps, "030_reddit").map(RedditSummary.apply)
    )
  }

  def mostRecentStepData(allSteps: JsObject, name: String): Option[JsObject] = {
    val matchingSteps = (allSteps \ "steps").as[Seq[JsObject]]
      .filter(elem => (elem \ "step").as[String] == name)
    if (matchingSteps.isEmpty) {
      None
    } else {
      val mostRecentStep = matchingSteps.maxBy(elem => Instant.parse((elem \ "time").as[String]))
      Some((mostRecentStep \ "data").as[JsObject])
    }
  }
}
