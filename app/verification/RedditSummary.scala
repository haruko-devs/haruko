package verification

import java.time.Instant

import play.api.libs.json.JsObject

case class RedditSummary
(
  id: String,
  username: String,
  creationTime: Instant,
  postKarma: Int,
  commentKarma: Int,
  isSuspended: Boolean,
  hasVerifiedEmail: Boolean,
  isOver18: Boolean,
  isGold: Boolean,
  isMod: Boolean,
  isVerified: Boolean,
  isEmployee: Boolean,
  subreddits: Option[Set[SubredditSummary]]
) {
  def totalKarma: Int = postKarma + commentKarma
  def url = s"u/$username"
}

object RedditSummary {
  def apply(stepData: JsObject): RedditSummary = {
    RedditSummary(
      id = (stepData \ "profile" \ "id").as[String],
      username = (stepData \ "profile" \ "name").as[String],
      creationTime = (stepData \ "profile" \ "created_utc").as[Instant],
      postKarma = (stepData \ "profile" \ "link_karma").as[Int],
      commentKarma = (stepData \ "profile" \ "comment_karma").as[Int],
      isSuspended = (stepData \ "profile" \ "is_suspended").as[Boolean],
      hasVerifiedEmail = (stepData \ "profile" \ "has_verified_email").as[Boolean],
      isOver18 = (stepData \ "profile" \ "over_18").as[Boolean],
      isGold = (stepData \ "profile" \ "is_gold").as[Boolean],
      isMod = (stepData \ "profile" \ "is_mod").as[Boolean],
      isVerified = (stepData \ "profile" \ "verified").as[Boolean],
      isEmployee = (stepData \ "profile" \ "is_employee").as[Boolean],
      // Uses intrusive permission, so it's optional in case we ever turn that off.
      subreddits = (stepData \ "subreddits").asOpt[Seq[JsObject]]
        .map { subreddits =>
          subreddits
            .map(SubredditSummary.apply)
            .toSet
        }
    )
  }
}

case class SubredditSummary
(
  id: String,
  name: String,
  title: String,
  url: String,
  creationTime: Instant,
  audienceTargets: Set[String],
  advertiserCategory: Option[String],
  subscribers: Int,
  isOver18Only: Boolean,
  visibility: String,
  isBanned: Boolean,
  isMuted: Boolean,
  isSubscribed: Boolean,
  isFavorite: Boolean,
  isContributor: Boolean,
  isMod: Boolean
)

object SubredditSummary {
  def apply(subData: JsObject): SubredditSummary = {
    SubredditSummary(
      id = (subData \ "id").as[String],
      name = (subData \ "display_name").as[String],
      title = (subData \ "title").as[String],
      url = (subData \ "url").as[String].stripPrefix("/").stripSuffix("/"),
      creationTime = Instant.ofEpochSecond((subData \ "created").as[Long]),
      audienceTargets = (subData \ "audience_target").as[String].split(',').toSet,
      advertiserCategory = (subData \ "advertiser_category").asOpt[String],
      subscribers = (subData \ "subscribers").as[Int],
      isOver18Only = (subData \ "over18").as[Boolean],
      visibility = (subData \ "subreddit_type").as[String],
      isBanned = (subData \ "user_is_banned").as[Boolean],
      isMuted = (subData \ "user_is_muted").as[Boolean],
      isSubscribed = (subData \ "over18").as[Boolean],
      isFavorite = (subData \ "user_has_favorited").as[Boolean],
      isContributor = (subData \ "user_is_contributor").as[Boolean],
      isMod = (subData \ "user_is_moderator").as[Boolean]
    )
  }
}
