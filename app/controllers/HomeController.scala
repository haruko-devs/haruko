package controllers

import java.time.ZoneId
import java.time.format.TextStyle
import java.util.Locale
import javax.inject.Inject

import modules.SecurityModule
import org.pac4j.core.config.Config
import org.pac4j.oauth.profile.OAuth20Profile
import org.pac4j.play.scala.Security
import org.pac4j.play.store.PlaySessionStore
import play.api.mvc._
import play.libs.concurrent.HttpExecutionContext

import scala.collection.JavaConverters._
import scala.collection.{SortedMap, mutable}

/**
  * @param config Used by pac4j Security trait.
  * @param playSessionStore Used by pac4j Security trait.
  * @param ec Used by pac4j Security trait.
  */
class HomeController @Inject() (
  protected override val config: Config,
  protected override val playSessionStore: PlaySessionStore,
  override protected val ec: HttpExecutionContext
) extends Controller with Security[OAuth20Profile] {

  def index: Action[AnyContent] = Action { request =>
    // DEBUG: show environment variables and request headers
    // TODO: limit this to admin users, show a welcome page otherwise.
    val env = mutable.LinkedHashMap.empty[String, String]
//    env ++= SortedMap[String, String](System.getenv().asScala.toSeq: _*)
//    env ++= request.headers.headers
    env("status") = "OK"
    env("authed") = "false"
    Ok(views.html.index(env))
  }

  def indexAuthed: Action[AnyContent] = Secure(SecurityModule.DiscordOAuth2Client) {
    profiles => Action {
      request => {
        // DEBUG: show environment variables, request headers, and profile attributes
        // TODO: limit this to admin users, show a welcome page otherwise.
        val env = mutable.LinkedHashMap.empty[String, String]
//        env ++= SortedMap[String, String](System.getenv().asScala.toSeq: _*)
//        env ++= request.headers.headers
//        profiles.foreach { profile =>
//          env ++= SortedMap[String, String](profile.getAttributes.asScala
//            .mapValues(_.toString)
//            .toSeq: _*)
//        }
        env("status") = "OK"
        env("authed") = "true"
        Ok(views.html.index(env))
      }
    }
  }

  def timezones: Action[AnyContent] = Action { request =>
    val sortedZoneIDs = ZoneId.getAvailableZoneIds.asScala.toIndexedSeq
      // Drop obviously legacy zone IDs in the default Java TZDB.
      .filter(_.contains("/"))
      .filterNot(id => Seq("Etc/", "SystemV/", "US/").exists(prefix => id.startsWith(prefix)))
      // Drop zone IDs that are alternate names for offset-based zones.
      .filter { id =>
        val zoneid = ZoneId.of(id)
        val normalized = zoneid.normalized()
        zoneid == normalized
      }
      .sortBy { id =>
        id.split("/") match {
          case Array(continent, region, sub) => (continent, region, sub)
          case Array(continent, region) => (continent, region, "")
          case Array(continent) => (continent, "", "")
          case _ => ("", "", "")
        }
      }

    val zoneDescs = mutable.LinkedHashMap.empty[String, String]
    sortedZoneIDs.foreach { id =>
      val zoneid = ZoneId.of(id)
      zoneDescs(id) = zoneid.getDisplayName(TextStyle.FULL, Locale.US)
    }
    Ok(views.html.timezones(zoneDescs))
  }
}
