package bot

import net.dv8tion.jda.core.JDA
import net.dv8tion.jda.core.entities.IMentionable
import net.dv8tion.jda.core.requests.RestAction

import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.matching.Regex

object JDAExtensions {

  implicit class ExtendedRestAction[T](restAction: RestAction[T])(implicit ec: ExecutionContext) {
    def future(shouldQueue: Boolean = true): Future[T] = {
      Future {
        blocking {
          restAction.complete(shouldQueue)
        }
      }
    }
  }

  implicit class ExtendedJDA(jda: JDA) {

    val userMarkup: Regex = """<@!?(\d+)>""".r
    val textChannelMarkup: Regex = """<#(\d+)>""".r
    val roleMarkup: Regex = """<@&(\d+)>""".r
    val emojiMarkup: Regex = """<:\w+:(\d+)>""".r

    def parseMentionable[T <: IMentionable](s: String): T = {
      val mentionable = s match {
        case userMarkup(id) => jda.getUserById(id)
        case textChannelMarkup(id) => jda.getTextChannelById(id)
        case roleMarkup(id) => jda.getRoleById(id)
        case emojiMarkup(id) => jda.getEmoteById(id)
      }
      mentionable.asInstanceOf[T]
    }
  }

  implicit class ExtendedString(s: String) {
    def toTitleCase: String = {
      s.head.toUpper + s.tail
    }
  }
}
