package bot

import java.time.{Duration => JDuration}
import java.util.function.Consumer

import scala.concurrent.duration._

import net.dv8tion.jda.core.JDA
import net.dv8tion.jda.core.entities.IMentionable
import net.dv8tion.jda.core.requests.RestAction
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.matching.Regex

object JDAExtensions {

  implicit class ExtendedRestAction[T](restAction: RestAction[T])(implicit ec: ExecutionContext) {
    //noinspection ConvertExpressionToSAM
    def future(): Future[T] = {
      val promise = Promise[T]()
      restAction.queue(
        new Consumer[T] {
          override def accept(value: T): Unit = promise.success(value)
        },
        new Consumer[Throwable] {
          override def accept(cause: Throwable): Unit = promise.failure(cause)
        }
      )
      promise.future
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

  implicit class ExtendedJDuration(jduration: JDuration) {
    def asScala: FiniteDuration = {
      jduration.getSeconds.seconds + jduration.getNano.nanos
    }
  }
}
