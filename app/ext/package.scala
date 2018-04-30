import java.time.{Duration => JDuration}
import java.util.function.Consumer

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.matching.Regex

import play.api.Logger

import com.github.halfmatthalfcat.stringmetric.similarity._
import com.github.mpkorstanje.unicode.tr39confusables.Skeleton
import net.dv8tion.jda.core.JDA
import net.dv8tion.jda.core.entities.IMentionable
import net.dv8tion.jda.core.requests.RestAction

package object ext {
  /**
    * Get a Scala [[Future]] for a [[RestAction]].
    *
    * @note This version is specialized for [[Void]] return types, and maps them to [[Unit]].
    */
  implicit class ExtendedRestActionVoid(restAction: RestAction[Void]) {
    //noinspection ConvertExpressionToSAM
    def future(): Future[Unit] = {
      val promise = Promise[Unit]()
      restAction.queue(
        new Consumer[Void] {
          override def accept(value: Void): Unit = promise.success(())
        },
        new Consumer[Throwable] {
          override def accept(cause: Throwable): Unit = promise.failure(cause)
        }
      )
      promise.future
    }
  }

  /**
    * Get a Scala [[Future]] for a [[RestAction]].
    */
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

  @deprecated("Use a Snowflake extractor", "2018-04-24")
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

    /**
      * @return The "skeleton transform" for this string.
      *         Can be compared with skeletons of other strings which might contain Unicode lookalike characters.
      *
      * @see [[https://www.unicode.org/reports/tr39 Unicode TR 39: Confusable Detection]].
      */
    def skeleton: String = Skeleton.skeleton(s)

    /**
      * Levenshtein edit distance between this string and another string.
      *
      * @note Neither string can be empty.
      * @note Lower scores are closer strings.
      */
    def levenshteinDistance(t: String): Int = {
      require(s.nonEmpty)
      require(t.nonEmpty)
      LevenshteinMetric.compare(s, t).get
    }

    /**
      * Levenshtein distance transformed into the same [0, 1] range as Jaccard.
      *
      * @note Ranges from 0 (no match) to 1 (perfect match).
      */
    def levenshteinSimilarity(t: String): Double = {
      Math.exp(-levenshteinDistance(t))
    }

    /**
      * Jaccard similarity between this string and another string.
      *
      * @note Neither string can be shorter than `n` characters.
      * @note Ranges from 0 (no match) to 1 (perfect match).
      */
    def jaccardSimilarity(t: String, n: Int = 1): Double = {
      require(s.length >= n)
      require(t.length >= n)
      JaccardMetric(n).compare(s, t).get
    }
  }

  implicit class ExtendedJDuration(jduration: JDuration)  {
    def asScala: FiniteDuration = {
      jduration.getSeconds.seconds + jduration.getNano.nanos
    }
  }

  implicit class ExtendedFuture[T](future: Future[T])(implicit ec: ExecutionContext, logger: Logger) {
    def logErrors(message: String): Unit = {
      future.onFailure {
        case NonFatal(throwable) => logger.error(message, throwable)
      }
    }
  }
}
