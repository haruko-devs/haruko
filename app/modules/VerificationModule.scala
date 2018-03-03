package modules

import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.zip.ZipInputStream

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
import resource._

import play.api.inject._
import play.api.{Configuration, Environment, Logger, Mode}

import com.blueconic.browscap.impl.UserAgentFileParser
import com.blueconic.browscap.{BrowsCapField, UserAgentParser, UserAgentService}

import verification._

/**
  * Config for Haruko's verification flow.
  */
class VerificationModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = {
    val configBlock = configuration.getConfig("verification").getOrElse {
      throw new RuntimeException("No verification block in Play config!")
    }

    val userAgentParser: UserAgentParser = configBlock
      .getString("browscapZipPath")
      .map(Success(_))
      .getOrElse(Failure(new Exception("No browscapZipPath in Play verification config block.")))
      .map(loadUserAgentParser)
      .recover {
        case NonFatal(e) if environment.mode != Mode.Prod =>
          Logger.warn(s"Falling back to bundled Browscap data version ${UserAgentService.BUNDLED_BROWSCAP_VERSION}.")
          new UserAgentService().loadParser()
      }
      .get

    val geoIP = configBlock
      .getString("geoipDir")
      .map(Success(_))
      .getOrElse(Failure(new Exception("No geoipDir in Play verification config block.")))
      .map(GeoIPImpl.apply)
      .recover {
        case NonFatal(e) if environment.mode != Mode.Prod =>
          Logger.warn(s"Falling back to GeoIP dev/test stub.")
          GeoIPStub
      }
      .get

    val fireholNetsets = configBlock
      .getString("fireholDir")
      .map(Success(_))
      .getOrElse(Failure(new Exception("No fireholDir in Play verification config block.")))
      .map(FireholNetsetsImpl.apply)
      .recover {
        case NonFatal(e) if environment.mode != Mode.Prod =>
          Logger.warn(s"Falling back to FireHOL dev/test stub.")
          FireholNetsetsStub
      }
      .get

    // TODO: periodic reload of these files
    Seq(
      bind[UserAgentParser].toInstance(userAgentParser),
      bind[GeoIP].toInstance(geoIP),
      bind[FireholNetsets].toInstance(fireholNetsets)
    )
  }

  /**
    * Workaround for poor choices in BlueConic Browscap data loader.
    */
  def loadUserAgentParser(browscapZipPath: String): UserAgentParser = {
    // This does not work because UserAgentService assumes that the first file in the ZIP is a CSV.
    // I don't know why anyone would ever assume that, instead of looking for a CSV extension.
    //new UserAgentService(browscapZipPath).loadParser()

    // This means we can't use the Browscap ZIP distribution because there's more than one file in it.
    // So we have to work around this issue:
    val browscapDefaultFields = BrowsCapField.values().filter(_.isDefault).toSet
    (for {
      input <- managed(Files.newInputStream(Paths.get(browscapZipPath)))
      zip <- managed(new ZipInputStream(input))
      // Advance to first CSV in file.
      _ = Iterator
        .continually(zip.getNextEntry)
        .takeWhile(_ != null)
        .find(_.getName.endsWith(".csv"))
        .getOrElse {
          throw new RuntimeException(s"Couldn't find CSV file in $browscapZipPath!")
        }
      reader <- managed(new InputStreamReader(zip, StandardCharsets.UTF_8))
    } yield {
      UserAgentFileParser.parse(reader, browscapDefaultFields.asJava)
    }).apply(identity)
  }
}
