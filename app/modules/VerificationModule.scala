package modules

import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.zip.ZipInputStream

import com.blueconic.browscap.impl.UserAgentFileParser
import com.blueconic.browscap.{BrowsCapField, UserAgentParser, UserAgentService}
import play.api.inject._
import play.api.{Configuration, Environment, Logger}
import resource._

import scala.collection.JavaConverters._

/**
  * Config for Haruko's verification flow.
  */
class VerificationModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = {
    val configBlock = configuration.getConfig("verification").getOrElse {
      throw new RuntimeException("No verification block in Play config!")
    }

    val userAgentParser = configBlock
      .getString("browscapZipPath")
      .map(loadUserAgentParser)
      .getOrElse {
        Logger.warn(
          "No browscapZipPath in Play verification config block. " +
          s"Falling back to bundled Browscap data version ${UserAgentService.BUNDLED_BROWSCAP_VERSION}."
        )
        new UserAgentService().loadParser()
      }

    Seq(
      bind[UserAgentParser].toInstance(userAgentParser)
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
