package modules

import play.api.inject._
import play.api.{Configuration, Environment}

import com.google.api.client.googleapis.util.Utils
import com.google.api.client.http.{HttpRequest, HttpRequestInitializer}
import com.google.api.services.customsearch.{Customsearch, CustomsearchRequestInitializer}
import com.google.common.util.concurrent.RateLimiter
import net.ruippeixotog.scalascraper.browser.{Browser, HtmlUnitBrowser, JsoupBrowser}

import bot.GoogleCustomSearchConfig

/**
  * Config for Haruko's web search commands.
  */
class SearchModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration): Seq[Binding[_]] = {
    val configBlock = configuration.getConfig("search").getOrElse {
      throw new RuntimeException("No search block in Play config!")
    }

    val rateLimiter = RateLimiter.create(
      configBlock
        .getDouble("rateLimit")
        .getOrElse {
          throw new RuntimeException("No rateLimit in Play search config block!")
        }
    )

    val browser = configBlock
      .getString("browser", Some(Set("jsoup", "htmlunit")))
      .getOrElse {
        throw new RuntimeException("No browser in Play search config block!")
      } match {
      case "jsoup" => JsoupBrowser()
      case "htmlunit" => HtmlUnitBrowser()
    }

    val googleCustomSearchConfig = GoogleCustomSearchConfig(
      id = configBlock
        .getString("gisCustomSearchEngineID")
        .getOrElse {
          throw new RuntimeException("No gisCustomSearchEngineID in Play search config block!")
        }
    )

    //noinspection ConvertExpressionToSAM
    val customsearch = new Customsearch.Builder(
      Utils.getDefaultTransport,
      Utils.getDefaultJsonFactory,
      // Doesn't need a request initializer because it uses a one-off API key scheme (see below).
      new HttpRequestInitializer {
        override def initialize(request: HttpRequest): Unit = {}
      }
    )
      .setApplicationName("Haruko")
      .setCustomsearchRequestInitializer(
        new CustomsearchRequestInitializer(
          configBlock
            .getString("gisCustomSearchEngineAPIKey")
            .getOrElse {
              throw new RuntimeException("No gisCustomSearchEngineAPIKey in Play search config block!")
            }
        )
      )
      .build()

    Seq(
      bind[RateLimiter].toInstance(rateLimiter),
      bind[Browser].toInstance(browser),
      bind[GoogleCustomSearchConfig].toInstance(googleCustomSearchConfig),
      bind[Customsearch].toInstance(customsearch)
    )
  }
}
