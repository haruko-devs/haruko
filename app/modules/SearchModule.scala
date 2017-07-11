package modules

import net.ruippeixotog.scalascraper.browser.{Browser, HtmlUnitBrowser, JsoupBrowser}
import play.api.inject._
import play.api.{Configuration, Environment}

import com.google.common.util.concurrent.RateLimiter

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

    Seq(
      bind[RateLimiter].toInstance(rateLimiter),
      bind[Browser].toInstance(browser)
    )
  }
}
