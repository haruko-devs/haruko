package bot

import java.net.URI
import javax.inject.Inject

import scala.collection.JavaConverters._
import scala.concurrent.{Future, blocking}

import play.core.parsers.FormUrlEncodedParser

import com.google.api.services.customsearch.Customsearch
import com.google.common.net.UrlEscapers
import com.google.common.util.concurrent.RateLimiter
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.model.Document


trait SearchEngine {
  val shortcut: String
  val desc: String

  /**
    * Assumed to be a blocking sync operation for now.
    */
  def search(query: String): Option[String]
}

trait URLPatternSearchEngine extends SearchEngine {

  val urlPattern: String

  /**
    * Get the URL for a given free-text query.
    *
    * @note Assumes the search engine is OK with it being form-encoded.
    */
  def url(query: String): String = {
    val encodedQuery = UrlEscapers.urlFormParameterEscaper().escape(query)
    urlPattern.replace("%s", encodedQuery)
  }
}

/**
  * Simple search engine that just fills in a URL template.
  */
case class PatternOnlySearchEngine(
  shortcut: String,
  urlPattern: String,
  desc: String
) extends URLPatternSearchEngine {
  override def search(query: String): Option[String] = Some(url(query))
}

/**
  * Search engine shortcuts like Firefox smart keywords/Chrome Omnibox search engines.
  *
  * @see https://support.google.com/chrome/answer/95426
  */
case class ScraperSearchEngine(
  shortcut: String,
  urlPattern: String,
  desc: String,
  extractFirstResult: Document => Option[String]
)(
  implicit val browser: Browser
) extends URLPatternSearchEngine {
  override def search(query: String): Option[String] = extractFirstResult(browser.get(url(query)))
}

/**
  * Generate a preview from an Amazon product page.
  */
case class AmazonSearchEngine
(
  shortcut: String,
  urlPattern: String,
  desc: String
)(
  implicit val browser: Browser
) extends URLPatternSearchEngine {
  override def search(query: String): Option[String] = {
    (browser.get(url(query)) >?> attr("href")("#atfResults .s-result-item a"))
      .flatMap { productURL =>
        val absProductURL = new URI("https://www.amazon.com/").resolve(productURL)
        // TODO: almost always errors out from here with 404, but URL works fine in cURL
        val productPage = browser.get(absProductURL.toString)
        val picURL = productPage >?> attr("src")("#landingImage")
        val descParts = productPage >?> texts("#productDescription")
        val msgParts = picURL.toSeq ++ descParts.map(_.filter(_.trim.nonEmpty)).getOrElse(Seq.empty)
        if (msgParts.isEmpty) {
          None
        } else {
          val msg = msgParts.mkString("\n")
          Some(msg)
        }
      }
  }
}

case class GoogleCustomSearchConfig(
  id: String
)

/**
  * Uses the Google Custom Search Engine API instead of scraping Google result pages.
  *
  * @see https://developers.google.com/custom-search/json-api/v1/using_rest
  * @see https://developers.google.com/custom-search/json-api/v1/reference/cse/list
  * @see https://support.google.com/cloud/answer/7035610
  */
case class GoogleCustomSearchEngine(
  shortcut: String,
  desc: String,
  searchType: String = null,
  siteSearch: String = null
)(
  implicit googleCustomSearchConfig: GoogleCustomSearchConfig,
  customsearch: Customsearch
) extends SearchEngine {
  override def search(query: String): Option[String] = {
    customsearch
      .cse()
      .list(query)
      .setCx(googleCustomSearchConfig.id)
      .setSearchType(searchType)
      .setSiteSearch(siteSearch)
      .setSafe("high")
      .execute()
      .getItems
      .asScala
      .map(_.getLink)
      .headOption
  }
}

class Searcher @Inject() (
  implicit browser: Browser,
  rateLimiter: RateLimiter,
  googleCustomSearchConfig: GoogleCustomSearchConfig,
  customsearch: Customsearch
) {
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  /**
    * Get the final URL of a search result after chasing redirects.
    */
  private def redirectExtractor(doc: Document): Option[String] = Some(doc.location)

  /**
    * Resolve URL relative to base.
    */
  private def rel(base: String)(url: String): String = {
    new URI(base).resolve(url).toString
  }

  val engines: Map[String, SearchEngine] = Seq[SearchEngine](
    GoogleCustomSearchEngine(
      shortcut = "g",
      desc = "Google Search"
    ),
    ScraperSearchEngine(
      shortcut = "g_fallback",
      urlPattern = "https://www.google.com/search?q=%s&btnI=1", // Use I'm Feeling Lucky button.
      desc = "Google",
      extractFirstResult = redirectExtractor
    ),
    PatternOnlySearchEngine(
      shortcut = "lmgtfy",
      urlPattern = "https://lmgtfy.com/?q=%s",
      desc = "Let Me Google That For You"
    ),
    AmazonSearchEngine(
      shortcut = "az",
      urlPattern = "https://www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=%s",
      desc = "Amazon"
    ),
    ScraperSearchEngine(
      shortcut = "pm",
      urlPattern = "https://www.ncbi.nlm.nih.gov/pmc/?term=%s",
      desc = "PubMed",
      extractFirstResult = _ >?> attr("href")(".rslt .title a").map(rel("https://www.ncbi.nlm.nih.gov/"))
    ),
    ScraperSearchEngine(
      shortcut = "ddg",
      urlPattern = "https://duckduckgo.com/?q=%s",
      desc = "DuckDuckGo",
      extractFirstResult = _ >?> attr("href")(".result__a").map(FormUrlEncodedParser.parse(_)("uddg").head)
    ),
    ScraperSearchEngine(
      shortcut = "ebay",
      urlPattern = "http://www.ebay.com/sch/i.html?_from=R40&_nkw=%s&_sacat=0",
      desc = "eBay",
      extractFirstResult = _ >?> attr("href")(".lvtitle a")
    ),
    ScraperSearchEngine(
      shortcut = "fb",
      urlPattern = "https://m.facebook.com/search/top/?q=%s&opensearch=1",
      desc = "Facebook",
      extractFirstResult = _ >?> attr("href")("tr a").map(rel("https://www.facebook.com/"))
    ),
    ScraperSearchEngine(
      shortcut = "gh",
      urlPattern = "https://github.com/search?q=%s&ref=opensearch",
      desc = "GitHub",
      extractFirstResult = _ >?> attr("href")(".repo-list-item a").map(rel("https://github.com/"))
    ),
    ScraperSearchEngine(
      shortcut = "gr",
      urlPattern = "https://www.goodreads.com/search?q=%s",
      desc = "GoodReads",
      extractFirstResult = _ >?> attr("href")(".bookTitle").map(rel("https://www.goodreads.com/"))
    ),
    GoogleCustomSearchEngine(
      shortcut = "gis",
      desc = "Google Image Search",
      searchType = "image"
    ),
    ScraperSearchEngine(
      shortcut = "gis_fallback",
      urlPattern = "https://www.google.com/search?tbm=isch&q=%s",
      desc = "Google Image Search (low-res fallback version)",
      extractFirstResult = _ >?> attr("src")("#ires td img")
    ),
    ScraperSearchEngine(
      shortcut = "r",
      urlPattern = "https://www.reddit.com/search?q=%s",
      desc = "Reddit (posts)",
      extractFirstResult = _ >?> attr("href")(".search-result-link a").map(rel("https://www.reddit.com/"))
    ),
    ScraperSearchEngine(
      shortcut = "rsub",
      urlPattern = "https://www.reddit.com/search?q=%s",
      desc = "Reddit (subreddits)",
      extractFirstResult = _ >?> attr("href")(".search-result-subreddit a")
    ),
    ScraperSearchEngine(
      shortcut = "steam",
      urlPattern = "https://store.steampowered.com/search/?ref=os&term=%s",
      desc = "Steam",
      extractFirstResult = _ >?> attr("href")(".search_result_row")
    ),
    ScraperSearchEngine(
      shortcut = "tv",
      urlPattern = "https://thetvdb.com/index.php?seriesname=%s&fieldlocation=2&language=7&order=translation&searching=Search&tab=advancedsearch",
      desc = "The TV DB",
      extractFirstResult = _ >?> attr("href")("#listtable tr td.odd a").map(rel("https://thetvdb.com/"))
    ),
    ScraperSearchEngine(
      shortcut = "tw",
      urlPattern = "https://twitter.com/search?q=%s",
      desc = "Twitter",
      extractFirstResult = { doc =>
        (doc >?> element("#timeline [data-item-type]")).flatMap { elem =>
          elem >> attr("data-item-type") match {
            case "tweet" => elem >?> attr("data-permalink-path")(".tweet").map(rel("https://twitter.com/"))
            case "user" => elem >?> attr("data-screen-name")(".ProfileCard").map(rel("https://twitter.com/"))
            case _ => None
          }
        }
      }
    ),
    ScraperSearchEngine(
      shortcut = "wp",
      urlPattern = "https://en.wikipedia.org/w/index.php?search=%s&button=&title=Special%3ASearch",
      desc = "Wikipedia",
      extractFirstResult = redirectExtractor
    ),
    GoogleCustomSearchEngine(
      shortcut = "yt",
      desc = "YouTube",
      siteSearch = "youtube.com"
    ),
    ScraperSearchEngine(
      shortcut = "yt_fallback",
      urlPattern = "https://www.youtube.com/results?search_query=%s&page={startPage?}&utm_source=opensearch",
      desc = "YouTube",
      extractFirstResult = _ >?> attr("href")("#results .yt-uix-tile-link").map(rel("https://www.youtube.com/"))
    ),
    ScraperSearchEngine(
      shortcut = "imdb",
      urlPattern = "https://www.imdb.com/find?ref_=nv_sr_fn&q=%s&s=all",
      desc = "IMDb",
      extractFirstResult = _ >?> attr("href")(".findResult a").map(rel("https://www.imdb.com/"))
    ),
    ScraperSearchEngine(
      shortcut = "kym",
      urlPattern = "http://knowyourmeme.com/search?q=%s",
      desc = "Know Your Meme",
      extractFirstResult = _ >?> attr("href")("#entries td a").map(rel("http://knowyourmeme.com/"))
    ),
    ScraperSearchEngine(
      shortcut = "igdb",
      urlPattern = "https://www.igdb.com/search?utf8=%E2%9C%93&type=1&q=%s",
      desc = "Internet Games Database",
      extractFirstResult = _ >?> attr("href")("#search-results .game-result .media-heading a").map(rel("https://www.igdb.com/"))
    ),
    ScraperSearchEngine(
      shortcut = "wh",
      urlPattern = "https://www.wikihow.com/wikiHowTo?search=%s",
      desc = "WikiHow",
      extractFirstResult = _ >?> attr("href")("#searchresults_list .result_link").map(rel("https://www.igdb.com/"))
    )
  )
    .map(engine => engine.shortcut -> engine)
    .toMap

  def search(shortcut: String, query: String): Future[Option[String]] = {
    Future {
      blocking {
        rateLimiter.acquire()
        engines(shortcut).search(query)
      }
    }
  }
}
