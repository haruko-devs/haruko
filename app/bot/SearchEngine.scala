package bot

import java.net.URI
import javax.inject.Inject

import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.model.Document

import com.google.common.net.UrlEscapers
import com.google.common.util.concurrent.RateLimiter

import scala.concurrent.{Future, blocking}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import play.core.parsers.FormUrlEncodedParser

/**
  * Search engine shortcuts like Firefox smart keywords/Chrome Omnibox search engines.
  *
  * @see https://support.google.com/chrome/answer/95426
  */
case class SearchEngine
(
  shortcut: String,
  urlPattern: String,
  desc: String,
  extractFirstResult: Option[Document => Option[String]] = None
) {
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

object SearchEngine {
  /**
    * Get the final URL of a search result after chasing redirects.
    */
  private def redirectExtractor(doc: Document): Option[String] = Some(doc.location)

  val engines: Map[String, SearchEngine] = Seq[SearchEngine](
    SearchEngine(
      shortcut = "g",
      urlPattern = "https://www.google.com/search?q=%s&btnI=1", // Use I'm Feeling Lucky button.
      desc = "Google",
      extractFirstResult = Some(redirectExtractor)
    ),
    SearchEngine(
      shortcut = "lmgtfy",
      urlPattern = "https://lmgtfy.com/?q=%s",
      desc = "Let Me Google That For You"
    ),
    SearchEngine(
      shortcut = "az",
      urlPattern = "https://www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=%s",
      desc = "Amazon",
      extractFirstResult = Some(_ >?> attr("href")("#atfResults .s-result-item a"))
    ),
    SearchEngine(
      shortcut = "pm",
      urlPattern = "https://www.ncbi.nlm.nih.gov/pmc/?term=%s",
      desc = "PubMed",
      extractFirstResult = Some(_ >?> attr("href")(".rslt .title a").map("https://www.ncbi.nlm.nih.gov" + _))
    ),
    SearchEngine(
      shortcut = "ddg",
      urlPattern = "https://duckduckgo.com/?q=%s",
      desc = "DuckDuckGo",
      extractFirstResult = Some(_ >?> attr("href")(".result__a").map(FormUrlEncodedParser.parse(_)("uddg").head))
    ),
    SearchEngine(
      shortcut = "ebay",
      urlPattern = "http://www.ebay.com/sch/i.html?_from=R40&_nkw=%s&_sacat=0",
      desc = "eBay",
      extractFirstResult = Some(_ >?> attr("href")(".lvtitle a"))
    ),
    SearchEngine(
      shortcut = "fb",
      urlPattern = "https://m.facebook.com/search/top/?q=%s&opensearch=1",
      desc = "Facebook",
      extractFirstResult = Some(_ >?> attr("href")("tr a").map("https://www.facebook.com" + new URI(_).getPath))
    ),
    SearchEngine(
      shortcut = "gh",
      urlPattern = "https://github.com/search?q=%s&ref=opensearch",
      desc = "GitHub",
      extractFirstResult = Some(_ >?> attr("href")(".repo-list-item a").map("https://github.com" + _))
    ),
    SearchEngine(
      shortcut = "gr",
      urlPattern = "https://www.goodreads.com/search?q=%s",
      desc = "GoodReads",
      extractFirstResult = Some(_ >?> attr("href")(".bookTitle").map("https://www.goodreads.com" + _))
    ),
    SearchEngine(
      shortcut = "gis",
      urlPattern = "https://www.google.com/search?tbm=isch&q=%s",
      desc = "Google Image Search",
      extractFirstResult = Some(_ >?> attr("src")("#ires td img"))
    ),
    SearchEngine(
      shortcut = "r",
      urlPattern = "https://www.reddit.com/search?q=%s",
      desc = "Reddit (posts)",
      extractFirstResult = Some(_ >?> attr("href")(".search-result-link a").map("https://www.reddit.com" + _))
    ),
    SearchEngine(
      shortcut = "rsub",
      urlPattern = "https://www.reddit.com/search?q=%s",
      desc = "Reddit (subreddits)",
      extractFirstResult = Some(_ >?> attr("href")(".search-result-subreddit a"))
    ),
    SearchEngine(
      shortcut = "steam",
      urlPattern = "https://store.steampowered.com/search/?ref=os&term=%s",
      desc = "Steam",
      extractFirstResult = Some(_ >?> attr("href")(".search_result_row"))
    ),
    SearchEngine(
      shortcut = "tv",
      urlPattern = "https://thetvdb.com/index.php?seriesname=%s&fieldlocation=2&language=7&order=translation&searching=Search&tab=advancedsearch",
      desc = "The TV DB",
      extractFirstResult = Some(_ >?> attr("href")("#listtable tr td.odd a").map("https://thetvdb.com" + _))
    ),
    SearchEngine(
      shortcut = "tw",
      urlPattern = "https://twitter.com/search?q=%s",
      desc = "Twitter",
      extractFirstResult = Some(doc => (doc >?> element("#timeline [data-item-type]")).flatMap { elem =>
        elem >> attr("data-item-type") match {
          case "tweet" => elem >?> attr("data-permalink-path")(".tweet").map("https://twitter.com" + _)
          case "user" => elem >?> attr("data-screen-name")(".ProfileCard").map("https://twitter.com/" + _)
          case _ => None
        }
      })
    ),
    SearchEngine(
      shortcut = "wp",
      urlPattern = "https://en.wikipedia.org/w/index.php?search=%s&button=&title=Special%3ASearch",
      desc = "Wikipedia",
      extractFirstResult = Some(redirectExtractor)
    ),
    SearchEngine(
      shortcut = "yt",
      urlPattern = "https://www.youtube.com/results?search_query=%s&page={startPage?}&utm_source=opensearch",
      desc = "YouTube",
      extractFirstResult = Some(_ >?> attr("href")("#results .yt-uix-tile-link").map("https://www.youtube.com" + _))
    ),
    SearchEngine(
      shortcut = "imdb",
      urlPattern = "https://www.imdb.com/find?ref_=nv_sr_fn&q=%s&s=all",
      desc = "IMDb",
      extractFirstResult = Some(_ >?> attr("href")(".findResult a").map("https://www.imdb.com" + _))
    ),
    SearchEngine(
      shortcut = "kym",
      urlPattern = "http://knowyourmeme.com/search?q=%s",
      desc = "Know Your Meme",
      extractFirstResult = Some(_ >?> attr("href")("#entries td a").map("http://knowyourmeme.com" + _))
    )
  )
    .map(engine => engine.shortcut -> engine)
    .toMap
}

class Searcher @Inject() (
  browser: Browser,
  rateLimiter: RateLimiter
) {
  import play.api.libs.concurrent.Execution.Implicits.defaultContext

  def search(shortcut: String, query: String): Future[Option[String]] = {
    Future {
      blocking {
        rateLimiter.acquire()

        val engine = SearchEngine.engines(shortcut)
        val searchURL = engine.url(query)

        engine.extractFirstResult match {
          case None => Some(searchURL)
          case Some(extractor) =>
            extractor(browser.get(searchURL))
        }
      }
    }
  }
}
