package bot

import com.google.common.net.UrlEscapers

/**
  * Search engine shortcuts like Firefox smart keywords/Chrome Omnibox search engines.
  *
  * @see https://support.google.com/chrome/answer/95426
  */
case class SearchEngine
(
  shortcut: String,
  urlPattern: String,
  desc: String
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
  val engines: Map[String, SearchEngine] = Seq[SearchEngine](
    SearchEngine(
      shortcut = "g",
      urlPattern = "https://www.google.com/search?q=%s",
      desc = "Google"
    ),
    SearchEngine(
      shortcut = "lmgtfy",
      urlPattern = "https://lmgtfy.com/?q=%s",
      desc = "Let Me Google That For You"
    ),
    SearchEngine(
      shortcut = "az",
      urlPattern = "https://www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=%s",
      desc = "Amazon"
    ),
    SearchEngine(
      shortcut = "citeseer",
      urlPattern = "https://citeseerx.ist.psu.edu/search?q=%s&start={startIndex?}&sort=cite&q=%s&start={startIndex?}&sort=cite",
      desc = "CiteSeerX"
    ),
    SearchEngine(
      shortcut = "pm",
      urlPattern = "https://www.ncbi.nlm.nih.gov/pmc/?term=%s",
      desc = "PubMed"
    ),
    SearchEngine(
      shortcut = "ddg",
      urlPattern = "https://duckduckgo.com/?q=%s",
      desc = "DuckDuckGo"
    ),
    SearchEngine(
      shortcut = "ebay",
      urlPattern = "http://www.ebay.com/sch/i.html?_from=R40&_nkw=%s&_sacat=0",
      desc = "eBay"
    ),
    SearchEngine(
      shortcut = "fb",
      urlPattern = "https://www.facebook.com/search/top/?q=%s&opensearch=1",
      desc = "Facebook"
    ),
    SearchEngine(
      shortcut = "gh",
      urlPattern = "https://github.com/search?q=%s&ref=opensearch",
      desc = "GitHub"
    ),
    SearchEngine(
      shortcut = "gr",
      urlPattern = "https://www.goodreads.com/search?q=%s",
      desc = "GoodReads"
    ),
    SearchEngine(
      shortcut = "gis",
      urlPattern = "https://www.google.com/search?tbm=isch&q=%s",
      desc = "Google Image Search"
    ),
    SearchEngine(
      shortcut = "tr",
      urlPattern = "https://translate.google.com/?source=osdd#auto|auto|%s",
      desc = "Google Translate"
    ),
    SearchEngine(
      shortcut = "r",
      urlPattern = "https://www.reddit.com/search?q=%s",
      desc = "Reddit"
    ),
    SearchEngine(
      shortcut = "steam",
      urlPattern = "https://store.steampowered.com/search/?ref=os&term=%s",
      desc = "Steam"
    ),
    SearchEngine(
      shortcut = "tv",
      urlPattern = "https://thetvdb.com/?string=%s&searchseriesid=&tab=listseries&function=Search",
      desc = "The TV DB"
    ),
    SearchEngine(
      shortcut = "tw",
      urlPattern = "https://twitter.com/search?q=%s",
      desc = "Twitter"
    ),
    SearchEngine(
      shortcut = "wp",
      urlPattern = "https://en.wikipedia.org/w/index.php?search=%s&button=&title=Special%3ASearch",
      desc = "wp"
    ),
    SearchEngine(
      shortcut = "yt",
      urlPattern = "https://www.youtube.com/results?search_query=%s&page={startPage?}&utm_source=opensearch",
      desc = "YouTube"
    ),
    SearchEngine(
      shortcut = "imdb",
      urlPattern = "https://www.imdb.com/find?ref_=nv_sr_fn&q=%s&s=all",
      desc = "IMDb"
    )
  )
    .map(engine => engine.shortcut -> engine)
    .toMap
}
