package verification

import play.api.libs.json._
import play.api.mvc.{Headers, RequestHeader}

case class WebSummary
(
  agreedToTOS: Option[Boolean],
  ip: Option[String],
  isp: Option[String],
  isProxy: Option[Boolean],
  browser: Option[String],
  deviceFingerprint: Option[String],
  tz: Option[String],
  languages: Option[Seq[String]],
  physicalAddress: Option[Seq[String]]
)

object WebSummary {

  def apply(maybeLandingData: Option[JsObject], maybeTOSData: Option[JsObject]): WebSummary = {
    val data: JsValue = JsObject(
      Seq(maybeLandingData, maybeTOSData)
        .flatMap(_.toSeq.flatMap(_.fields))
    )

    val agreedToTOS = (data \ "agreed").asOpt[Boolean]

    val ip: Option[String] = (data \ "ip").asOpt[String]

    val ipMetadata: JsLookupResult = {
      ip.map(data \ "ip_metadata" \ _)
        .getOrElse(JsUndefined("No IP metadata found!"))
    }

    val isp: Option[String] = (ipMetadata \ "asn" \ "autonomous_system_organization").asOpt[String]

    val isProxy: Option[Boolean] = {
      val maxmindProxyTraits = Seq(
        "is_anonymous",
        "is_anonymous_proxy",
        "is_anonymous_vpn",
        "is_hosting_provider",
        "is_legitimate_proxy",
        "is_public_proxy",
        "is_tor_exit_node"
      )
      // The "firehol_anonymous" part could change if we switch or add IP lists.
      val proxyLookups: Seq[JsLookupResult] = (ipMetadata \ "firehol" \ "firehol_anonymous") +:
        maxmindProxyTraits.map(ipMetadata \ "city" \ "traits" \ _)
      if (proxyLookups.isEmpty) {
        None
      } else {
        Some(proxyLookups.exists(_.asOpt[Boolean].getOrElse(false)))
      }
    }

    val browser: Option[String] = {
      val nameParts: Seq[String] = Seq(
        "browser",
        "browser_major_version",
        "platform",
        "platform_version",
        "device_type",
        "browser_type"
      )
        .flatMap(property => (data \ "browser" \ property).asOpt[String])

        if (nameParts.isEmpty) {
          None
        } else {
          Some(nameParts.mkString(" "))
        }
      }

    val deviceFingerprint: Option[String] = {
      val fingerprintingErrors = Set("NOJS", "CANT_INIT_FINGERPRINT", "CANT_GET_FINGERPRINT")
      (data \ "fingerprint").asOpt[String].filterNot(fingerprintingErrors.contains)
    }

    val tz: Option[String] = {
      val tzErrors = Set("NOJS", "TZ_UNKNOWN")
      val clientTz = (data \ "timezone").asOpt[String].filterNot(tzErrors.contains)
      clientTz.orElse((ipMetadata \ "city" \ "location" \ "time_zone").asOpt[String])
    }

    val languages: Option[Seq[String]] = {
      (data \ "accept_language")
        .asOpt[String]
        .map(parseLanguages)
    }

    val physicalAddress: Option[Seq[String]] = {
      val geoLevels = Seq.newBuilder[JsLookupResult]
      geoLevels += (ipMetadata \ "city" \ "city")
      (ipMetadata \ "city" \ "subdivisions").asOpt[JsArray].foreach { _.value.foreach { geoLevels += JsDefined(_) } }
      geoLevels += (ipMetadata \ "city" \ "country")
      geoLevels += (ipMetadata \ "city" \ "continent")

      val geoNames: Seq[String] = geoLevels.result()
        // One more thing to change if we ever do i18n.
        .flatMap(geoLevel => (geoLevel \ "names" \ "en").asOpt[String])

      if (geoNames.isEmpty) {
        None
      } else {
        Some(geoNames)
      }
    }

    WebSummary(
      agreedToTOS = agreedToTOS,
      ip = ip,
      isp = isp,
      isProxy = isProxy,
      browser = browser,
      deviceFingerprint = deviceFingerprint,
      tz = tz,
      languages = languages,
      physicalAddress = physicalAddress
    )
  }

  //noinspection NotImplementedCode,TypeAnnotation
  case class AcceptLanguageParser(acceptLanguage: String) extends RequestHeader {

    override def headers = Headers(play.api.http.HeaderNames.ACCEPT_LANGUAGE -> acceptLanguage)

    override def id = ???
    override def tags = ???
    override def uri = ???
    override def path = ???
    override def method = ???
    override def version = ???
    override def queryString = ???
    override def remoteAddress = ???
    override def secure = ???
    override def clientCertificateChain = ???
  }

  def parseLanguages(acceptLanguage: String): Seq[String] = {
    AcceptLanguageParser("en-US,en;q=0.9").acceptLanguages.map(_.toLocale.getDisplayName)
  }
}

