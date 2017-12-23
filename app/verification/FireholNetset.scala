package verification

import java.net.{Inet4Address, InetAddress}
import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._
import scala.io.{Codec, Source}
import resource._

import play.api.libs.json.{JsBoolean, JsObject}

import com.google.common.net.InetAddresses
import org.feijoas.mango.common.collect._

/**
  * A range of networks from the FireHOL project's IP lists.
  *
  * @note Only supports IPv4 for now.
  *
  * @see https://github.com/firehol/blocklist-ipsets
  * @see https://iplists.firehol.org/?ipset=firehol_anonymous for an example
  */
case class FireholNetset
(
  name: String,
  ranges: RangeSet[Inet4Address, Inet4Ordering.type]
) {
  def contains(ip: InetAddress): Boolean = {
    ip match {
      case v4: Inet4Address => ranges.contains(v4)
      case _ => false
    }
  }

  def contains(ip: String): Boolean = {
    contains(InetAddresses.forString(ip))
  }
}

object FireholNetset {
  def apply(netsetPath: Path): FireholNetset = {
    val name = netsetPath.getFileName.toString.stripSuffix(".netset")
    (for {
      source <- managed(Source.fromFile(netsetPath.toFile)(Codec.UTF8))
    } yield {
      FireholNetset(name, source)
    }).apply(identity)
  }

  def apply(name: String, source: Source): FireholNetset = {
    val builder = RangeSet.newBuilder[Inet4Address, Inet4Ordering.type](Inet4Ordering)
    builder ++= source
      .getLines()
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .filter(_.nonEmpty)
      .map(CIDRRange.apply)
      .filter(_.address.isInstanceOf[Inet4Address])
      .map { cidr =>
        Range.closed(
          cidr.start.asInstanceOf[Inet4Address],
          cidr.end.asInstanceOf[Inet4Address]
        )(Inet4Ordering)
      }
    FireholNetset(name, builder.result())
  }
}

case class FireholNetsets
(
  netsets: Set[FireholNetset]
) {
  def json(ip: InetAddress): JsObject = {
    JsObject(netsets.toSeq.map { netset =>
      netset.name -> JsBoolean(netset.contains(ip))
    })
  }
}

object FireholNetsets {
  def apply(netsetDir: String): FireholNetsets = {
    FireholNetsets(
      Files.list(Paths.get(netsetDir)).iterator().asScala
        .filter(_.getFileName.toString.endsWith(".netset"))
        .map(FireholNetset.apply)
        .toSet
    )
  }
}
