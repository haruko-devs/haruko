package verification

import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}

import scala.util.Try

import play.api.libs.json.{JsObject, JsValue, Json}

import com.maxmind.db.CHMCache
import com.maxmind.geoip2.{DatabaseProvider, DatabaseReader}

trait GeoIP extends AutoCloseable {
  def json(ip: InetAddress): JsObject
}

/**
  * Convenience interface to GeoIP2 databases.
  */
class GeoIPImpl(
  asnDB: DatabaseProvider,
  cityDB: DatabaseProvider
) extends GeoIP {

  override def close(): Unit = {
    /**
      * Not all DatabaseProvider instances are AutoCloseable.
      */
    def softClose(db: AnyRef): Unit = {
      db match {
        case closeable: AutoCloseable => closeable.close()
        case _ => ()
      }
    }

    softClose(asnDB)
    softClose(cityDB)
  }

  def json(ip: InetAddress): JsObject = {
    val builder = Map.newBuilder[String, JsValue]
    Try(asnDB.asn(ip)).foreach(x => builder += ("asn" -> Json.parse(x.toJson)))
    Try(cityDB.city(ip)).foreach(x => builder += ("city" -> Json.parse(x.toJson)))
    JsObject(builder.result())
  }
}

object GeoIPImpl {
  def apply(geoipDir: String): GeoIPImpl = {

    def openDBWithCache(path: Path): DatabaseReader = {
      new DatabaseReader.Builder(path.toFile)
        .withCache(new CHMCache())
        .build()
    }

    val baseDir = Paths.get(geoipDir)

    val asnFile = baseDir.resolve("GeoLite2-ASN.mmdb")

    val cityFile = Some(baseDir.resolve("GeoIP2-City.mmdb"))
      .filter(Files.exists(_))
      .getOrElse(baseDir.resolve("GeoLite2-City.mmdb"))

    new GeoIPImpl(
      asnDB = openDBWithCache(asnFile),
      cityDB = openDBWithCache(cityFile)
    )
  }
}

/**
  * Stub for development and testing.
  */
object GeoIPStub extends GeoIP {
  override def close(): Unit = ()
  def json(ip: InetAddress): JsObject = JsObject(Map.empty[String, JsValue])
}
