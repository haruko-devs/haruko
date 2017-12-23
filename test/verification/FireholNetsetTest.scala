package verification

import scala.io.Source

import org.scalatest.FunSuite

class FireholNetsetTest extends FunSuite {

  val testNetsetText: String =
    """
      |# comment followed by blank
      |
      |# another comment
      |2.188.79.6
      |222.186.45.122/31
    """.stripMargin.trim

  test("contains") {
    val netset = FireholNetset("test", Source.fromString(testNetsetText))
    assert(netset.contains("2.188.79.6"))
    assert(netset.contains("222.186.45.122"))
    assert(netset.contains("222.186.45.123"))
    assert(!netset.contains("127.0.0.1"))
  }
}
