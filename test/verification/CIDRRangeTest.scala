package verification

import com.google.common.net.InetAddresses
import org.scalatest.FunSuite

class CIDRRangeTest extends FunSuite {

  test("apply IPv4") {
    val cidr = CIDRRange("127.0.0.1/30")
    assert(cidr.start === InetAddresses.forString("127.0.0.0"))
    assert(cidr.end === InetAddresses.forString("127.0.0.3"))
  }
}
