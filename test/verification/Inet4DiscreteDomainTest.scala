package verification

class Inet4DiscreteDomainTest extends org.scalatest.FunSuite {
  test("increment max") {
    Inet4DiscreteDomain.next(Inet4DiscreteDomain.maxValue.get).isEmpty
  }

  test("decrement min") {
    Inet4DiscreteDomain.previous(Inet4DiscreteDomain.minValue.get).isEmpty
  }
}
