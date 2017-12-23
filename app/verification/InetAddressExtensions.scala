package verification

import java.net.{Inet4Address, Inet6Address, InetAddress}

import com.google.common.net.InetAddresses
import org.feijoas.mango.common.collect.DiscreteDomain

object InetAddressExtensions {
  implicit class InetAddressExtensions(ip: InetAddress) {
    def isSpecial: Boolean = {
      ip.isLoopbackAddress ||
        ip.isAnyLocalAddress ||
        ip.isLinkLocalAddress ||
        ip.isSiteLocalAddress ||
        ip.isMulticastAddress
    }
  }
}

case class CIDRRange
(
  address: InetAddress,
  subnetBits: Int
) {
  if (subnetBits < 0 || subnetBits > maxSubnetBits) {
    throw new IllegalArgumentException(s"$address may not have a subnet mask with $subnetBits bits!")
  }

  def maxSubnetBits: Int = CIDRRange.maxSubnetBits(address)

  def start: InetAddress = {
    val bytes = address.getAddress
    var i = bytes.length - 1
    var changeBits = maxSubnetBits - subnetBits
    while (i >= 0) {
      bytes(i) = (bytes(i) & (-1 << changeBits.max(0).min(8))).toByte
      i -= 1
      changeBits -= 8
    }
    InetAddress.getByAddress(bytes)
  }

  def end: InetAddress = {
    val bytes = address.getAddress
    var i = bytes.length - 1
    var changeBits = maxSubnetBits - subnetBits
    while (i >= 0) {
      bytes(i) = (bytes(i) | ~(-1 << changeBits.max(0).min(8))).toByte
      i -= 1
      changeBits -= 8
    }
    InetAddress.getByAddress(bytes)
  }
}

object CIDRRange {
  def apply(cidrStr: String): CIDRRange = {
    val (addressStr, maybeSubnetBitsStr) = cidrStr.split('/') match {
      case Array(a, s) => (a, Some(s))
      case Array(a) => (a, None)
      case _ => throw new IllegalArgumentException(s"$cidrStr is not a valid CIDR range!")
    }
    val address = InetAddresses.forString(addressStr)
    val subnetBits = maybeSubnetBitsStr
      .map(_.toInt)
      .getOrElse(maxSubnetBits(address))

    CIDRRange(address, subnetBits)
  }

  def maxSubnetBits(address: InetAddress): Int = {
    address match {
      case _: Inet4Address => 32
      case _: Inet6Address => 128
      case _ => throw new IllegalArgumentException(s"$address is not from a known IP address family!")
    }
  }
}

object Inet4DiscreteDomain extends DiscreteDomain[Inet4Address] {
  override def next(value: Inet4Address): Option[Inet4Address] = {
    if (InetAddresses.isMaximum(value)) {
      None
    } else {
      Some(InetAddresses.increment(value).asInstanceOf[Inet4Address])
    }
  }

  override def previous(value: Inet4Address): Option[Inet4Address] = {
    // This version of Guava doesn't have decrement() yet.
    val bytes = value.getAddress
    var i = bytes.length - 1
    while (i >= 0 && bytes(i) == 0x00.toByte) {
      bytes(i) == 0xff.toByte
      i -= 1
    }
    if (i < 0) {
      None
    } else {
      bytes(i) = (bytes(i) - 1).toByte
      Some(InetAddress.getByAddress(bytes).asInstanceOf[Inet4Address])
    }
  }

  override def distance(start: Inet4Address, end: Inet4Address): Long = {
    val startLong = Integer.toUnsignedLong(InetAddresses.coerceToInteger(start))
    val endLong = Integer.toUnsignedLong(InetAddresses.coerceToInteger(end))
    endLong - startLong
  }

  override val minValue: Option[Inet4Address] = Some(InetAddress.getByAddress(Array.fill(4)(0x00.toByte)).asInstanceOf[Inet4Address])

  override val maxValue: Option[Inet4Address] = Some(InetAddress.getByAddress(Array.fill(4)(0xff.toByte)).asInstanceOf[Inet4Address])
}

object Inet4Ordering extends Ordering[Inet4Address] {
  override def compare(x: Inet4Address, y: Inet4Address): Int = {
    Integer.compareUnsigned(
      InetAddresses.coerceToInteger(x),
      InetAddresses.coerceToInteger(y)
    )
  }
}
