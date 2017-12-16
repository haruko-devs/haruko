package verification

import java.net.InetAddress

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
