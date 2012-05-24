package test

import com.shc.http._
import com.shc.codec._
import org.apache.commons.codec.binary.{ Base64 => apache64 }
import org.apache.commons.codec.net.{ URLCodec => apacheURL }
import java.net.URLEncoder.{ encode => javaURLenc }
import java.net.URLDecoder.{ decode => javaURLdec }

object SpeedComparison {

  def getTime = System.currentTimeMillis
  var start: Long = _
  var end: Long = _
  var test = scala.util.Random.nextString(100)

  def main(args: Array[String]):Unit = {
    urlEncodeTest
    urlDecodeTest
    base64EncodeTest
    base64DecodeTest
  }

  def urlEncodeTest() {
    val enc = URLCodec
    var mine: Long = 0
    var javas: Long = 0
    var apache: Long = 0

    start = getTime
    Range(0, 5000).foreach(i => javaURLenc(test))
    end = getTime
    javas = end - start

    start = getTime
    Range(0, 5000).foreach(i => new String(apacheURL.encodeUrl(null, test.getBytes), "UTF-8"))
    end = getTime
    apache = end - start

    start = getTime
    Range(0, 5000).foreach(i => enc.encode(test))
    end = getTime
    mine = end - start

    println("[url encode]Slower %s times than java.net.URLEncoder".format(mine / javas))
    println("[url encode]Slower %s times than apache-commons".format(mine / apache))
    println("codec.PercentEncoder : " + mine)
    println("apache-commons       : " + apache)
    println("java.net.URLEncoder  : " + javas)
  }

  def urlDecodeTest() {
    val enc = URLCodec
    var mine: Long = 0
    var javas: Long = 0
    var apache: Long = 0

    test = javaURLenc(test)

    start = getTime
    Range(0, 5000).foreach(i => javaURLdec(test))
    end = getTime
    javas = end - start

    start = getTime
    Range(0, 5000).foreach(i => new String(apacheURL.decodeUrl(test.getBytes)))
    end = getTime
    apache = end - start

    start = getTime
    Range(0, 5000).foreach(i => enc.decode(test))
    end = getTime
    mine = end - start

    println("[url decode]Slower %s times than java.net.URLEncoder".format(mine / javas))
    println("[url decode]Slower %s times than apache-commons".format(mine / apache))
    println("codec.PercentEncoder : " + mine)
    println("apache-commons       : " + apache)
    println("java.net.URLEncoder  : " + javas)
  }

  def base64EncodeTest() {

    var mine1: Long = 0
    var mine2: Long = 0
    var apache: Long = 0

    start = getTime
    for (i <- 0 to 5000) { apache64.encodeBase64String(test.getBytes) }
    end = getTime
    apache = end - start

    val enc = Base64
    start = getTime
    for (i <- 0 to 5000) { enc.encodeSlow(test) }
    end = getTime
    mine1 = end - start

    start = getTime
    for (i <- 0 to 5000) { enc.encode(test) }
    end = getTime
    mine2 = end - start

    println("[base64 encode1]Slower %s times than apache".format(mine1 / apache))
    println("[base64 encode2]Slower %s times than apache".format(mine2 / apache))
    println("codec.Base64.encode     : " + mine1)
    println("codec.Base64.encodeFast : " + mine2)
    println("apache-commons          : " + apache)
  }

  def base64DecodeTest() {

    var mine1: Long = 0
    var mine2: Long = 0
    var apache: Long = 0

    test = apache64.encodeBase64String(test.getBytes)

    start = getTime
    for (i <- 0 to 10000) { new String(apache64.decodeBase64(test), "UTF-8") }
    end = getTime
    apache = end - start

    val enc = Base64
    start = getTime
    for (i <- 0 to 10000) { enc.decodeSlow(test) }
    end = getTime
    mine1 = end - start

    start = getTime
    for (i <- 0 to 10000) { enc.decode(test) }
    end = getTime
    mine2 = end - start

    println("[base64 decode1]Slower %s times than apache".format(mine1 / apache))
    println("[base64 decode2]Slower %s times than apache".format(mine2 / apache))
    println("codec.Base64.decode     : " + mine1)
    println("codec.Base64.decodeFast : " + mine2)
    println("apache-commons          : " + apache)
  }
}