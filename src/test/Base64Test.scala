package test

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit._
import com.shc.codec._
import org.apache.commons.codec.binary.{Base64 => apache64}

class Base64Test extends AssertionsForJUnit {
  
  def comparing(key:String, value:String) = "%s:%s".format(key, value)
  
  @Test
  def encodeTest() {
    var test = comparing("a", " cd")
    //var expect = apache64.encodeBase64String(test.getBytes)
    var expect = Base64.encode(test)
    var actual = Base64.encodeSlow(test)
    assertEquals(expect, actual)
    
    test = comparing("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    expect = apache64.encodeBase64String(test.getBytes)
    actual = Base64.encode(test)
    assertEquals(expect, actual)
    
    test = "うa"
    expect = apache64.encodeBase64String(test.getBytes)
    actual = Base64.encode(test)
    val actual2 = Base64.encodeSlow(test)
    assertEquals(actual, actual2)
    assertEquals(expect, actual)
    
    test = "うa"
    val charset = "sjis"
    expect = apache64.encodeBase64String(test.getBytes(charset))
    actual = Base64.encode(test, charset)
    assertEquals(expect, actual)
  }
  
  @Test
  def decodeTest() {
    var test = "abcdefg"
    var encoded = Base64.encode(test)
    var decoded = Base64.decode(encoded)
    assertEquals(test, decoded)
  }
  
  @Test
  def squat10000times() {
    import scala.util.Random
    var test: String = null
    var encode: String = null
    var encodeFast: String = null
    var encodeApache: String = null
    var decode: String = null
    var decodeFast: String = null
    var decodeApache: String = null
    
    var encodeok: Boolean = false
    var decodeok: Boolean = false
    val result = Range(0, 10000).map(i => {
      val (keylen, passlen) = (Random.nextInt(200), Random.nextInt(200))
      test = comparing(Random.nextString(keylen), Random.nextString(passlen))
      encode = Base64.encode(test)
      encodeFast = Base64.encodeSlow(test)
      encodeApache = apache64.encodeBase64String(test.getBytes)
      decode = Base64.decode(encode)
      decodeFast = Base64.decodeSlow(encode)
      decodeApache = new String(apache64.decodeBase64(encode.getBytes))
      encodeok = encode == encodeApache & encodeFast == encodeApache
      decodeok = decode == decodeApache & decodeFast == decodeApache
      encodeok & decodeok
    })
    assertTrue(result.forall(_ == true))
  }
}