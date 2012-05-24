package test

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit._
import com.shc.http._
import com.shc.codec._

class EncodeTest extends AssertionsForJUnit {
  
  @Test
  def squat10000times() {
    import scala.util.Random
    val enc = URLCodec
    var test: String = null
    var encoded: String = null
    var encodedWithJava: String = null
    var decoded: String = null
    var actual : String = null
    var temp:Boolean = false
    val result = Range(0, 10000).map(i => {
      test = Random.nextString(Random.nextInt(100))
      encoded = URLCodec.encode(test).map(_.toChar).mkString
      encodedWithJava = java.net.URLEncoder.encode(test)
      decoded = URLCodec.decode(encoded)
      temp = encoded == encodedWithJava & decoded == test
      if(temp == false) {
        println("orz...")
        println(" encodedWithMine: " + encoded)
        println(" encodedWithJava: " + encodedWithJava)
        println(" decoded equals to test: " + (decoded == test))
      }
      temp
    })
    assertTrue(result.forall(_ == true))
  }
  
  @Test
  def encodeTest() {
    val enc = URLCodec
    var teststr = "-._~ !'()*"
    var encoded = enc.encode(teststr)
    val expectNG = teststr.map(c => "%c%02X".format('%', c.toByte)).mkString
    assertEquals("%2D%2E%5F%7E%20%21%27%28%29%2A", expectNG)
    //default unreserved special characters are same with java.net.URLEncoder
    assertEquals("-._%7E+%21%27%28%29*", encoded.map(_.toChar).mkString)
    
    teststr = " "
    encoded = enc.encode(teststr)
    //default substitude map is ' ' -> '+'
    assertEquals("+", encoded.map(_.toChar).mkString)
    encoded = enc.encode(teststr, unsafe = " ")
    assertEquals("%20", encoded.map(_.toChar).mkString)
    encoded = enc.encode(teststr, safe = " ", unsafe = " ", encodemap = Map(' ' -> '?'))
    //priority is unsafe > encodemap > safe
    assertEquals("%20", encoded.map(_.toChar).mkString)
    
    teststr = "0123abcdABCD"+enc.javaEnc
    encoded = enc.encode(teststr)
    assertEquals(teststr, encoded.map(_.toChar).mkString)
    encoded = enc.encode(teststr, unsafe = "0aA*")
    assertEquals("%30123%61bcd%41BCD" + "-._%2A", encoded.map(_.toChar).mkString)
    
    teststr = "a"
    encoded = enc.encode(teststr, unsafe="a", safe="a")
    assertEquals("%c%02X".format('%', 'a'.toByte), encoded.map(_.toChar).mkString)
    encoded = enc.encode(teststr, safe="a", encodemap=Map('a' -> 'b'))
    assertEquals("b", encoded.map(_.toChar).mkString)
    
    teststr = "tR3+Ty81lMeYAr/Fid0kMTYa/WM="
    encoded = enc.encode(teststr)
    assertEquals("tR3%2BTy81lMeYAr%2FFid0kMTYa%2FWM%3D", encoded.map(_.toChar).mkString)
    
    teststr = "hello\r\n\0w\torld"
    encoded = enc.encode(teststr)
    assertEquals(java.net.URLEncoder.encode(teststr), encoded.map(_.toChar).mkString)
  }
  
  @Test
  def decodeTest() {
    val enc = URLCodec
    
    var test = "a"
    var encoded = enc.encode(test, charset = "utf8")
    var decoded = enc.decode(encoded.map(_.toChar).mkString)
    assertEquals(test, decoded)
    
    test = "a@"
    encoded = enc.encode(test, charset = "utf8")
    decoded = enc.decode(encoded.map(_.toChar).mkString)
    assertEquals(test, decoded)
    
    test = "a@b"
    encoded = enc.encode(test, charset = "utf8")
    decoded = enc.decode(encoded.map(_.toChar).mkString)
    assertEquals(test, decoded)
    
    test = "1234!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~abcd"
    encoded = enc.encode(test, charset = "utf8")
    decoded = enc.decode(encoded.map(_.toChar).mkString)
    assertEquals(test, decoded)
    
    test = " "
    encoded = enc.encode(test)
    decoded = enc.decode(encoded.map(_.toChar).mkString)
    assertEquals("+", encoded.map(_.toChar).mkString)
    assertEquals(test, decoded)
    
    test = " +"
    encoded = enc.encode(test)
    decoded = enc.decode(encoded.map(_.toChar).mkString)
    assertEquals("+%2B", encoded.map(_.toChar).mkString)
    assertEquals(test, decoded)
    
    test = "ウィキペディア"
    encoded = enc.encode(test, charset = "sjis")
    assertEquals("%83E%83B%83L%83y%83f%83B%83A", encoded.map(_.toChar).mkString)
    decoded = enc.decode(encoded.map(_.toChar).mkString, charset = "sjis")
    assertEquals(test, decoded)
    
  }
}