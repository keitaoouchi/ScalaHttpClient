package com.shc.codec

import java.lang.Byte.{ parseByte => str2byte }
import java.lang.Integer.{ parseInt => str2int }
import java.lang.Character

/** This object provide percent encoding and decoding methods.
 *  In Java standard library(java.net.URLEncoder), users cannot
 *  change reserved and unreserved characters, and ' ' characters
 *  always substitute with '+' and cannnot change its behavior.
 *  In this Object's encode/decode method, users can change reserved,
 *  unreserved, map of substitute characters with attributes.
 *  
 *  [Encoding]
 *  var encoded = URLCodec.encode(" ")
 *  assertEquals(encoded, "+")
 *  encoded = URLCodec.encode(" ", unsafe = " ")
 *  assertEquals(encoded, "%20")
 *  encoded = URLCodec.encode("~")
 *  assertEquals(encoded, "%7E")
 *  encoded = URLCodec.encode("~", safe = "~")
 *  assertEquals(encoded, "~")
 *  encoded = URLCodec.encode(" ", encodemap = Map(' ' -> '-')
 *  assertEquals(encoded, "-")
 *  
 */
object URLCodec {

  protected val PERCENT = '%'
  protected val BIT4MASK = 0xf // 1111

  // from RFC2396
  val rfc2396 = "!'()*-._~"
  // from RFC3986
  val rfc3986 = "-._~"
  // from java.net.URLEncoder
  val javaEnc = "-._*"

  protected val emptyEncoding = Map(' ' -> '+')

  protected val emptyDecoding = Map('+' -> ' ')

  protected val defaultUnreserved = {
    val bitset = new scala.collection.mutable.BitSet(256)
    Range('0', '9' + 1).foreach(i => bitset.update(i, true))
    Range('A', 'Z' + 1).foreach(i => bitset.update(i, true))
    Range('a', 'z' + 1).foreach(i => bitset.update(i, true))
    this.javaEnc.foreach(c => bitset.update(c, true))
    bitset.update(' ', true)
    bitset
  }
  
  protected val encodeHeadHexTable:Array[Char] = {
    val array = new Array[Char](256) // 0 to 255
    Range(0, 256).foreach(i => array(i) = Character.toUpperCase(Character.forDigit((i >> 4) & BIT4MASK, 16)))
    array
  }
  
  protected val encodeTailHexTable:Array[Char] = {
    val array = new Array[Char](256) // 0 to 255
    Range(0, 256).foreach(i => array(i) = Character.toUpperCase(Character.forDigit(i & BIT4MASK, 16)))
    array
  }
  
  protected val decodeHeadHexTable:Array[Int] = {
    val array = new Array[Int](128)
    Range('0', '9' + 1).foreach(c => array(c) = (Character.digit(c, 16) << 4))
    Range('A', 'G').foreach(c => array(c) = (Character.digit(c, 16) << 4))
    array
  }
  
  protected val decodeTailHexTable:Array[Int] = {
    val array = new Array[Int](128)
    Range('0', '9' + 1).foreach(c => array(c) = Character.digit(c, 16))
    Range('A', 'G').foreach(c => array(c) = Character.digit(c, 16))
    array
  }
  
  protected def encode2HexString(int: Int): (Char, Char) = {
    (encodeHeadHexTable(int),encodeTailHexTable(int))
  }
  
  protected def decodeHexEncoded(head: Char, tail: Char): Byte = {
    (decodeHeadHexTable(head) + decodeTailHexTable(tail)).toByte
  }

  /**Percent encoding method.
   * Default unreserved characters are same with java.net.URLEncoder.
   * ' ' also substitute with '+'.
   * You can change them with safe, unsafe, encodemap arguments.
   * Priority is unsafe > encodemap > safe.
   * 
   * @param  str       String to encode
   * @param  charset   Charset of str
   * @param  safe      Characters to be unreserved
   * @param  unsafe    Characters to be reserved
   * @param  encodemap Map of Characters to be unreserved and substitute with
   * 
   * @return String
   */
  def encode(str: String,
             charset: String = "UTF-8",
             safe: String = null,
             unsafe: String = null,
             encodemap: Map[Char, Char] = null): String = {
    val strBytes = str.getBytes(charset)

    // create temporaly safe characters mapping.
    val tempSafe = this.defaultUnreserved.clone
    if (safe != null) safe.foreach(c => tempSafe.update(c, true))
    val replaceMap = if (encodemap != null) {
      encodemap.keysIterator.foreach(c => tempSafe.update(c, true))
      encodemap
    } else {
      this.emptyEncoding
    }
    if (unsafe != null) unsafe.foreach(c => tempSafe.update(c, false))

    val result = new scala.collection.mutable.StringBuilder()
    var int = 0
    var encoded: (Char, Char) = null
    strBytes.foreach(b => {
      int = if (b < 0) 256 + b else b
      if (tempSafe(int)) {
        result.append(replaceMap.getOrElse(int.toChar, int.toChar))
      } else {
        result.append(PERCENT)
        encoded = encode2HexString(int)
        result.append(encoded._1)
        result.append(encoded._2)
      }
    })
    result.toString
  }
  
  /**Percent decoding method.
   * Default substitute character is same with java.net.URLEncoder.('+' to ' ')
   * You can change this with decodemap argument.
   * 
   * @param  str       String to decode
   * @param  charset   Charset of returning string.
   * @param  decodemap Map of Characters to be substituted and substitute with.
   * 
   * @return String
   */
  def decode(str: String,
             charset: String = "UTF-8",
             decodemap: Map[Char, Char] = null) = {
    val replaceMap = if (decodemap == null) this.emptyDecoding else decodemap
    val strSize = str.length
    // ArrayBuffer and its append method too much cost!
    val result = new Array[Byte](strSize)
    var i, j = 0
    var head, tail: Char = '0'
    while (i < strSize) {
      head = str(i)
      if (head == PERCENT) {
        i += 1
        head = str(i)
        i += 1
        tail = str(i)
        result(j) = decodeHexEncoded(head, tail)
      } else { 
        result(j) = replaceMap.getOrElse(head, head).toByte
      }
      i += 1
      j += 1
    }
    new String(result.slice(0, j), charset)
  }

  def decodeSlow(str: String,
                 charset: String = "UTF-8",
                 decodemap: scala.collection.Map[Char, Char] = this.emptyDecoding) = {
    val regexp = "%[0-9A-F]{2}?".r
    val found = regexp.findAllIn(str)
    var last = str
    var head: String = null
    var splitted: Array[String] = null
    val result = new scala.collection.mutable.ArrayBuffer[Byte]()
    for (s <- found) {
      splitted = last.split(s, 2)
      if (splitted.length > 0) {
        last = splitted.last
        head = splitted.head
        if (head != "" & decodemap != null) decodemap.keysIterator.foreach(b => head = head.replace(b.toChar, decodemap(b)))
        result.appendAll(head.getBytes(charset))
      }
      result.append(this.percentDecode(s))
    }
    if (last != "" & decodemap != null) decodemap.keysIterator.foreach(b => last = last.replace(b.toChar, decodemap(b)))
    result.appendAll(last.getBytes(charset))
    new String(result.toArray, charset)
  }

  protected def percentEncode(int: Int): String = "%02X".format(int)

  protected def percentDecode(percentStr: String): Byte = {
    str2int(percentStr.slice(1, 3), 16).toByte
  }

  protected def removeEncoded(encoded: String): String = {
    val regexp = "%[0-9A-F]{2}?".r
    val found = regexp.findAllIn(encoded)
    var last = encoded
    var splitted: Array[String] = null
    val result = new scala.collection.mutable.StringBuilder()
    for (s <- found) {
      splitted = last.split(s, 2)
      if (splitted.length > 0) {
        last = splitted.last
        if (splitted.head != "") result.append(splitted.head)
      }
    }
    result.toString
  }
}