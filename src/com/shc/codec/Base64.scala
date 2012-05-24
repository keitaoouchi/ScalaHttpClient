package com.shc.codec

import java.lang.Byte.{ parseByte => str2byte }

/**Base64 codec Object providing static encode and decode method. 
 * Both encoding and decoding speed are equivalent to apache.commons.codec.
 * 
 * [Usage]
 * val encoded = Base64.encode("hello world")
 * assertEquals(encoded, "aGVsbG8gd29ybGQ=")
 * 
 */
object Base64 {

  protected val encTable = Array(
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/')

  protected val decTable = {
    val array = new Array[Byte](128)
    Range(0, 128).foreach(i => array(i) = -1)
    Range(0, 64).foreach(i => array(encTable(i)) = i.toByte)
    array
  }

  protected val BIT2MASK = 0x3 // 11
  protected val BIT4MASK = 0xf // 1111
  protected val BIT6MASK = 0x3f // 111111
  protected val BIT8MASK = 0xff // 11111111
  protected val BIT8MASK2TOP = 0xc0 // 11000000
  protected val BIT8MASK4TOP = 0xf0 // 11110000
  protected val BIT8MASK6TOP = 0xfc // 11111100
  protected val EQUALPAD = '='

  /**Base64 encoding method.
   * 
   * @param str        string to be encoded.
   * @param charset    charset of str
   * @return           encoded string
   */
  def encode(str: String, charset: String = "UTF-8"): String = {
    var bytes = str.getBytes(charset)
    val result = new StringBuilder()
    var bytesId = 0
    var restCount = bytes.length
    while (restCount >= 3) {
      // take 3bytes(characters) and integrate them as one Int.
      var byte1 = bytes(bytesId)
      bytesId += 1
      var byte2 = bytes(bytesId)
      bytesId += 1
      var byte3 = bytes(bytesId)
      bytesId += 1
      val char3asInt = this.integrate3bytesToInt(byte1, byte2, byte3)
      // take 4 * 6bit and encode each 6bit to base64 character.
      result.append(encTable( char3asInt >> 18))
      result.append(encTable((char3asInt >> 12) & BIT6MASK))
      result.append(encTable((char3asInt >> 6)  & BIT6MASK))
      result.append(encTable( char3asInt        & BIT6MASK))
      restCount -= 3
      //bytesId += 3
    }

    // As for relation between str.length and size of EQUALPAD,
    // see base64_decoding_memo
    // 
    // str.length = 3 * ("4 base64 characters") - {0 or 1 or 2}
    restCount match {
      case 1 => {
        // 1 character(8bit) as 1 int.
        val restBytes = bytes(bytesId) & BIT8MASK
        // take 6bit
        result.append(encTable(restBytes >> 2))
        // rest 2bit
        result.append(encTable((restBytes << 4) & BIT6MASK))
        result.append(EQUALPAD)
        result.append(EQUALPAD)
      }
      case 2 => {
        // 2 characters(16bit) as 1 int.
        val restBytes = 
          ((bytes(bytesId) & BIT8MASK) << 8) + (bytes(bytesId + 1) & BIT8MASK)
        // take 6bit ( 16 - 10)
        result.append(encTable(restBytes >> 10))
        // take 6bit
        result.append(encTable((restBytes >> 4) & BIT6MASK))
        // rest 2bit 
        result.append(encTable((restBytes << 2) & BIT6MASK))
        result.append(EQUALPAD)
      }
      case _ => null // case 0
    }
    result.toString
  }
  
  /**Base64 decoding method.
   * 
   * @param str         string to be decoded
   * @pamra charset     charset of decoded string.
   * @return            decoded string.
   */
  def decode(str: String, charset: String = "UTF-8"): String = {
    val strLength = str.length
    // branching tail pattern based on tail 2 characters
    val last2: Char = str(strLength - 2)
    val last3: Char = str(strLength - 1)
    // possible decoded size are 3 pattern. see base64_decoding_memo.
    val tailPattern =
      if (last3 == EQUALPAD) {
        if (last2 == EQUALPAD) 2 else 1
      } else 0
    val result = new Array[Byte](3 * strLength / 4 - tailPattern)
    
    //if tailPattern is zero, there is no EQUALPAD (and Zero Padding is also zero). 
    val processLength = if(tailPattern == 0) strLength else strLength - 4

    //each encoded characters represent 6bit.
    //4 6bit characters represent 3bytes of decoded string.
    var toBeDecoded = new Array[Char](4)
    var decoded: (Byte, Byte, Byte) = (0,0,0)
    var resultIndex = 0
    var processIndex = 0
    while(processIndex < processLength) {
      toBeDecoded(0) = str(processIndex)
      processIndex += 1
      toBeDecoded(1) = str(processIndex)
      processIndex += 1
      toBeDecoded(2) = str(processIndex)
      processIndex += 1
      toBeDecoded(3) = str(processIndex)
      processIndex += 1
      val decoded = this.decode24bit(toBeDecoded)
      
      result(resultIndex) = decoded._1
      resultIndex += 1
      result(resultIndex) = decoded._2
      resultIndex += 1
      result(resultIndex) = decoded._3
      resultIndex += 1
    }

    toBeDecoded(0) = str(strLength - 4)
    toBeDecoded(1) = str(strLength - 3)
    if(tailPattern == 2){
      result(resultIndex) = this.decode8bit(toBeDecoded)
    }else if(tailPattern == 1){
      toBeDecoded(2) = last2
      val lastBits = this.decode16bit(toBeDecoded)
      result(resultIndex) = lastBits._1
      resultIndex += 1
      result(resultIndex) = lastBits._2
    }
    new String(result, charset)
  }

  protected def decode8bit(chars: Array[Char]) = {
    ((decTable(chars(0)) << 2 & BIT8MASK6TOP) |     // --bbbbbb
     (decTable(chars(1)) >> 4 & BIT2MASK)).toByte   // --bb----
  }

  protected def decode16bit(chars: Array[Char]) = {
    (((decTable(chars(0)) << 2 & BIT8MASK6TOP) |    // --bbbbbb
      (decTable(chars(1)) >> 4 & BIT2MASK)).toByte, // --bb----
     ((decTable(chars(1)) << 4 & BIT8MASK4TOP) |    // ----bbbb
      (decTable(chars(2)) >> 2 & BIT4MASK)).toByte) // --bbbb--
  }

  protected def decode24bit(chars: Array[Char]) = {
    (((decTable(chars(0)) << 2 & BIT8MASK6TOP) |    // --bbbbbb
      (decTable(chars(1)) >> 4 & BIT2MASK)).toByte, // --bb----
     ((decTable(chars(1)) << 4 & BIT8MASK4TOP) |    // ----bbbb
      (decTable(chars(2)) >> 2 & BIT4MASK)).toByte, // --bbbb--
     ((decTable(chars(2)) << 6 & BIT8MASK2TOP) |    // ------bb
      (decTable(chars(3)))).toByte)                 // --bbbbbb
  }

  /**
   * http://en.wikibooks.org/wiki/Algorithm_Implementation/Miscellaneous/Base64
   */
  def encodeSlow(str: String, charset: String = "UTF-8"): String = {
    var bytes = str.getBytes(charset)
    var res = bytes.length % 3
    var padding = if (res > 0) 3 - res else 0
    val ints = 
      new scala.collection.mutable.ArrayBuffer[Int](bytes.length + padding)
    bytes.foreach(b => if (b < 0) ints.append(256 + b) else ints.append(b))
    for (i <- 0 until padding) ints.append(0)

    val unitsLength = ints.length
    var char3asInt = 0
    val result = new StringBuilder()
    for (i <- 0 until unitsLength by 3) {
      char3asInt = (ints(i) << 16) + (ints(i + 1) << 8) + ints(i + 2)
      result.append(encTable((char3asInt >> 18) & BIT6MASK))
      result.append(encTable((char3asInt >> 12) & BIT6MASK))
      result.append(encTable((char3asInt >> 6) & BIT6MASK))
      result.append(encTable(char3asInt & BIT6MASK))
    }
    if (padding != 0) result.dropRight(padding).padTo(result.length, "=").mkString
    else result.toString
  }

  def decodeSlow(str: String, charset: String = "UTF-8") = {
    val padding =
      if (str.endsWith("==")) 2
      else if (str.endsWith("=")) 1
      else 0
    // 'A' = base64encode(0) 0 = base64decode('A') 
    val paddingRemoved = str.dropRight(padding) + "A" * padding
    val strLength = paddingRemoved.length
    var char4asInt: Int = 0
    val result = new scala.collection.mutable.ArrayBuffer[Byte]()
    for (i <- 0 until strLength by 4) {
      char4asInt =
        (decTable(paddingRemoved(i)) << 18) +
        (decTable(paddingRemoved(i + 1)) << 12) +
        (decTable(paddingRemoved(i + 2)) << 6) +
         decTable(paddingRemoved(i + 3))

      result.append(((char4asInt >> 16) & BIT8MASK).toByte)
      result.append(((char4asInt >> 8) & BIT8MASK).toByte)
      result.append((char4asInt & BIT8MASK).toByte)
    }
    if (padding > 0) new String(result.dropRight(padding).toArray, charset)
    else new String(result.toArray, charset)
  }

  def encodeSlowest(str: String, charset: String = "UTF-8"): String = {
    val bytes = str.getBytes(charset)
    var bitStr = new StringBuilder()
    bytes.map(b => {
      val int = if (b < 0) 256 + b else b
      bitStr.append(bitString(int, 8))
    })
    val bits = filledArray(bitStr.toString, 6, '0')
    var encoded = bits.map(s => encTable(str2byte(s, 2)))
    val res = encoded.length % 4
    if (res == 0) encoded.mkString else encoded.mkString + "=" * (4 - res)
  }

  def decodeSlowest(str: String, charset: String = "UTF-8") = {
    var paddingRemoved = str.reverse.dropWhile(_ == EQUALPAD).reverse
    val indexArray = paddingRemoved.map(c => encTable.indexOf(c))
    val sixBits = indexArray.map(i => bitString(i, 6)).mkString
    paddingRemoved = sixBits.dropRight(sixBits.length % 8)
    val eightBits = strSplitWithStep(paddingRemoved, 8)
    val result = eightBits.map(bin => java.lang.Integer.parseInt(bin, 2).toByte)
    new String(result, charset)
  }
  
  /**Take 3bytes(characters) and integrate them as one Int.
   * 
   */
  protected def integrate3bytesToInt(byte1:Byte, byte2:Byte, byte3:Byte):Int = {
    val b1 = if(byte1 < 0) 256 + byte1 else byte1
    val b2 = if(byte2 < 0) 256 + byte2 else byte2
    val b3 = if(byte3 < 0) 256 + byte3 else byte3
    ((b1     & BIT8MASK) << 16) +
    ((b2 & BIT8MASK) << 8 ) +
     (b3 & BIT8MASK)
  }

  /**
   * ("0" * (width - 1) + int.toBinaryString).takeRight(width)
   */
  protected def bitString(int: Int, width: Byte) = {
    val str = new StringBuilder()
    Range(0, width).foreach(i => {
      str.append((int >> (width - i - 1)) & 1)
    })
    str.toString
  }

  protected def filledArray(str: String, step: Int, fillWith: Char) = {
    val array = strSplitWithStep(str, step)
    val lastIndex = array.length - 1
    array(lastIndex) = array(lastIndex).padTo(step, fillWith)
    array
  }

  protected def strSplitWithStep(str: String, step: Int) = {
    val strSize = str.length
    val arraySize = 
      if (strSize % step == 0) strSize / step 
      else strSize / step + 1
    val result = new Array[String](arraySize)
    val lastIndex = arraySize - 1
    for (i <- 0 to lastIndex) result(i) = str.slice(i * step, (i + 1) * step)
    result
  }
}
