package test

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit._

import com.shc.http._

import java.io.{ File, FileOutputStream, OutputStream }

class MockRequest(override val url: String,
              	  override val headers: scala.collection.Map[String, String])
  extends Request(url, headers) {

  override def methodHandler(method: String,
                    data: scala.collection.Map[String, String],
                    files: scala.collection.Map[String, String]) = null

  override def buildQuery(params: scala.collection.Map[String, String]) = super.buildQuery(params)

  override def setHeader(headers: scala.collection.Map[String, String],
                         conn: java.net.HttpURLConnection) = super.setHeader(headers, conn)

  override def file2Bytes(filepath: String) = super.file2Bytes(filepath)

  override def buildMsgBody(boundary: String,
                            data: scala.collection.Map[String, String],
                            files: scala.collection.Map[String, String]) = super.buildMsgBody(boundary, data, files)

  override def buildSubMsgHeader(boundary: String,
                                 key: String,
                                 filename: String = null) = super.buildSubMsgHeader(boundary, key, filename)

  override def buildMultipartMsg(boundary: String,
                                 data: scala.collection.Map[String, String],
                                 files: scala.collection.Map[String, String]) = super.buildMultipartMsg(boundary, data, files)

  override def calculateContentsLength(boundary: String,
                                       data: scala.collection.Map[String, String],
                                       files: scala.collection.Map[String, String]) = super.calculateContentsLength(boundary, data, files)

  override def buildContentType(boundary: String,
                                data: scala.collection.Map[String, String],
                                files: scala.collection.Map[String, String]) = super.buildContentType(boundary, data, files)

  override def writeBytes(stream: OutputStream,
                          boundary: String,
                          data: scala.collection.Map[String, String],
                          files: scala.collection.Map[String, String]) = super.writeBytes(stream, boundary, data, files)
}

class ConnectableTest extends AssertionsForJUnit {

  var req: MockRequest = _

  @Before
  def init {
    val url = "http://www.ghostreet.net"
    req = new MockRequest(url, Map())
  }

  /* Unreserved characters of RFC3986 are (ALPHA, DIGIT, "-", ".", "_", "~") 
   * Reserved characters are (";", "/", "?", ":", "@", "&", "=", "+", "$", ",")
   * 
   * buildQuery internally uses java.net.URLEncoder.encode, so the true unreserved
   * characters are ("-", "_", ".", "*") only.
   */
  @Test
  def buildQueryTest {
    val params1 = Map("test1" -> "value1", "test2" -> "value2")
    val params2 = Map("space" -> " ")
    val params3 = Map("reserved" -> "!#$%&'()=~^|{[}]`@:;+<,>?/")
    val params4 = Map("unreserved" -> "-_.*")
    val params5 = Map("urlencode" -> "abcd1234!#$%&'()=~|{}*+`_?><,./ @][:;" )
    assertEquals(req.buildQuery(params1), "test1=value1&test2=value2")
    assertEquals(req.buildQuery(params2), "space=+")
    var expect = params3("reserved").map(c => c.toByte).map(b => "%" + "%02X".format(b)).mkString
    assertEquals(req.buildQuery(params3), "reserved=%s".format(expect))
    expect = params4("unreserved")
    assertEquals(req.buildQuery(params4), "unreserved=%s".format(expect))
    expect = java.net.URLEncoder.encode(params5("urlencode"))
    assertEquals(req.buildQuery(params5), "urlencode=%s".format(expect))
  }

  @Test
  def file2ByteTest {
    val src1 = "./test_data/test.txt"
    assertEquals(req.file2Bytes(src1).map(_.toChar).mkString, "123456789\n987654321")
  }

  @Test
  def contentTypeTest {
    var contentType: String = null
    contentType = req.buildContentType("***", Map(), Map())
    assertEquals(contentType, "")
    contentType = req.buildContentType("***", Map("k" -> "v"), Map())
    assertEquals(contentType, "application/x-www-form-urlencoded")
    contentType = req.buildContentType("***", Map(), Map("k" -> "v"))
    assertEquals(contentType, "multipart/form-data; boundary=***")
  }

  @Test
  def contentsLengthTest {
    val src = "./test_data/test.txt"
    val data = Map("name" -> "value")
    val files = Map("file" -> src)
    val msgBody = req.buildMultipartMsg("***", data, files)
    val calculated = req.calculateContentsLength("***", data, files)
    assertEquals(msgBody.map(_.toByte).length, calculated)
    assertEquals(req.buildMultipartMsg("***", Map(), Map()).map(_.toByte).length, 0)
  }

  @Test
  def writingToStreamTest {
    val readFrom = "./test_data/test.txt"
    val writeTo = "./test_data/test2write.txt"
    var stream = new FileOutputStream(new File(writeTo))
    val data = Map("dataname" -> "datavalue")
    val files = Map("filename" -> readFrom)
    var writtenLength = req.writeBytes(stream, "***", data, files)
    stream.close
    var calculated = req.calculateContentsLength("***", data, files)
    assertEquals(writtenLength, calculated)
    assertEquals(writtenLength, new File(writeTo).length)

    stream = new FileOutputStream(new File(writeTo))
    writtenLength = req.writeBytes(stream, "***", data, Map())
    calculated = req.calculateContentsLength("***", data, Map())
    assertEquals(writtenLength, calculated)
    assertEquals(writtenLength, new File(writeTo).length)
    stream.close

    stream = new FileOutputStream(new File(writeTo))
    writtenLength = req.writeBytes(stream, "***", Map(), files)
    calculated = req.calculateContentsLength("***", Map(), files)
    assertEquals(writtenLength, calculated)
    assertEquals(writtenLength, new File(writeTo).length)
    stream.close

    stream = new FileOutputStream(new File(writeTo))
    calculated = new File(writeTo).length.toInt
    writtenLength = req.writeBytes(stream, "***", Map(), Map())
    assertEquals(writtenLength, 0)
    assertEquals(writtenLength, calculated)
    stream.close
  }

  @Test
  def multipartHeaderTest {
    val src1 = "./test_data/test.txt"
    val ln = "\r\n"
    val msg_with_file = req.buildSubMsgHeader("***", "name", "test.txt")
    val template1 =
      "--***" + ln +
        "Content-Disposition: form-data; name=\"name\"; filename=\"test.txt\"" + ln +
        "Content-type: text/plain; charset=utf-8" + ln +
        ln

    val msg_without_file = req.buildSubMsgHeader("***", "name")
    val template2 =
      "--***" + ln +
        "Content-Disposition: form-data; name=\"name\"" + ln +
        ln
    assertEquals(msg_with_file, template1)
    assertEquals(msg_without_file, template2)
  }

  @Test
  def multipartMessageTest {
    val src = "./test_data/test.txt"
    val data = Map("name" -> "value")
    val files = Map("test-file" -> src)
    val file2byte = req.file2Bytes(src)
    val ln = "\r\n"

    val msg_without = req.buildMultipartMsg("***", Map(), Map())
    val msg_with_data_file = req.buildMultipartMsg("***", data, files)
    val msg_with_data = req.buildMultipartMsg("***", data, Map())
    val msg_with_file = req.buildMultipartMsg("***", Map(), files)

    val header_with_file = req.buildSubMsgHeader("***", "test-file", "test.txt")
    val header_without_file = req.buildSubMsgHeader("***", "name")

    val expect_without = ""
    val expect_with_data_file =
      header_without_file + //this includes tail ln
        "value" + ln +
        header_with_file +
        file2byte.map(_.toChar).mkString + ln +
        "--***--" + ln
    val expect_with_data =
      header_without_file +
        data("name") + ln
    val expect_with_file =
      header_with_file +
        file2byte.map(_.toChar).mkString + ln +
        "--***--" + ln

    assertEquals(msg_without.map(_.toChar).mkString, expect_without)
    assertEquals(msg_with_data_file.map(_.toChar).mkString, expect_with_data_file)
    assertEquals(msg_with_data.map(_.toChar).mkString, expect_with_data)
    assertEquals(msg_with_file.map(_.toChar).mkString, expect_with_file)
  }

  @Test
  def mimeTest {
    val tar = "test.tar"
    val csv = "test.csv"
    val css = "test.css"
    val pdf = "test.pdf"
    val tar_t = req.checkMimeType(tar)
    val csv_t = req.checkMimeType(csv)
    val css_t = req.checkMimeType(css)
    val pdf_t = req.checkMimeType(pdf)
    assertEquals(tar_t, "application/x-tar")
    assertEquals(csv_t, "text/csv")
    assertEquals(css_t, "text/css")
    assertEquals(pdf_t, "application/pdf")
  }

}