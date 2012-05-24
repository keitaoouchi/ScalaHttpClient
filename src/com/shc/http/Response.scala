package com.shc.http

import java.net.{ URL, HttpURLConnection, CookieManager, CookieHandler }
import java.util.zip.{ GZIPInputStream, InflaterInputStream }
import java.io.BufferedInputStream
import scala.collection.JavaConversions._
import scala.collection.mutable.{ Map => MutableMap}
import com.shc.readwrite.ReadWrite

/** Ease of retrieving response message! 
 * 
 *  [Overview]
 *   Easing of handling http response is our aim. And this class enables you to
 *   fetch basic information from http response.
 *   StatusCode, URL, Headers are automatically stored to the public fields,
 *   and based on requestMethod response body message will be automatically
 *   stored to contents fields wrapped with Option.
 *   HttpURLConnection will be automatically disconnected, so you don't have to
 *   care about connection or stream resources.
 *
 *   [Basic usage]
 *   val response = Request.get("http://www.ghosreet.net")
 *   if(response != None){
 *     println(response.get.message)
 *     response.get.cookies.foreach(println)
 *     val store_path = "./test_data/test.html"
 *     val bool = response.get.store(store_path)
 *     println(bool)
 *   }else{
 *     println(statusCode)
 *     println("[Fail] Something wrong with your request!"
 *   }
 *  
 *   [How to access protected resources]
 *   val url = "http://some.protected.resources"
 *   var response = Request.get(url)
 *   if(response != None & response.get.statusCode == 401){
 *     val credentials = ("username", "password")
 *     response = Request.get(url, resp.auth(credentails._1, credentials._2))
 *     println(response.get.statusCode)
 *   }
 */
class Response(connection: HttpURLConnection) {

  var statusCode: Int = 500
  var url: URL = _
  var requestMethod: String = _
  var headers: scala.collection.Map[String, String] = _
  var cookieManager: CookieManager = CookieHandler.getDefault.asInstanceOf[CookieManager]
  var contents: Array[Byte] = _
  var charset: String = _
  var message: String = _

  if (connection != null) {
    this.statusCode = connection.getResponseCode
    this.url = connection.getURL
    this.requestMethod = connection.getRequestMethod
    this.headers = this.getHeaders(connection)
    this.contents = this.getContents(connection)
    val contentType = connection.getContentType
    if(contentType.startsWith("text") && contentType.contains("charset=")){
      this.charset = contentType.split("charset=").last
      this.message = new String(this.contents.toArray, this.charset)
    }else{
      this.charset = ""
      this.message = ""
    }
    /*
    this.message = {
      if (connection.getContentType.startsWith("text")) new String(this.contents.toArray)
      else ""
    }
    */
    connection.disconnect
  }

  def cookies: List[java.net.HttpCookie] = {
    if (cookieManager != null) cookieManager.getCookieStore.getCookies.toList
    else List()
  }

  /** store the contents(byte array) in path.
   * 
   */
  def store(path: String): Boolean = {
    val writer = ReadWrite.writeIter(path)
    if(writer.open) {
      this.contents.foreach(data => writer.write(data))
      val result = writer.save
      writer.close
      result
    }else false
  }

  private def getHeaders(conn: HttpURLConnection):Map[String, String] = {
    val result = MutableMap[String, String]()
    val items = conn.getHeaderFields
    for(item <- items) {
      result(item._1) = item._2.map(str => str.replace(", ", " && ")).mkString(" && ")
    }
    result.toMap
  }
  
  def auth(username: String, password: String):Option[String] = {
    val authheader = this.headers.get("WWW-Authenticate")
    if(authheader != None) {
      val headerVal = authheader.get
      val sepPosition = headerVal.indexOf(" ")
      val method = headerVal.slice(0, sepPosition)
      val infoStr = headerVal.slice(sepPosition + 1, authheader.get.length)
      val result = MutableMap[String, String]()
      val splittedInfo = infoStr.split(" && ")
      for(info <- splittedInfo) {
        val keyval = info.split("=")
        result(keyval(0)) = keyval(1).replace("\"", "")
      }
      val authvalue = EasyAuth.auth(method, this.requestMethod, this.url.getPath, username, password, result)
      Some(authvalue)
    }else {
      None
    }
  }

  private def getContents(conn: HttpURLConnection): Array[Byte] = {
    if (this.statusCode != 200) {
      Array[Byte]()
    } else {
      val compression_type = conn.getContentEncoding
      val is = compression_type match {
        case "gzip"    => new GZIPInputStream(conn.getInputStream)
        case "deflate" => new InflaterInputStream(conn.getInputStream)
        case _         => conn.getInputStream
      }
      val bis = new BufferedInputStream(is)
      val result = ReadWrite.read(bis)
      bis.close
      result.toArray
    }
  }

}