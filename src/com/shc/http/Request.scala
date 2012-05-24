package com.shc.http

import java.net.{ URL, HttpURLConnection }
import java.net.{ CookieManager, CookieHandler, CookiePolicy, CookieStore }
import java.io.OutputStream
import javax.activation.{ MimetypesFileTypeMap => MimeMap }
import scala.collection.mutable.{ Queue, ArrayBuffer }
import com.shc.codec.URLCodec.{encode => urlEncode}
import com.shc.readwrite.{ BytesReader, ReadWrite }

/** This Object provides static method to build and send http request.
 * 
 *  [Overview]
 *  This object allows you to ease of building http request with queries,
 *  headers, authentication data, files to upload.
 *  Returned http response will be requests.Response instance object.
 *
 *  [available methods]
 *  Request.head
 *  Request.get
 *  Request.post
 *
 *  [Basic Use]
 *  >>>1 authentication
 *   val url = "http://somewhere.else.com"
 *   var resp = Request.get(url)
 *   if(resp != None && resp.get.statusCode == 200){
 *     println(resp.get.message) // if content-type starts with "text/" else ""
 *   }else if(resp.get.statusCode == 401){
 *     println(resp.get.headers.getOrElse("WWW-Authenticate"))
 *     val auth = ("key", "pass")
 *     resp = Request.get(url, auth = auth)
 *     println(resp.get.statusCode == 200)
 *   }
 *
 *  >>>2 Uploading files
 *   val files = Map("key" -> "/somewhere/your/file/exist")
 *   resp = Request.post("http://somewhere.else.com", files = files)
 *
 *  >>>3 Uploading message with data.
 *   resp = Request.post(url, Map("key" -> "value"), files = files)
 *
 *  >>>4 download anything
 *   resp = Request.get(url + "/sometext.txt")
 *   if(resp != None){
 *     val store_path = "/somewehre/you/can/write/anything"
 *     val bool = resp.get.store(store_path)
 *     println(bool)
 *   }
 */
object Request {

  def apply(url: String,
            headers: scala.collection.Map[String, String] = Map()) = {
    new Request(url, headers)
  }

  /** Ease of get request!
   * 
   *  @param  url            url to access excluding query strings
   *  @param  queries        url + "?" + key1=value1 + "&" + ...
   *  @param  headers        header parameters
   *  @param  cookie         some coockie manager.
   *  @param  auth           authentication credential
   *  @return                request.Response object instance.
   */
  def get(url: String,
          queries: scala.collection.Map[String, String] = Map(),
          timeout: Int = 60000,
          headers: scala.collection.Map[String, String] = Map(),
          cookie: CookieManager = null,
          auth: String = null): Option[Response] = {
    new Request(url, headers).send("GET", queries, timeout, cookie, auth, Map())
  }

  /** Ease of post request!
   * 
   *  @param  url            url to access excluding query strings
   *  @param  queries        queries are written in message body as parts of 
   *                         multipart message or www-form-urlencoded format.
   *  @param  headers        header parameters
   *  @param  cookie         some cookie manager.
   *  @param  auth           authentication credential
   *  @return                request.Response object instance.
   */
  def post(url: String,
           queries: scala.collection.Map[String, String] = Map(),
           timeout: Int = 60000,
           headers: scala.collection.Map[String, String] = Map(),
           cookie: CookieManager = null,
           auth: String = null,
           files: scala.collection.Map[String, String] = Map()): Option[Response] = {
    new Request(url, headers).send("POST", queries, timeout, cookie, auth, files)
  }


  /** Same with get
   * 
   */
  def head(url: String,
           queries: scala.collection.Map[String, String] = Map(),
           timeout: Int = 60000,
           headers: scala.collection.Map[String, String] = Map(),
           cookie: CookieManager = null,
           auth: String = null): Option[Response] = {
    new Request(url, headers).send("HEAD", queries, timeout, cookie, auth, Map())
  }

}

class Request(val url: String,
              val headers: scala.collection.Map[String, String]){
  
  var cookieManager: CookieManager = CookieHandler.getDefault.asInstanceOf[CookieManager]

  protected def methodHandler(method: String,
                              queries: scala.collection.Map[String, String],
                              files: scala.collection.Map[String, String]): HttpURLConnection = {
    method match {
      case "GET" | "HEAD" => this.requestGetHead(method, queries)
      case "POST"         => this.requestPost(queries, files)
    }
  }

  //GET & HEAD request
  protected def requestGetHead(method: String,
                               queries: scala.collection.Map[String, String]) = {
    val reqUrl = if (!queries.isEmpty) "%s?%s".format(url,this.buildQuery(queries))
                 else url
    val conn = new URL(reqUrl).openConnection.asInstanceOf[HttpURLConnection]
    conn.setRequestMethod(method)
    if (!headers.isEmpty) setHeader(headers, conn)
    conn
  }

  //POST request
  protected def requestPost(queries: scala.collection.Map[String, String],
                            files: scala.collection.Map[String, String]) = {
    val boundary = java.util.UUID.randomUUID.toString
    val contentTypeHeader = this.buildContentType(boundary, queries, files)
    val contentsLength = this.calculateContentsLength(boundary, queries, files)
    val conn = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    if (!headers.isEmpty) setHeader(headers, conn)
    if(contentsLength != 0) {
	    conn.setRequestMethod("POST")
	    conn.setRequestProperty("Connection", "Keep-Alive")
	    conn.setRequestProperty("Content-Length", contentsLength.toString)
	    conn.setRequestProperty("Content-Type", contentTypeHeader)
	    conn.setUseCaches(false)
	    conn.setDoInput(true)
	    conn.setDoOutput(true)
	    val dos = conn.getOutputStream
	    this.writeBytes(dos, boundary, queries, files)
	    dos.flush
	    dos.close
    }else {
      conn.setRequestMethod("GET")
    }
    conn
  }
  
    /** Mime Type Checker.
   * 
   */
  def checkMimeType(filename: String) = {
    if (filename != null) {
      val src = "./META-INF/mime.types"
      val mime =
        if (new java.io.File(src).exists) new MimeMap(src) else new MimeMap
      mime.getContentType(filename)
    } else {
      "text/plain; charset=utf-8"
    }
  }

  /** Sending request
   *  
   *  @param  queries    query strings with (key -> value)
   *  @param  cookie     some cookieManager
   *  @param  auth       some authentication credential
   *  @param  files      some files to upload
   */
  def send(method: String,
           queries: scala.collection.Map[String, String] = Map(),
           timeout: Int = 60000,
           cookie: CookieManager = null,
           auth: String = null,
           files: scala.collection.Map[String, String] = Map()): Option[Response] = {
    val connecter = this.buildConnection(method, queries, cookie, auth, files)
    connecter.setConnectTimeout(timeout)
    connecter.setReadTimeout(timeout)
    try {
      connecter.connect
      Some(new Response(connecter))
    } catch {
      case e: Exception => connecter.disconnect; None
    }
  }

  protected def buildConnection(method: String,
                                queries: scala.collection.Map[String, String],
                                cookie: CookieManager,
                                auth: String,
                                files: scala.collection.Map[String, String]) = {
    val connecter = this.methodHandler(method, queries, files)
    if (cookie != null) {
      this.cookieManager = cookie
    } else if (this.cookieManager == null) {
      this.cookieManager = new CookieManager
      this.cookieManager.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER)
    }
    CookieHandler.setDefault(this.cookieManager)
    if (auth != null) {
      connecter.setRequestProperty("Authorization", auth)
    }
    connecter
  }

  // set http headers.
  protected def setHeader(headers: scala.collection.Map[String, String],
                          conn: HttpURLConnection) = {
    headers.iterator.foreach { header =>
      conn.setRequestProperty(header._1, header._2)
    }
  }

  // read the file contents as byte string.
  protected def file2Bytes(filepath: String): Array[Byte] = {
    val file = new java.io.File(filepath)
    val fis = new java.io.FileInputStream(file)
    val array = new Array[Byte](file.length.toInt)
    fis.read(array)
    fis.close
    array
  }

  // for test.
  protected def buildMsgBody(boundary: String,
                             queries: scala.collection.Map[String, String],
                             files: scala.collection.Map[String, String]) = {
    if (!files.isEmpty) {
      this.buildMultipartMsg(boundary, queries, files).map(_.toChar).mkString
    } else if (!queries.isEmpty) {
      this.buildQuery(queries)
    } else {
      ""
    }
  }

  protected def buildQuery(queries: scala.collection.Map[String, String]): String = {
    queries.iterator.map(i => urlEncode(i._1) + "=" + urlEncode(i._2)).mkString("&")
    }
  
  protected def quoteString(str: String, replace:String) = {
    val replaceTo = replace.map(_.toByte).map(b => "%c%02x".format('%', b)).mkString
    str.replace(replace, replaceTo)
  }

  protected def buildMultipartMsg(boundary: String,
                                  queries: scala.collection.Map[String, String],
                                  files: scala.collection.Map[String, String]): Array[Byte] = {
    val msgQue = new ArrayBuffer[Byte]()
    val ln = "\r\n".map(_.toByte)
    if (!queries.isEmpty) {
      for (i <- queries.iterator) {
        msgQue.appendAll(this.buildSubMsgHeader(boundary, i._1).map(_.toByte))
        msgQue.appendAll(i._2.map(_.toByte))
        msgQue.appendAll(ln)
      }
    }
    if (!files.isEmpty) {
      for (i <- files.iterator) {
        val filename = new java.io.File(i._2).getName
        msgQue.appendAll(this.buildSubMsgHeader(boundary, i._1, filename).map(_.toByte))
        val fileBytes = file2Bytes(i._2)
        msgQue.appendAll(fileBytes)
        msgQue.appendAll(ln)
      }
    }
    if (!files.isEmpty) {
      msgQue.appendAll("--%s--".format(boundary).map(_.toByte))
      msgQue.appendAll(ln)
    }
    msgQue.toArray
  }

  protected def buildSubMsgHeader(boundary: String, key: String, filename: String = null): String = {
    val headerQue = new Queue[String]()
    headerQue.enqueue("--%s".format(boundary))
    val disposition = this.buildContentDisposition(key, filename)
    headerQue.enqueue(disposition)
    if (filename != null) {
      val mime = checkMimeType(filename)
      val filetype =
        if (mime == "text/plain") "text/plain; charset=utf-8" else mime
      headerQue.enqueue("Content-type: %s".format(filetype))
    }
    headerQue.enqueue("")
    headerQue.enqueue("")
    headerQue.mkString("\r\n")
  }

  protected def buildContentDisposition(key: String, filename: String = null): String = {
    if (filename != null) "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"".format(key, filename)
    else "Content-Disposition: form-data; name=\"%s\"".format(key)
  }

  protected def buildContentType(boundary: String,
                                 queries: scala.collection.Map[String, String],
                                 files: scala.collection.Map[String, String]): String = {
    if (!files.isEmpty) "multipart/form-data; boundary=%s".format(boundary)
    else if (!queries.isEmpty) "application/x-www-form-urlencoded"
    else ""
  }

  protected def writeBytes(stream: OutputStream,
                           boundary: String,
                           queries: scala.collection.Map[String, String],
                           files: scala.collection.Map[String, String]): Int = {
    if (!files.isEmpty) {
      this.writeMultipart(stream, boundary, queries, files)
    } else if (!queries.isEmpty) {
      this.writeQuery(stream, queries)
    } else {
      0
    }
  }

  protected def writeQuery(stream: OutputStream,
                           queries: scala.collection.Map[String, String]): Int = {
    var queryBytes = this.buildQuery(queries).getBytes
    stream.write(queryBytes)
    queryBytes.length
    
  }

  protected def writeMultipart(stream: java.io.OutputStream,
                               boundary: String,
                               queries: scala.collection.Map[String, String],
                               files: scala.collection.Map[String, String]): Int = {
    var contentsLength = 0
    var temp: String = null
    var bytes: Array[Byte] = null
    var readIter: BytesReader = null
    if (!queries.isEmpty) {
      for (query <- queries) {
        temp = "%s%s\r\n".format(this.buildSubMsgHeader(boundary, query._1), query._2)
        bytes = temp.map(_.toByte).toArray
        contentsLength += bytes.length
        stream.write(bytes)
      }
    }
    if (!files.isEmpty) {
      val ln = "\r\n".map(_.toByte).toArray
      for (file <- files) {
        temp = this.buildSubMsgHeader(boundary, file._1, new java.io.File(file._2).getName)
        bytes = temp.map(_.toByte).toArray
        contentsLength += bytes.length
        stream.write(bytes)
        readIter = ReadWrite.readBytes(file._2)
        for (readed <- readIter) {
          contentsLength += readed.length
          stream.write(readed)
        }
        readIter.close
        contentsLength += ln.length
        stream.write(ln)
      }
    }
    if (!files.isEmpty) {
      val footer = "--%s--\r\n".format(boundary).map(_.toByte).toArray
      contentsLength += footer.length
      stream.write(footer)
    }
    contentsLength
  }

  protected def calculateContentsLength(boundary: String,
                                        queries: scala.collection.Map[String, String],
                                        files: scala.collection.Map[String, String]): Int = {
    var result: Int = 0
    if (!files.isEmpty) {
      val ln = "\r\n".map(_.toByte).length
      if (!queries.isEmpty) {
        for (i <- queries.iterator) {
          result += this.buildSubMsgHeader(boundary, i._1).map(_.toByte).length
          result += i._2.map(_.toByte).length
          result += ln
        }
      }
      for (i <- files.iterator) {
        val file = new java.io.File(i._2)
        result += this.buildSubMsgHeader(boundary, i._1, file.getName).map(_.toByte).length
        result += file.length.toInt
        result += ln
      }
      result += "--%s--\r\n".format(boundary).map(_.toByte).length
    } else if (!queries.isEmpty) {
      result = this.buildQuery(queries).map(_.toByte).length
    }
    result
  }
}
