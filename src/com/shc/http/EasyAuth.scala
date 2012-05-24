package com.shc.http

import scala.collection.mutable.{ ArrayBuffer, Map => MutableMap }
import java.security.MessageDigest
import com.shc.codec.Base64

object EasyAuth {

  private val cNonceBytes = new Array[Byte](64)
  
  val cNonce = java.util.UUID.randomUUID.toString
  var ncMap = scala.collection.mutable.Map[String, Int]()

  def auth(method: String,
           httpMethod: String,
           uri: String,
           username: String,
           password: String,
           info: MutableMap[String, String]) = {
    if (method == "Basic") buildBasicValue(username, password)
    else buildDigestValue(httpMethod, uri, username, password, info)
  }

  protected def buildBasicValue(username: String, password: String) = {
    val credential = Base64.encode(username + ":" + password)
    "Basic %s".format(credential)
  }

  protected def buildDigestValue(httpMethod: String,
                                 uri: String,
                                 username: String,
                                 password: String,
                                 info: MutableMap[String, String]):String = {
    val parts = ArrayBuffer[String]()
    val form = "%s=%s"
    val realm = info.get("realm").get
    val nonce = info.get("nonce").get
    //val stale = info.get("stale")
    val opaque = info.get("opaque")
    val algorithm = info.getOrElse("algorithm", "MD5")
    val qop = {
      val qopStr = info.get("qop")
      if (qopStr != None) {
        try {
          processQop(qopStr.get)
        } catch {
          case e:Exception => return ""
        }
      }
      else None
    }
    val messageDigest = MessageDigest.getInstance(algorithm)
    val response = this.buildResponse(messageDigest, httpMethod, uri, username, password, realm, nonce, qop)

    parts.append(form.format("username", "\"%s\"".format(username)))
    parts.append(form.format("realm", "\"%s\"".format(realm)))
    parts.append(form.format("nonce", "\"%s\"".format(nonce)))
    parts.append(form.format("uri", "\"%s\"".format(uri)))
    parts.append(form.format("algorithm", algorithm))
    if (qop != None) {
      parts.append(form.format("qop", qop.get))
      parts.append(form.format("cnonce", "\"%s\"".format(this.cNonce)))
      val nc = "%08X".format(ncMap.get(nonce).get) // nonce was registered to ncMap when buildResponse executed.
      parts.append(form.format("nc", nc))
    }
    parts.append(form.format("response", "\"%s\"".format(response)))
    if (opaque != None) parts.append(form.format("opaque", "\"%s\"".format(opaque.get)))
    "Digest %s".format(parts.mkString(", "))
  }

  protected def processNC(nonce: String) = {
    val nc = ncMap.getOrElse(nonce, 0)
    if (ncMap.size > 5) ncMap.clear
    ncMap.update(nonce, nc + 1)
    nc + 1
  }

  protected def processQop(qop: String) = {
    val splitted = qop.split(",").map(_.replace(" ", ""))
    if (splitted.exists(_ == "auth")) Some("auth")
    else throw new Exception("only auth is supported.")
  }

  protected def buildResponse(messageDigest: MessageDigest,
                              httpMethod: String,
                              uri: String,
                              username: String,
                              password: String,
                              realm: String,
                              nonce: String,
                              qop: Option[String]): String = {

    val responseList = new ArrayBuffer[String]()
    val a1 = this.a1(messageDigest, username, password, realm)
    val a2 = this.a2(messageDigest, httpMethod, uri)
    responseList.append(a1)
    responseList.append(nonce)
    if (qop != None) {
      responseList.append("%08X".format(this.processNC(nonce))) // nc + 1
      responseList.append(this.cNonce)
      responseList.append(qop.get) //???
    }
    responseList.append(a2)
    val temp = responseList.mkString(":")
    val respBytes = messageDigest.digest(temp.map(_.toByte).toArray)
    bytes2HexString(respBytes)
  }

  protected def a1(messageDigest: MessageDigest,
                   username: String,
                   password: String,
                   realm: String) = {
    val bytes = messageDigest.digest("%s:%s:%s".format(username, realm, password).map(_.toByte).toArray)
    bytes2HexString(bytes)
  }

  protected def a2(messageDigest: MessageDigest,
                   httpMethod: String,
                   uri: String) = {
    val bytes = messageDigest.digest("%s:%s".format(httpMethod, uri).map(_.toByte).toArray)
    bytes2HexString(bytes)
  }

  protected def bytes2HexString(bytes: Array[Byte]) = {
    bytes.map(b => "%02x".format(b)).mkString
  }

}