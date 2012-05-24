package test

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit._
import java.io.File
import com.shc.http._
import com.shc.readwrite._

class RequestTest extends AssertionsForJUnit {

  var req: Request = _
  var resp: Response = _
  val data = Map("key1" -> "val1", "key2" -> "val2")
  val file1 = "./test_data/test2send1.txt"
  val file2 = "./test_data/test2send2.txt"
  val files = Map("file1" -> file1, "file2" -> file2)
  val storetest1 = "./test_data/test2write.txt"
  val storetest2 = "./test_data/nonwritable/test.txt"
  //val testurl = "http://www.ghostreet.net/dj/"
  val testurl = "http://localhost:8000/"
  val methodtest = testurl + "method"
  val cookietest = testurl + "cookie"
  val basictest = testurl + "basic"
  val digesttest = testurl + "digest"
  val timeouttest = testurl + "timeout"
  
  /**
   * [How to build test environments in localhost]
   * 	>>> cd testserver
   * 	>>> bash fetchbootstrap.sh
   * 	>>> python bootstrap.py
   * 	>>> bin/buildout
   * 	>>> bin/django runserver
   * that's all!
   * 
   * Here is a server side test program in django.
   *
   * def cookie(request):
   *     response = HttpResponse("your cookie is ok.")
   *     count = int(request.COOKIES["count"]) + 1 if "count" in request.COOKIES else 0
   *     response.set_cookie("count", str(count))
   *     return response
   *
   * def method(request):
   *     method = "[{0}]".format(request.method)
   *     if request.method == "GET":
   *         queries = get_queries(request.GET)
   *         return HttpResponse(method + queries)
   *     elif request.method == "POST":
   *         queries = method + get_queries(request.POST)
   *         if request.FILES:
   *             filequeries = get_files(request.FILES)
   *             queries = queries + "<FILES>" + filequeries
   *         return HttpResponse(queries)
   *     else
   *         return HttpResponse()
   *
   * def get_queries(dictlike):
   *     queries = "&".join(["{0}={1}".format(key, dictlike[key]) for key in sorted(dictlike.keys())])
   *     return queries
   *
   * def get_files(dictlike):
   *     queries = "&".join(["{0}={1}:{2}".format(key, dictlike[key].name, dictlike[key].size) for key in sorted(dictlike.keys())])
   *     return queries
   */
  
  @Test
  def timeoutTest(){
    val req = Request.get(timeouttest, timeout = 5000)
    val res = req match{
    	case Some(response) => response
    	case None => None
    }
    assertTrue(res != None)
  }

  @Test
  def getTest() {
    req = Request(methodtest)
    resp = req.send("GET").get
    assertEquals(resp.statusCode, 200)
    assertEquals(resp.url.toString, methodtest)
    assertEquals(resp.message, "[GET]")

    resp = req.send("GET", data).get
    assertEquals(resp.statusCode, 200)
    assertEquals(resp.url.toString, methodtest + "?key1=val1&key2=val2")
    assertEquals(resp.message, "[GET]key1=val1&key2=val2")
    
    resp = Request.get(methodtest, data).get
    assertTrue(resp.store(storetest1))
  }

  @Test
  def postTest() {
    req = Request(methodtest)
    resp = req.send("POST").get
    assertEquals(resp.statusCode, 200)
    assertEquals(resp.url.toString, methodtest)
    assertEquals(resp.message, "[GET]")

    resp = req.send("POST", data).get
    assertEquals(resp.statusCode, 200)
    assertEquals(resp.url.toString, methodtest)
    assertEquals(resp.message, "[POST]key1=val1&key2=val2")

    resp = req.send("POST", files = files).get
    assertEquals(resp.statusCode, 200)
    assertEquals(resp.url.toString, methodtest)
    var expect = "[POST]<FILES>file1=test2send1.txt:%s&file2=test2send2.txt:%s".format(new File(file1).length, new File(file2).length)
    assertEquals(resp.message, expect)

    resp = req.send("POST", data, files = files).get
    assertEquals(resp.statusCode, 200)
    assertEquals(resp.url.toString, methodtest)
    expect = "[POST]key1=val1&key2=val2" +
      "<FILES>file1=test2send1.txt:%s&file2=test2send2.txt:%s".format(new File(file1).length, new File(file2).length)
    assertEquals(resp.message, expect)
  }

  @Test
  def cookieTest() {
    var cookie = new java.net.CookieManager
    cookie.setCookiePolicy(java.net.CookiePolicy.ACCEPT_ALL)
    req = Request(cookietest)
    import scala.collection.JavaConversions._
    resp = req.send("GET", cookie=cookie).get
    assertEquals(resp.cookies.map(_.toString), List("count=0"))
    req.send("GET").get
    assertEquals(resp.cookies.map(_.toString), List("count=1"))
    req.send("GET").get
    assertEquals(resp.cookies.map(_.toString), List("count=2"))
    cookie = resp.cookieManager
    cookie.getCookieStore.removeAll
    resp = req.send("GET", cookie = cookie).get
    assertEquals(resp.cookies.map(_.toString), List("count=0"))
    val handler = java.net.CookieHandler.getDefault
    assertEquals(handler, resp.cookieManager)
    resp = Request.get(cookietest).get
    assertEquals(resp.cookies.map(_.toString), List("count=1"))
    assertEquals(resp.cookieManager.getCookieStore.getCookies, cookie.getCookieStore.getCookies)
    resp = Request.get(cookietest, cookie = resp.cookieManager).get
    assertEquals(resp.cookies.map(_.toString), List("count=2"))
    cookie.getCookieStore.removeAll
    cookie.setCookiePolicy(java.net.CookiePolicy.ACCEPT_ORIGINAL_SERVER)
  }
  
  @Test
  def authTest(){
    val auth = ("thisurlonly", "thisurlonly")
    //digest authentication test.
    resp = Request.get(digesttest).get
    assertEquals(resp.statusCode, 401)
    var temp = resp.auth(auth._1, auth._2).get
    resp = Request.get(digesttest, auth = temp).get
    assertEquals(resp.statusCode, 200)
    
    //basic authentication test
    resp = Request.get(basictest).get
    assertEquals(resp.statusCode, 401)
    assertNotNull(resp.headers.getOrElse("WWW-Authenticate", null))

    var validauth = resp.auth(auth._1, auth._2).get
    resp = Request.get(basictest, auth = validauth).get
    assertEquals(resp.statusCode, 200)
    assertNull(resp.headers.getOrElse("WWW-Authenticate", null))
    
    resp = Request.get(basictest).get
    assertEquals(resp.statusCode, 401)
    val badauth = ("hoge", "hoge")  
    resp = Request.get(basictest, auth = resp.auth(badauth._1, badauth._2).get).get
    assertEquals(resp.statusCode, 401)
    
    resp = Request.get(basictest).get
    assertEquals(resp.statusCode, 401)
    resp = Request.get(basictest, auth = validauth).get
    assertEquals(resp.statusCode, 200)
  }
  
}