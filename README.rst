How to build
------------
Just run ant, then copy and paste jar file to your classpath.
Ivy is needed to resolve dependencies.

Dependencies
------------
All libraries are used to execute unit test.
    * scalatest
    * junit
    * commons-codec

Created jar is executable without any dependencies.

How to use
----------

Just import com.shc.http::

    import com.shc.http._

Here is a simple GET request. This returns request.Response instance obj::

    val url = "http://example.com"
    var resp = Request.get(url)

If you want to add query strings, just pass key-value map to the method::

    resp = Request.get(url, Map("name" -> "foofoo"))

How to retrieve response contents::

    if(resp != None){
      val url = resp.get.url // java.net.URL
      val statusCode = resp.get.statusCode // String
      val headers = resp.get.headers // Map[String, List[String]]
      val message = resp.get.message // String
      val cookies = resp.get.cookies // List[java.net.HttpCookie]
    }

Uploading files::

    val files = Map("key" -> "/where/the/file/exist")
    resp = Request.post(url, files = files)
    resp = Request.post(url, Map("key" -> "value"), files)

Creating authentication credential. This support basic & digest auth::

    if(resp != None & resp.get.statusCode == 401){
      resp = Request.get(url, auth = resp.auth("user", "pass"))
      println(resp.get.statusCode == 200)
    }

Reusing cookieManager::

    val cookies = resp.cookies.foreach(println)
    val cookieManager = resp.cookieManager
    resp = Request.get(url, cookie = cookieManager)

Storing contents::

    val path = "/path/to/somewhere"
    val bool = resp.store(path)
    println(bool)
