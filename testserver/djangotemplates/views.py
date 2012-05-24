from django.http import HttpResponse
from django_digest.decorators import httpdigest
from django.contrib.auth import authenticate, login
import base64

def view_or_basicauth(view, request, test_func, realm="", *args, **kwargs):
    if test_func(request.user):
        return view(request, *args, **kwargs)

    if 'HTTP_AUTHORIZATION' in request.META:
        auth = request.META['HTTP_AUTHORIZATION'].split()
        if len(auth) == 2:
            if auth[0].lower() == "basic":
                uname, passwd = base64.b64decode(auth[1]).split(':')
                user = authenticate(username=uname, password=passwd)
                if user is not None:
                    login(request, user)
                    request.user = user
                    return view(request, *args, **kwargs)

    response = HttpResponse()
    response.status_code = 401
    response['WWW-Authenticate'] = 'Basic realm="%s"' % realm
    return response

def logged_in_or_basicauth(realm=""):
    def view_decorator(func):
        def wrapper(request, *args, **kwargs):
            return view_or_basicauth(func, request,
                                     lambda u: u.is_authenticated(),
                                     realm, *args, **kwargs)
        return wrapper
    return view_decorator

@logged_in_or_basicauth("test@test.test")
def basic(request):
    return HttpResponse("ok")

@httpdigest
def digest(request):
    if request.user.is_authenticated():
        val = "you are authenticated."
    else:
        val = "good bye."
    return HttpResponse(val)

def cookie(request):
    response = HttpResponse("your cookie is ok.")
    count = int(request.COOKIES["count"]) + 1 if "count" in request.COOKIES else 0
    response.set_cookie("count", str(count))
    return response

def timeout(request):
    import time
    time.sleep(2)
    return HttpResponse("slept 5 sec.")

def method(request):
    method = "[{0}]".format(request.method)
    if request.method == "GET":
        queries = get_queries(request.GET)
        return HttpResponse(method + queries)
    elif request.method == "POST":
        queries = method + get_queries(request.POST)
        if request.FILES:
            filequeries = get_files(request.FILES)
            queries = queries + "<FILES>" + filequeries
        return HttpResponse(queries)
    elif request.method == "HEAD":
        return HttpResponse()

def get_queries(dictlike):
    queries = "&".join(["{0}={1}".format(key, dictlike[key]) for key in sorted(dictlike.keys())])
    return queries

def get_files(dictlike):
    queries = "&".join(["{0}={1}:{2}".format(key, dictlike[key].name, dictlike[key].size) for key in sorted(dictlike.keys())])
    return queries
