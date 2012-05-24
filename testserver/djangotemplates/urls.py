from django.conf.urls.defaults import patterns, include
from views import *

urlpatterns = patterns('',
    (r'^method', method),
    (r'^cookie', cookie),
    (r'^digest', digest),
    (r'^basic', basic),
    (r'^timeout', timeout),
)
