from setuptools import setup, find_packages

import os
if not os.path.exists("src"):
    os.mkdir("src")

setup(
    name = "testserver",
    version = "0.1",
    packages = find_packages('src'),
    package_dir = {'': 'src'},
)

def copy2django():
    if not os.path.exists("testserver"):
        os.mkdir("testserver")
    prjdir = "testserver"
    templatesdir = "djangotemplates"
    files2copy = os.listdir(templatesdir)
    for file in files2copy:
        readsrc = os.path.join(templatesdir, file)
        writedst = os.path.join(prjdir, file)
        fread = open(readsrc)
        fwrite = open(writedst, "w")
        try:
            fwrite.write(fread.read())
        finally:
            fwrite.close()
            fread.close()

copy2django()
