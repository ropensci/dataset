## Test environments

Local:
* Windows10 x86_64-w64-mingw32 (64-bit), R version 4.5.0, locally.
* https://win-builder.r-project.org/ all three configurations

r_hub:
* latest release: macos, linux, windows (Windows Server 2022)
* devel: atlas (Fedora Linux 38)
* R-oldrelease win-builder.r-project.org

rOpenSci:
* ubuntu-latest with stricter checks.

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## 0.4.1 sumbission

This is a minor release that works better with Date and POSIXct types. 
In some tests, the following false negative URLs may appear, which are 
correct uses of example.com, the World Wide Web consortium's usage of 
non-existing example URLs:

Found the following (possibly) invalid URLs:
  URL: http://example.com/dataset#eg:1
    From: inst/doc/rdf.html
    Status: 404
    Message: Not Found
  URL: http://example.com/dataset#eg:2
    From: inst/doc/rdf.html
    Status: 404
    Message: Not Found
  URL: http://example.com/dataset#eg:3
    From: inst/doc/rdf.html
    Status: 404
    Message: Not Found

The domain `http://example.com/` is reserved by the IETF/W3C for use in
documentation and examples. These IRIs were included in the vignette to
demonstrate RDF/Linked Data usage, and are valid by design even though they do
not resolve.

However, to facilitate CRAN publication and avoid repeated URL check notes, I
replaced these with example URIs hosted on the package website that do return
HTTP 200 and are published in both HTML and Turtle format. This removes the 404
check while retaining the intended illustration of example IRIs.

Furthermore, on https://win-builder.r-project.org/, while testing, once the 
Eurostat metadata definition server failed. This is extremely unexpected, but 
I replaced this definition with that of the Statistical Data and Metadata 
eXchange, i.e., to http://purl.org/linked-data/sdmx/2009/dimension#refArea.
(This is a permanent IRI not an URL, therefore no https.)


