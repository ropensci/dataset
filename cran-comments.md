## Test environments

Local:
* Windows10 x86_64-w64-mingw32 (64-bit), R version 4.5.0, locally.

r_hub:
* various platforms

rOpenSci:
* ubuntu-latest with stricter checks.

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## 0.4.3 resumbission

A fix was earlier offered but apparently out of the time window to retain 
the package.

This update fixes a failing unit test that relied on exact printed
formatting of `bibentry`/`person` objects under recent R-devel versions.
The affected test was revised to check semantic validity rather than
platform-dependent textual formatting.

In addition, S3 method registration/documentation was modernized to
comply with current roxygen2 requirements.

All checks now pass locally.




