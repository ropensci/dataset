## Test environments

Local:
* Windows10 x86_64-w64-mingw32 (64-bit), R version 4.5.0, locally.

r_hub:
* various platforms

rOpenSci:
* ubuntu-latest with stricter checks.

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## 0.4.2 sumbission

The previous CRAN check failure on R-devel was due to a brittle unit test that 
assumed a stable printed representation of `utils::bibentry()` output.
The package code itself was not at fault; only the test relied on exact author
formatting, which changed in R-devel. The test has been revised to check
package-specific behavior rather than base R print output, making it robust 
across R versions.

No functional changes were made to the package API or implementation.



