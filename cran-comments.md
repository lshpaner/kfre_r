## Test environments
* Local: Windows 11 x64, R 4.4.2 — OK
* win-builder: R-devel — OK (2 NOTEs)

## R CMD check results
0 errors | 0 warnings | 2–3 notes

* New submission.

* NOTE (local Windows only): “unable to verify current time”.
  This is the usual false-positive timestamp check on Windows; no files with
  future timestamps are present.

* NOTE: Possible “misspellings” — KFRE, CKD, ESRD, Tangri, et al. — are domain terms.

* NOTE: “Author field differs from Authors@R” — cosmetic formatting difference
  due to ORCID/URL; intentional.

## Reverse dependencies
* None.


## R-hub
* ubuntu-release: OK
* windows (R-devel): OK
* macOS-arm64 (R-devel): OK
