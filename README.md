# madrat-based package providing core FAO-related preprocessing functions

R package **mrfaocore**, version **1.8.1**

   [![R build status](https://github.com/pik-piam/mrfaocore/workflows/check/badge.svg)](https://github.com/pik-piam/mrfaocore/actions) [![codecov](https://codecov.io/gh/pik-piam/mrfaocore/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrfaocore) [![r-universe](https://pik-piam.r-universe.dev/badges/mrfaocore)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

This madrat-based package provides core FAO-related
    preprocessing functions.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r
install.packages("mrfaocore")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r
update.packages()
```

## Questions / Problems

In case of questions / problems please contact David Chen <david.chen@pik-potsdam.de>.

## Citation

To cite package **mrfaocore** in publications use:

Chen D, Kreidenweis U, Mishra A, Karstens K, Leon Bodirsky B, Leip D, Stevanovic M, Leon Bodrisky B, Klein D, Molina Bacca E (2026). "mrfaocore: madrat-based package providing core FAO-related preprocessing functions." Version: 1.8.1, <https://github.com/pik-piam/mrfaocore>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrfaocore: madrat-based package providing core FAO-related preprocessing
    functions},
  author = {David Chen and Ulrich Kreidenweis and Abhijeet Mishra and Kristine Karstens and Benjamin {Leon Bodirsky} and Debbora Leip and Mishko Stevanovic and Benjamin {Leon Bodrisky} and David Klein and Edna {Molina Bacca}},
  date = {2026-07-30},
  year = {2026},
  url = {https://github.com/pik-piam/mrfaocore},
  note = {Version: 1.8.1},
}
```
