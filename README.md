# testmsim

The aim of the R package **testmsim** is to provide functions, data and
documentation for the testing of *spatial microsimulation*. There is an
extensive academic literature and body of applied applications that use
*spatial microdata* (individual-level data allocated to geographical zones)
but little testing of the methods used to generate the data data or the
validity of the results.

The main focus of **testmsim** is on the process of *population synthesis*,
which involves generating spatial microdata by allocating individuals from
a survey to zones based on geographically-aggregated constraint variables.

# Installing **testmsim**

The latest version of the package can be installed directly from the developers'
GitHub accounts. The
command `install_github` relies on the
excellent **[devtools](http://cran.r-project.org/web/packages/devtools/index.html)** package:

```
library(devtools)
install_github("emunozh/testmsim") # install Esteban's latest version
# or
install_github("robinlovelace/testmsim") # install Robin's latest version
```

# Building the package from source

To build the package from source on Linux, follow the below script.

```
cd testsim
./updatedoc.sh
cd ..
R CMD build testsim
R CMD INSTALL testsim_1.0.tar.gz
R CMD check testsim
```
