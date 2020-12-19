# multidictR

Functions that help to work with dictionaries that include search strings (and
wildcards) consisting of multiple words. Quanteda does not allow for such more
complex multi-word dictionaries that for example include wildcards spanning
multiple words. This package provides some functions to deal with such
dictionaries. It is used with the populism dictionary in popdictR.

Functions from this package are used with the populism dictionary in my [popdictR](https://github.com/jogrue/popdictR) project.


## Status

The package worked for my particular use case. All functions are documented 
already. However, the package has not been tested extensively. Thus, I am glad 
for any feedback or issues. A new version (1.0) is planned for the end of March 2021. Some of the issues I plan on addressing:

* More thorough testing
* Better documentation
* Highlighting use cases for this package


## Install

This package requires my package
[regexhelpeR](https://github.com/jogrue/regexhelpeR) which should be installed
before this package.

You can install everything from within R using [devtools](https://github.com/hadley/devtools):

```R
library(devtools)

# Install the dependency regexhelpeR from GitHub
devtools::install_github("jogrue/regexhelpeR")

# Install the multidictR package from GitHub
devtools::install_github("jogrue/multidictR")
```

## Cite

Gründl, J. (2020). Populist ideas on social media: A dictionary-based measurement of populist communication. _New Media & Society_. Advance online publication. [https://doi.org/10.1177/1461444820976970](https://doi.org/10.1177/1461444820976970)

Gründl, J. (2020). _multidictR_ (R package). [https://github.com/jogrue/multidictR](https://github.com/jogrue/multidictR)
