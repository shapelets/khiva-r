[![License: MPL 2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://github.com/shapelets/khiva-java/blob/master/LICENSE.txt)  

| Branch        | Build Linux and Mac OS                                                                                                         |  Build Windows                                                                                                                                                           | Code Coverage                                                                                                                                      |
| --------------|:------------------------------------------------------------------------------------------------------------------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:--------------------------------------------------------------------------------------------------------------------------------------------------:|
| master        | [![Build Status](https://travis-ci.org/shapelets/khiva-r.svg?branch=master)](https://travis-ci.org/shapelets/khiva-r/branches) | [![Build status](https://ci.appveyor.com/api/projects/status/49yh3arn9fx3ij0l/branch/master?svg=true)](https://ci.appveyor.com/project/shapelets/khiva-r/branch/master)  |[![Coverage Status](https://codecov.io/gh/shapelets/khiva-r/branch/master/graph/badge.svg)](https://codecov.io/gh/shapelets/khiva-r/branch/master)  |
| develop       | [![Build Status](https://travis-ci.org/shapelets/khiva-r.svg?branch=develop)](https://travis-ci.org/shapelets/khiva-r/branches)| [![Build status](https://ci.appveyor.com/api/projects/status/49yh3arn9fx3ij0l/branch/develop?svg=true)](https://ci.appveyor.com/project/shapelets/khiva-r/branch/develop)|[![Coverage Status](https://codecov.io/gh/shapelets/khiva-r/branch/develop/graph/badge.svg)](https://codecov.io/gh/shapelets/khiva-r/branch/develop)|

# README #
This is the Khiva binding for connecting the R programming language and the Khiva C++ library.

## License
This project is licensed under [MPL-v2](https://www.mozilla.org/en-US/MPL/2.0/).
 
## Quick Summary
This R library called 'khiva' provides all the functionalities of the Khiva C++ library for time series analytics.

## Set up

Note that Khiva imports the "bit64" package by default, so in order to use Khiva you should have installed the "bit64" package.

In order to use this R binding, is is needed to have the Khiva C++ Library installed.
Next, we explain two methods of how to install the Khiva R library.

### Installing from source
To install the library, you need to set the current working directory to the library path 
and install it by executing the following commands: 
```R
options(devtools.install.args = "--no-multiarch")
devtools::install()
```
After that, the library is made available by executing:
```R
library(khiva)  
```

### Installing from Github

To install the library using the Github repository, it is just needed to execute the following command: 
```R
options(devtools.install.args = "--no-multiarch")
devtools::install_github("shapelets/khiva-r")
```
After that, we made the library available by executing:
```R
library(khiva)  
```

## Testing:
To execute the tests, set your working directory to the library path and execute the following command:
```R
devtools::test()
```

## Documentation
This R library follows the standard way of writing documentation of R by using Roxygen2.

## Contributing
The rules to contribute to this project are described [here](CONTRIBUTING.md)

[![Powered by Shapelets](https://img.shields.io/badge/powered%20by-Shapelets-orange.svg?style=flat&colorA=E1523D&colorB=007D8A)](https://shapelets.io)