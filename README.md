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
