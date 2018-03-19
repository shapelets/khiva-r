# TSA-R #

This is the TSA wrapper or binding for connecting the R programming language and
 the TSA library.

### What is this repository for? ###

* Quick summary:
This R package called 'tsa' provides all the functionalities of the TSA library for time series analytics.
* Version:
0.1


### How do I get set up? ###

* Summary of set up
* Dependencies:
This package needs of "bit64" package.
For installing it, execute the next line in R console:
```
install.packages("bit64")
```
* Installation: For installing this R package, it is necessary to execute next command:
```
devtools::install_github("<github_url>")
```
or
download the folder:
```
git clone <github_url>
```
get into the tsa package:
```
cd tsa-r
cd tsa
```
Execute next command in the R console:
```
devtools::install()
```
### Executing the tests:

Set as the working directory the directory corresponding to the tests by using:

```
setwd("<tests-directory>")
```
Execute the next line for running the tests:

```
test_results <- test_dir("./", reporter="summary")
```
See the results:

```
test_results
```
### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact
