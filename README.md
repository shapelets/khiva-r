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

### Branching model
Our branching model has two permanent branches, **develop** and **master**. 
We aim at using `develop` as the main branch, where all features are merged. 
In this sense, we use the master branch to push the release versions of the binding for the Khiva library.

### Contribution process
In order to contribute to the code base, we follow the next process:
1. The main branch is develop, every developer should pull the current status of the branch before starting to develop any new feature.
`git pull`
2. Create a new branch with the following pattern "feature/[name_of_the_feature]"
`git checkout -b feature/exampleFeature`
3. Develop the new feature on the the new branch. It includes testing and documentation.
`git commit -a -m "Bla, Bla, Bla";  git push`
4. Open a Pull Request to merge the feature branch into develop. Currently, a pull request has to be reviewed by one person at least.
6. Switch back to the develop branch.
`git checkout develop`
6. Finally, delete your local feature branch.
`git branch -d feature/exampleFeature`
7. Pull the latest changes.
`git pull`
