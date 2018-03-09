[![Travis-CI Build Status](https://travis-ci.org/M3SOulu/NLoN.svg?branch=master)](https://travis-ci.org/M3SOulu/NLoN)

# NLoN - Natural Language or Not

R package for identifying text containing natural language (or not)
using machine learning.

## Installation

<!-- From CRAN: -->

<!--     install.packages("NLoN") -->

With devtools:

    devtools::install_github("M3SOulu/NLoN")


## Example Usage
    library(NLoN)
    data(nlon.data)
    model <- NLoNModel(nlon.data$text, nlon.data$rater2) 

    topredict <- c("This is natural language.", "public void NotNaturalLanguageFunction(int i, String s)")
    NLoNPredict(model, topredict, lambda=0.01) 
