library(devtools)
library(NLoN)

system.time(nlon.model <- NLoN::NLoNModel(nlon.data$text, nlon.data$rater2,
                                          cv="caret", repeats=10))
print(nlon.model$finalModel$lambdaOpt)

#devtools::use_data(nlon.model, overwrite=TRUE)
