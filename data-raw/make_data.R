library(devtools)
library(data.table)

filenames <- c(mozilla="data-raw/lines.10k.cfo.sample.2000 - Mozilla (Firefox, Core, OS).csv",
               kubernetes="data-raw/lines.10k.cfo.sample.2000 - Kubernetes (Slackarchive.io).csv",
               lucene="data-raw/lines.10k.cfo.sample.2000 - Lucene-dev mailing list.csv")

MakeFactor <- function(ratings) factor(ratings, labels=c("NL", "Not"))

nlon.data <- rbindlist(lapply(names(filenames), function(x) {
  res <- fread(filenames[x], encoding="UTF-8")[!is.na(Disagreement)]
  if (x == "lucene") {
    res[, Text := gsub("^[>|\\s]+", "", Text, perl=TRUE)]
  }
  res <- res[, list(source=x, text=Text,
                    rater1=MakeFactor(Mika),
                    rater2=MakeFactor(Fabio))]
  fwrite(res, sprintf("data-raw/%s.csv", x))
  res
}))

devtools::use_data(nlon.data, overwrite=TRUE)
