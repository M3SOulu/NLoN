context("Regression test for feature extraction")

FeatureExtractionOld <- function(labeled) {
  labeled <- copy(labeled)
  myslq_sw <- system.file("extdata", "mysql_sw_wo_code_words.txt",
                          package="NLoN", mustWork=TRUE)
  myslq_sw <- read.csv(myslq_sw, stringsAsFactors=FALSE, header=FALSE)
  #Not same encoding as in cubernets. not used.
  labeled$length <- nchar(labeled$text)
  labeled$ratio.caps <- plyr::ldply(stringr::str_match_all(labeled$text,"[A-Z]"),length)/labeled$length
  labeled$ratio.specials <- plyr::ldply(stringr::str_match_all(labeled$text,"[^a-zA-Z\\d\\s]"),length)/labeled$length
  labeled$ratio.numbers <- plyr::ldply(stringr::str_match_all(labeled$text,"[\\d]"),length)/labeled$length

  labeled$length.words <- labeled$length /(plyr::ldply(stringr::str_match_all(labeled$text,"[\\s+]"),length) +1)
  labeled$words <- plyr::ldply(stringr::str_match_all(labeled$text,"[\\s+]"),length) +1
  #we count stopword twice with two different tokenizers.
  labeled[,stopwords:= sapply(text, function(x) sum(tolower(tokenizers::tokenize_regex(x, pattern = "\\s+", simplify = TRUE)) %in%  myslq_sw$V1))/ labeled$words]
  labeled[,stopwords2:= sapply(text, function(x) sum(tokenizers::tokenize_words(x, simplify = TRUE) %in%  myslq_sw$V1))/ labeled$words]
  #If the line ends to emoticon this is not true :-) so we substract. R does not support lookahead in regex which would also solve this
  labeled[,last.char.code:= sapply(text, function(x) max(c(sum(grep("\\)$|\\{$|;$", x)) - sum (grep (":-\\)$|;-\\)|:\\)$|;\\)$|:-\\($|:\\($", x)), 0)))]

  labeled[,last.char.nl:= sapply(text, function(x) sum(grep("\\.$|\\!$|\\?$|:$|,$", x)))]
  labeled$first.3.chars.letters <- plyr::ldply  (stringr::str_match_all(substr(gsub("\\s", "", labeled$text), 1,3), "[a-zA-Z]"),length)
  labeled$emoticons <- plyr::ldply(stringr::str_match_all(labeled$text,":-\\)|;-\\)|:\\)|;\\)|:-\\(|:\\("),length)
  #Sanity chek should empty
  #labeled[last.char.nl==1 & last.char.code==1]$text

  #New features from Kubernets
  #Starts with @ sign
  labeled[,first.char.at:= sapply(text, function(x) sum(grep("^@", x)))]
  labeled
}

test_that("FeatureExtraction gives same results as FeatureExtractionOld", {
  skip_on_cran()
  data(nlon.data)
  res <- FeatureExtraction(nlon.data$text)
  res.old <- FeatureExtractionOld(nlon.data)

  expect_equal(res$ratio.caps, res.old$ratio.caps)
  expect_equal(res$ratio.specials, res.old$ratio.specials)
  expect_equal(res$ratio.numbers, res.old$ratio.numbers)
  expect_equal(res$length.words, res.old$length.words)
  expect_equal(res$stopwords, res.old$stopwords)
  expect_equal(res$stopwords2, res$stopwords2)
  expect_equal(res$last.char.code, res.old$last.char.code)
  expect_equal(res$last.char.nl, res.old$last.char.nl)
  expect_equal(res$first.3.chars.letters, res.old$first.3.chars.letters)
  expect_equal(res$emoticons, res.old$emoticons)
  expect_equal(res$first.char.at, res.old$first.char.at)
})
