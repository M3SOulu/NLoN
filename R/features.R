mysql.stopwords <- system.file("extdata", "mysql_sw_wo_code_words.txt",
                               package="NLoN", mustWork=TRUE)
mysql.stopwords <- read.csv(mysql.stopwords, stringsAsFactors=FALSE,
                            header=FALSE)$V1

## emojis <- system.file("extdata", "emojis.csv",
##                       package="NLoN", mustWork=TRUE)
## emojis <- "data/emojis.csv"
## emojis <- fread(emojis)

ConvertFeatures <- function(data) {
  ## Make sure that the feature data is a matrix or Matrix object.
  ## Converts list into data.frame and then data.frame into matrix.
  if (is.list(data)) {
    if (length(unique(sapply(data, length))) == 1) {
      data <- as.data.table(data)
    } else stop("feature values don't have the same length")
  }
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  if ((is.matrix(data) && is.numeric(data)) || inherits(data, "Matrix")) {
    data
  } else stop("feature values are not a numeric matrix")
}

ComputeFeatures <- function(text, features) {
  ## Compute features. If features is a function, it will simply be
  ## applied on the text (and must return a list, data.frame, matrix
  ## or Matrix of numeric values). If feature is a list of functions,
  ## do a sapply of the functions which must all return a numeric
  ## vector of the same length as text.
  if (is.function(features)) {
    data <- features(text)
  } else if (is.list(features) && all(sapply(features, is.function))) {
    if (is.null(names(features))) {
      warning("features is a list of functions without names")
    }
    res <- sapply(features, function(f) f(text))
    if (length(text) == 1) {
      t(as.matrix(res))
    } else res
  } else stop("features must be a function or a list of functions")
}

#' Feature extraction.
#'
#' Computes a set of simple text-based features.
#'
#' The features computed are the followings:
#' \describe{
#'   \item{\code{ratio.caps}}{The ratio of uppercase letters.}
#'   \item{\code{ratio.specials}}{The ratio of special characters.}
#'   \item{\code{ratio.numbers}}{The ratio of number characters.}
#'   \item{\code{length.words}}{The average word length.}
#'   \item{\code{stopwords}}{The ratio of English stopwords (using first
#'   tokenizer).}
#'   \item{\code{stopwords2}}{The ratio of English stopwords (using second
#'   tokenizer).}
#'   \item{\code{last.char.nl}}{Boolean for the use of NL character at the
#'   end of the text.}
#'   \item{\code{last.char.code}}{Boolean for the use of code character at
#'   the end of text.}
#'   \item{\code{first.3.chars.letters}}{Number of letters in the three
#'   first characters.}
#'   \item{\code{emoticons}}{Number of emoticons}
#'   \item{\code{first.char.at}}{Boolean for the use of @ character at
#'   the beginning of the line.}
#' }
#'
#' @param text The text.
#' @return A data.table with values of the 11 features.
#' @export
FeatureExtraction <- function(text) {
  data <- data.table(text)
  features <- list(ratio.caps=features$CapsRatio,
                   ratio.specials=features$SpecialCharsRatio,
                   ratio.numbers=features$NumbersRatio,
                   length.words=features$AverageWordLength,
                   stopwords=features$StopwordsRatio1,
                   stopwords2=features$StopwordsRatio2,
                   last.char.code=features$LastCharCode,
                   last.char.nl=features$LastCharNL,
                   first.3.chars.letters=features$First3CharsLetter,
                   emoticons=features$Emoticons,
                   first.char.at=features$StartWithAt)
  as.data.table(ComputeFeatures(text, features))
}

#' Character 3-gram extraction.
#'
#' Computes the document term matrix of character 3-gram.
#'
#' @param text The text.
#' @return A document term matrix (sparse Matrix).
#' @export
Character3Grams <- function(text) {
  Preprocessor <- function(x) {
    gsub("[0-9]", "0", gsub("\\\032", "", x))
  }
  Tokenizer <- function (x) {
    tokenizers::tokenize_character_shingles(x, n=3, strip_non_alphanum=FALSE,
                                            lowercase=TRUE)
  }
  it <- text2vec::itoken(text, tokenizer=Tokenizer,
                         preprocessor=Preprocessor,
                         progressbar=FALSE)
  vocab <- text2vec::create_vocabulary(it)
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  text2vec::create_dtm(it, vectorizer)
}

#' 3-grams and feature extraction.
#'
#' Computes both 3-gram and simple text features.
#'
#' @param text The text.
#' @return A sparse Matrix with text features and 3-gram.
#' @seealso \code{\link{Character3Grams}}
#' @seealso \code{\link{FeatureExtraction}}
#' @export
TriGramsAndFeatures <- function(text) {
  cbind(Character3Grams(text), as.matrix(FeatureExtraction(text)))
}

#' Features.
#'
#' Module containing functions for individual simple text feature
#' extraction.
#'
#' Most functions have a single \code{text} parameter. The module
#' contains the following functions:
#'
#' \describe{
#'   \item{\code{Stopwords}}{Number of stopwords. Uses two optional
#'   parameters: \code{Tokenize} which is the word tokenizer to use
#'   and \code{stopwords} which is the list of stopwords to use.}
#'   \item{\code{Tokenize1}}{First tokenizer available for
#'   \code{Stopwords}.}
#'   \item{\code{Tokenize2}}{Second tokenizer available for
#'   \code{Stopwords}.}
#'   \item{\code{StopwordsRatio1}}{Ratio of stopwords using \code{Tokenize1}}
#'   \item{\code{StopwordsRatio2}}{Ratio of stopwords using \code{Tokenize2}}
#'   \item{\code{Caps}}{Number of uppercase letters.}
#'   \item{\code{CapsRatio}}{Ratio of uppercase letters.}
#'   \item{\code{SpecialChars}}{Number of special characters.}
#'   \item{\code{SpecialCharsRatio}}{Ratio of special characters.}
#'   \item{\code{Numbers}}{Number of digit characters.}
#'   \item{\code{NumbersRatio}}{Ratio of digit characters.}
#'   \item{\code{Words}}{Number of words.}
#'   \item{\code{AverageWordLength}}{Average word length.}
#'   \item{\code{LastCharCode}}{Boolean for the use of a code character at the
#'   end of the text.}
#'   \item{\code{LastCharNL}}{Boolean for the use of a natural language boolean
#'   at the end of the text.}
#'   \item{\code{First3Chars}}{Returns the first three non white characters.}
#'   \item{\code{First3CharsLetters}}{The number of three first non white
#'   characters that are letters.}
#'   \item{\code{Emoticons}}{The number of emoticons}
#'   \item{\code{StartWithAt}}{Boolean for the use of @ at the start of
#'   the text.}
#' }
#'
#' @export
features <- modules::module({
  modules::export("^[^.]")

  .CountRegexMatches <- function(text, re) {
    ## Count the number of match of a regex
    sapply(stringr::str_match_all(text, re), length)
  }

  Tokenize1 <- function(text) {
    ## Need to be fixed: add punctuation for seperator (at least .)
    ## lapply(tolower(text), tokenize_regex, pattern="\\s+", simplify=TRUE)
    lapply(tolower(text), tokenizers::tokenize_regex,
           pattern="\\s+", simplify=TRUE)
  }

  Tokenize2 <- function(text) {
    tokenizers::tokenize_words(text, simplify=TRUE)
  }

  Stopwords <- function(text, Tokenize=Tokenize, stopwords=mysql.stopwords) {
    ## Computes the number of stopwords present in text based on a given
    ## Tokenize function
    if (length(text) == 1) {
      tokenized <- list(Tokenize(text))
    } else {
      tokenized <- Tokenize(text)
    }
    sapply(tokenized, function(words) sum(words %in% stopwords))
  }

  Caps <- function(text) {
    ## Number of uppercase characters
    .CountRegexMatches(text, "[A-Z]")
  }

  SpecialChars <- function(text) {
    ## Number of special characters
    .CountRegexMatches(text, "[^a-zA-Z\\d\\s]")
  }

  Numbers <- function(text) {
    ## Number of digits
    .CountRegexMatches(text, "[\\d]")
  }

  CapsRatio <- function(text) Caps(text) / nchar(text)
  SpecialCharsRatio <- function(text) SpecialChars(text) / nchar(text)
  NumbersRatio <- function(text) Numbers(text) / nchar(text)
  StopwordsRatio1 <- function(text) Stopwords(text, Tokenize1) / Words(text)
  StopwordsRatio2 <- function(text) Stopwords(text, Tokenize2) / Words(text)

  Words <- function(text) {
    ## Number of words
    ## .CountRegexMatches(text, "\\w")
    .CountRegexMatches(text, "[\\s+]") + 1
  }

  AverageWordLength <- function(text) {
    nchar(text) / Words(text)
  }

  LastCharCode <- function(text) {
    ## Boolean whether last character is a character code
    ## If the line ends to emoticon this is not true :-) so we
    ## substract. R does not support lookahead in regex which would also
    ## solve this
    (!grepl("(:-\\)|;-\\)|:\\)|;\\)|:-\\(|:\\()$", text) &
     grepl("[){;]$", text))
  }

  LastCharNL <- function(text) {
    ## Last character is related to natural language (punctuation)
    grepl("\\.$|\\!$|\\?$|:$|,$", text)
  }

  First3Chars <- function(text) {
    ## First three characters (after stripping white spaces)
    substr(gsub("\\s", "", text), 1, 3)
  }

  First3CharsLetters <- function(text) {
    ## Number of characters in the firsrt three characters
    .CountRegexMatches(First3Chars(text), "[a-zA-Z]")
  }

  Emoticons <- function(text) {
    ## Number of emoticons
    ## Using more larger lis of emoticons e.g. ones built for
    ## SentiStrength, might cause more false postive as some of them are
    ## similar to elements that appear in code.
    .CountRegexMatches(text, ":-\\)|;-\\)|:\\)|;\\)|:-\\(|:\\(")
  }

  StartWithAt <- function(text) {
    ## TRUE if text starts with @ symbol
    grepl("^@", text)
    ## sapply(text, function(x) sum(grep("^@", x)))
  }
})
