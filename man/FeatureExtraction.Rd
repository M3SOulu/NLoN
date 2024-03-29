% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features.R
\name{FeatureExtraction}
\alias{FeatureExtraction}
\title{Feature extraction.}
\usage{
FeatureExtraction(text)
}
\arguments{
\item{text}{The text.}
}
\value{
A data.table with values of the 11 features.
}
\description{
Computes a set of simple text-based features.
}
\details{
The features computed are the followings:
\describe{
  \item{\code{ratio.caps}}{The ratio of uppercase letters.}
  \item{\code{ratio.specials}}{The ratio of special characters.}
  \item{\code{ratio.numbers}}{The ratio of number characters.}
  \item{\code{length.words}}{The average word length.}
  \item{\code{stopwords}}{The ratio of English stopwords (using first
  tokenizer).}
  \item{\code{stopwords2}}{The ratio of English stopwords (using second
  tokenizer).}
  \item{\code{last.char.nl}}{Boolean for the use of NL character at the
  end of the text.}
  \item{\code{last.char.code}}{Boolean for the use of code character at
  the end of text.}
  \item{\code{first.3.chars.letters}}{Number of letters in the three
  first characters.}
  \item{\code{emoticons}}{Number of emoticons}
  \item{\code{first.char.at}}{Boolean for the use of @ character at
  the beginning of the line.}
}
}
