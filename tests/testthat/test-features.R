context("features")

test_that("Stopwords works", {
  text <- c("", "text", "123", "!@#$", "This is some text.",
            "This isn't some text.", "This is.")
  expect_equal(features$Stopwords(text, features$Tokenize1),
               c(0, 0, 0, 0, 3, 3, 1))
  expect_equal(features$Stopwords(text, features$Tokenize2),
               c(0, 0, 0, 0, 3, 3, 2))
})

test_that("Caps works", {
  text <- c("", "text", "123", "!@#$", "This is some text.",
            "A", "ABC", "aaABCaa")
  expect_equal(features$Caps(text),
               c(0, 0, 0, 0, 1, 1, 3, 3))
})

test_that("SpecialChars works", {
  text <- c("", "text", "123", "!@#$", "This is some text.",
            "x-y", "test;", "just some text")
  expect_equal(features$SpecialChars(text),
               c(0, 0, 0, 4, 1, 1, 1, 0))
})

test_that("Numbers works", {
  text <- c("", "text", "123", "!@#$", "This is some text.",
            "There is 1 number.")
  expect_equal(features$Numbers(text),
               c(0, 0, 3, 0, 0, 1))
})

test_that("Words works", {
  text <- c("", "text", "123", "!@#$", "This is some text.",
            "one-word.", "abc!def")
  expect_equal(features$Words(text),
               c(1, 1, 1, 1, 4, 1, 1))
})

test_that("AverageWordLength works", {
  text <- c("", "123", "123 123", "1", "!2c$", "abc def!", "1 234")
  expect_equal(features$AverageWordLength(text),
               c(0, 3, 3.5, 1, 4, 4, 2.5))
})

test_that("LastCharCode works", {
  text <- c("", "This is text.", "func(x, y);", "if (true) {",
            "func()", ":-)", "Hello ;-)", ":)", ":-(", ":(", ":-))")
  expect_equal(features$LastCharCode(text),
               c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
                 FALSE, FALSE, TRUE))
})

test_that("LastCharNL works", {
  text <- c("", "abc", "1", ".", "!", "?", "? ", ":", ",", "Hello!!")
  expect_equal(features$LastCharNL(text),
               c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
                 TRUE, TRUE))
})

test_that("LastCharCode and LastCharNL are not both true", {
  text <- c("", "This is text.", "func(x, y);", "if (true) {",
            "func()", ":-)", "Hello ;-)", ":)", ":-(", ":(", ":-))",
            "", "abc", "1", ".", "!", "?", "? ", ":", ",", "Hello!!")
  expect_true(all(!features$LastCharCode(text) | !features$LastCharNL(text)))
})

test_that("First3Chars works", {
  text <- c("", "1", "1  2345", " 12345", "      12345")
  expect_equal(features$First3Chars(text),
               c("", "1", "123", "123", "123"))
})

test_that("First3CharsLetters works", {
  text <- c("", "a", " a", "   abc", "!@#abc", " a2#d", "Hello",
            "123", "!@#", "H3ll0")
  expect_equal(features$First3CharsLetters(text),
               c(0, 1, 1, 3, 0, 1, 3, 0, 0, 2))
})

test_that("Emoticons works", {
  text <- c("", "123", "abc", ":--)", ":-)", "Hello ;-)", "\":-)\";",
            ":)", ";)", ":-(", ":(", ":(:)", ":) :) :)", ":):(:")
  expect_equal(features$Emoticons(text),
               c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 3, 2))
})

test_that("StartWithAt works", {
  text <- c("", "abc", "123", "!@#", "@abc", "@", " @")
  expect_equal(features$StartWithAt(text),
               c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
})

test_that("FeatureExtraction works", {
  text <- c("", "abc", "123", "!@#")
  features <- FeatureExtraction(text)
  expect_true(is.data.table(features))
  expect_equal(nrow(features), 4)
  expect_equal(colnames(features),
               c("ratio.caps", "ratio.specials", "ratio.numbers",
                 "length.words", "stopwords", "stopwords2",
                 "last.char.code", "last.char.nl",
                 "first.3.chars.letters", "emoticons", "first.char.at"))
  features <- FeatureExtraction(text[1])
  expect_true(is.data.table(features))
  expect_equal(nrow(features), 1)
  expect_equal(colnames(features),
               c("ratio.caps", "ratio.specials", "ratio.numbers",
                 "length.words", "stopwords", "stopwords2",
                 "last.char.code", "last.char.nl",
                 "first.3.chars.letters", "emoticons", "first.char.at"))
})

test_that("Character3Grams works", {
  text <- c("", "abcd", "1234", "!@#$", "1234abcd")
  res <- Character3Grams(text)
  expect_equal(dim(res), c(5, 8))
  expect_equal(colnames(res),
               c("0ab", "NA", "@#$", "00a", "!@#", "bcd", "abc", "000"))
})

test_that("ConvertFeatures works", {
  text <- c("", "abcd", "1234", "!@#$", "1234abcd")
  expect_true(inherits(ConvertFeatures(Character3Grams(text)), "Matrix"))
  expect_true(inherits(ConvertFeatures(TriGramsAndFeatures(text)), "Matrix"))
  expect_true(is.matrix(ConvertFeatures(as.list(FeatureExtraction(text)))))
  expect_true(is.matrix(ConvertFeatures(FeatureExtraction(text))))
  expect_error(ConvertFeatures(list(1:9, 1:10)))
  expect_error(ConvertFeatures(data.table(1:26, letters)))
})

test_that("ComputeFeatures works", {
  text <- c("", "abcd", "1234", "!@#$", "1234abcd")
  expect_true(inherits(ComputeFeatures(text, Character3Grams), "Matrix"))
  expect_true(inherits(ComputeFeatures(text, TriGramsAndFeatures), "Matrix"))
  expect_true(is.data.frame(ComputeFeatures(text, FeatureExtraction)))

  res <- ComputeFeatures(text, list(caps=features$Caps,
                                    special.chars=features$SpecialChars,
                                    numbers=features$Numbers,
                                    words=features$Words))
  expect_true(is.matrix(res))
  expect_warning(ComputeFeatures(text, list(features$Caps,
                                            features$SpecialChars,
                                            features$Numbers,
                                            features$Words)))
  expect_error(ComputeFeatures(text, list(1, 2, 3)))
})
