\name{ldknn.odds}
\alias{ldknn.odds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the odds of correctly predicting a response
}
\description{
Perform a logistic regression to compute the odds of correctly predicting a particular response. Used internally by ldknn.run. Not intended for separate use
}
\usage{
ldknn.odds(type, probability, reference)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
A factor corresponding to the true class for each response.}
  \item{probability}{
Probability of the reference level response.}
  \item{reference}{
A character string corresponding to the reference level.
}
}



\author{
Emmanuel Keuleers}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ldknn}}
}
