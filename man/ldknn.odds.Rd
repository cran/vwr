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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Emmanuel Keuleers}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ldknn}}
}
