\encoding{utf-8}
\name{coltheart.N}
\alias{coltheart.N}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Coltheart's N}
\description{
Compute Coltheart's N measure (the number of neighbors at distance 1).}
\usage{
coltheart.N(sources, targets, multicore = FALSE, 
distance = 1, method = "Hamming")
}
\arguments{
  \item{sources}{
a list of words for which Coltheart's N should be computed. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.
}
  \item{targets}{
a list of words containing possible neighbors. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.
}
  \item{multicore}{
with multicore=TRUE, function will run on multiple cores simultaneously.
}
  \item{distance}{
specifies the distance on which N should be based. This should be left to 1 to compute the original measure.
}
  \item{method}{
with method="Hamming", compute N based on the \code{\link{hamming.distance}},
with method="Levenshtein", compute N based on the \code{\link{levenshtein.distance}}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An integer vector with names corresponding to sources.
}
\references{
Coltheart, M., Davelaar, E., Jonasson, J. T., & Besner, D. (1977). Access to the internal lexicon. Attention and performance VI, 535–555.
}
\author{
Emmanuel Keuleers}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hamming.distance}},\code{\link{levenshtein.distance}}
}
\examples{
data(spanish.words)
sample.words<-sample(spanish.words,20)
coltheart.N(sample.words,spanish.words)
coltheart.N(sample.words,spanish.words, method='Levenshtein')
}

