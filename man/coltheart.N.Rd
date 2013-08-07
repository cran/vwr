\encoding{utf-8}
\name{coltheart.N}
\alias{coltheart.N}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Coltheart's N}
\description{
Compute Coltheart's N measure (the number of neighbors at distance 1).}
\usage{
coltheart.N(sources, targets, distance = 1, method = "hamming", parallel = FALSE)
}
\arguments{
  \item{sources}{
a list of words for which Coltheart's N should be computed. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.
}
  \item{targets}{
a list of words containing possible neighbors. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.
}
  \item{parallel}{
with parallel=TRUE, \code{coltheart.N} will run in parallel an multiple cores. The number of parallel processes is specified by \code{detectCores(logical = FALSE)}.
}
\item{distance}{
specifies the distance on which N should be based. This should be left to 1 to compute the original measure.
}
  \item{method}{
with method="hamming", compute N based on the \code{\link{hamming.distance}},
with method="levenshtein", compute N based on the \code{\link{levenshtein.distance}}
with method="levenshtein-damerau", compute N based on the \code{\link{levenshtein.damerau.distance}}
}
}

\value{
An integer vector with names corresponding to sources.
}
\references{
Coltheart, M., Davelaar, E., Jonasson, J. T., & Besner, D. (1977). Access to the internal lexicon. Attention and performance VI, 535–555.
}
\author{
Emmanuel Keuleers}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hamming.distance}},\code{\link{levenshtein.distance}}
}
\examples{
data(spanish.words)
sample.words<-sample(spanish.words,20)
coltheart.N(sample.words,spanish.words)
coltheart.N(sample.words,spanish.words, method='levenshtein')
}

