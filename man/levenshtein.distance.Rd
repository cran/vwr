\encoding{utf-8}
\name{levenshtein.distance}
\alias{levenshtein.distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Levenshtein distances }
\description{
Compute the Levenshtein distance between two character strings (the minimal number of insertions, deletions or replacements required to transform one string into the other)}
\usage{
levenshtein.distance(xsource, targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xsource}{
A character string to compute the Levenshtein distance from.
}
  \item{targets}{
A list of words to compute the Levenshtein distance to. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.}
}
\details{
The distance computation is performed by {\link{stringdist}} with method="lv".
}
\value{
An integer vector containing Levenshtein distances, with names corresponding to targets.
}
\references{
Levenshtein, V. I. (1966, February). Binary codes capable of correcting deletions, insertions and reversals. In Soviet physics doklady (Vol. 10, p. 707).}
\author{
Emmanuel Keuleers}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{levenshtein.neighbors}},\code{\link{stringdist}}, \code{\link{ald}}, \code{\link{levenshtein.damerau.distance} } 
}
\examples{
data(french.words)
levenshtein.distance('pourquoi',sample(french.words,20))
}
