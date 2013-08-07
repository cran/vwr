\encoding{utf-8}
\name{levenshtein.damerau.distance}
\alias{levenshtein.damerau.distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Levenshtein-Damerau distances }
\description{
Compute the Levenshtein-Damerau distance between two character strings (the minimal number of insertions, deletions replacements, or transpositions required to transform one string into the other)}
\usage{
levenshtein.damerau.distance(xsource, targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xsource}{
A character string to compute the Levenshtein-Damerau distance from.
}
  \item{targets}{
A list of words to compute the Levenshtein-Damerau distance to. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.}
}
\details{
The distance computation is performed by {\link{stringdist}} with method="osa". Note that this function computes the restricted Levenshtein-Damerau distance instead of the unrestricted version.} 
\value{
An integer vector containing Restricted Levenshtein-Damerau distances, with names corresponding to targets.
}
\references{
Damerau, F. J. (1964). A technique for computer detection and correction of spelling errors. Communications of the ACM, 7(3), 171—176.
}
\author{
Emmanuel Keuleers}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{levenshtein.neighbors},\link{levenshtein.distance}, \link{stringdist}, \link{ald}} 
}
\examples{
data(french.words)
levenshtein.damerau.distance('pourquoi',sample(french.words,20))
}
