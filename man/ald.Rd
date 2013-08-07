\encoding{utf-8}
\name{ald}
\alias{ald}
\alias{old20}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute average Levenshtein distances}
\description{
Compute the average Levenshtein distances between a word and its n nearest neighbors in a lexicon.}
\usage{
ald(sources, targets, n, method="levenshtein", parallel = FALSE)
old20(sources, targets, method="levenshtein", parallel = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sources}{
a list of words for which the average Levenshtein distance should be computed. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.
}
  \item{targets}{
a list of words containing possible neighbors. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.
}
  \item{method}{
specifies the distance function. With "levenshtein", \code{levenshtein.distance} is used, with "levenshtein.damerau" \code{levenshtein.damerau} is used.}
  \item{n}{
specifies the number of nearest neighbors on which the average should be based. The variant \code{old20} does not take the \code{n} argument (it is fixed to 20). 
}
  \item{parallel}{
with parallel=TRUE, ald will run in parallel an multiple cores. The number of parallel processes is specified by \code{detectCores(logical = FALSE)}.
}
}
\details{
The OLD20 measure was originally proposed by Yarkoni et al. (2008). This implementation is orders of magnitude faster than Tal Yarkoni's LDcalc program (see http://talyarkoni.com/materials.php).
Do not use multicore=TRUE in a GUI environment, as it will most likely crash your R session. 
}
\value{
A vector of average Levenshtein distances with names corresponding to sources.
}
\references{
Yarkoni, T., Balota, D., & Yap, M. (2008). Moving beyond Coltheart’s N: A new measure of orthographic similarity. Psychonomic Bulletin & Review, 15(5), 971–979.
}
\author{
Emmanuel Keuleers
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{levenshtein.distance}}, \code{\link{levenshtein.neighbors}}
}
\examples{
data(basque.words)
ald(basque.words[1:10],basque.words,20)
old20(basque.words[1:10],basque.words)
}