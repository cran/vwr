\name{levenshtein.damerau.neighbors}
\alias{levenshtein.damerau.neighbors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Levenshtein-Damerau neighbors
}
\description{
List the neighbors of a character string by Levenshtein-Damerau distance.
}
\usage{
levenshtein.damerau.neighbors(xsource, targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xsource}{
A character string.
}
  \item{targets}{
Potential neighbors. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.}
}


\value{
A list of neighbors at each distance.}


\author{
Emmanuel Keuleers}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{levenshtein.damerau.distance}}
}

\examples{
data(serbian_latin.words)
levenshtein.neighbors('pola',serbian_latin.words)[1:2]
}

