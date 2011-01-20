\name{levenshtein.neighbors}
\alias{levenshtein.neighbors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Levenshtein neighbors
}
\description{
List the neighbors of a character string by Levenshtein distance.
}
\usage{
levenshtein.neighbors(source, targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source}{
A character string.
}
  \item{targets}{
Potential Levenshtein neighbors. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of neighbors at each distance.}

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
\code{\link{levenshtein.distance}}
}

\examples{
data(serbian_latin.words)
levenshtein.neighbors('pola',serbian_latin.words)[1:2]
}

