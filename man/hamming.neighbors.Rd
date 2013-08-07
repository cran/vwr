\name{hamming.neighbors}
\alias{hamming.neighbors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Hamming neighbors
}
\description{
List the neighbors of a character string by Hamming distance.}
\usage{
hamming.neighbors(source, targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source}{
A character string.
}
  \item{targets}{
Potential Hamming neighbors.}
}


\value{
A list of neighbors at each distance.}

\author{
Emmanuel Keuleers}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hamming.distance}, \link{stringdist}}
}
\examples{
data(english.words)
hamming.neighbors('electroencephalogram',english.words)
hamming.neighbors('hello',english.words)
}

