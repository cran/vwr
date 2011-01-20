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
Potential Hamming neighbors. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}. Targets will be converted internally with \code{\link{tpreprocess}}, but preprocessing can also be done before passing targets to the function.}
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
\code{\link{hamming.distance}}
}
\examples{
data(english.words)
hamming.neighbors('electroencephalogram',english.words)
hamming.neighbors('hello',english.words)
}

