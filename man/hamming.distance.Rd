\name{hamming.distance}
\alias{hamming.distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Hamming distances
}
\description{
Compute the Hamming distance (the number of non-overlapping characters) between words of the same length.}
\usage{
hamming.distance(source, targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source}{
A character string to compute the Hamming distance from.
}
  \item{targets}{
Words to which the Hamming distance must be computed. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}. Targets will be converted internally with \code{\link{tpreprocess}}, but preprocessing can also be done before passing targets to the function.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An integer vector containing Hamming distances, with names corresponding to targets. Since the Hamming distances is only defined between words of the same length,  the output of hamming.distance is only guaranteed to have the same length as \code{targets} if all \code{targets} have the same length as \code{source}. 
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
\code{\link{hamming.neighbors}}, \code{\link{coltheart.N}}
}
\examples{
data(english.words)
hamming.distance('electroencephalogram',english.words)
}
