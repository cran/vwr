\name{hamming.distance}
\alias{hamming.distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Hamming distances
}
\description{
Compute the Hamming distance (the number of non-overlapping characters) between words of the same length.}
\usage{
hamming.distance(xsource, targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xsource}{
A character string to compute the Hamming distance from.
}
  \item{targets}{
Words to which the Hamming distance must be computed. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.}
}
\details{
The actual distance computation is performed by {\link{stringdist}} with "method='h'". 
}
\value{
An integer vector containing Hamming distances, with names corresponding to targets. Since the Hamming distances is only defined between words of the same length,  the output of hamming.distance is only guaranteed to have the same length as \code{targets} if all \code{targets} have the same length as \code{source}. 
}
\references{
Hamming, R. W. (1950). Error detecting and error correcting codes. Bell System technical journal, 29(2), 147-160.}
\author{
Emmanuel Keuleers}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{stringdist}, \link{hamming.neighbors}}, \code{\link{coltheart.N}}
}
\examples{
data(english.words)
targets<-english.words[which(nchar(english.words)==5)]
hamming.distance('electroencephalogram',english.words)
}
