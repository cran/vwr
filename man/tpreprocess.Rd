\name{tpreprocess}
\alias{tpreprocess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internally used to preprocess targets for \code{hamming.distance}, \code{hamming.neighbors}, and \code{coltheart.N}
}
\description{
Preprocesses targets for functions involving computation of the Hamming distance.}
\usage{
tpreprocess(targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{targets}{
a list of character strings. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.}
}
\details{
\code{tpreprocess} exists because splitting strings and computing their length every time becomes an expensive operation if it has to be done thousands of times. \code{tpreprocess} does this once and stores the results for further use.}
\value{
A list with class \code{preprocessed.targets} with the following components:
\item{names}{The names of the targets}
\item{spelling}{The spelling of the targets, split into individual characters}
\item{n.chars}{The number of characters of the targets}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
