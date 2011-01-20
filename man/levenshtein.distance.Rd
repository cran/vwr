\encoding{utf-8}
\name{levenshtein.distance}
\alias{levenshtein.distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Levenshtein distances }
\description{
Compute the Levenshtein distance between two character strings (the minimal number of insertions, deletions or replacements required to transform one string into the other)}
\usage{
levenshtein.distance(source, targets)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{source}{
A character string to compute the Levenshtein distance from.
}
  \item{targets}{
A list of words to compute the Levenshtein distance to. Must be of type \code{character}, or convertible to type \code{character} with \code{as.character}.}
}
\details{
Uses the \code{\link{levenshteinDist}} function from the \code{RecordLinkage} package and adds target names to the vector with distances.}
\value{
An integer vector containing Levenshtein distances, with names corresponding to targets.
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
\code{\link{levenshtein.neighbors}},\code{\link{levenshteinDist}}, \code{\link{ald}} 
}
\examples{
data(french.words)
levenshtein.distance('pourquoi',sample(french.words,20))
}
