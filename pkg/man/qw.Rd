\name{qw}
\alias{qw}
\title{
Quote multiple strings
}
\usage{
qw(a, b, c)
}
\arguments{
\item{a,b,c}{Valid R names}
}
\description{
Quote multiple tokens without typing the actual quotes.
}
\details{
 For the Perl survivors.
}
\value{A character string}
\examples{
qw(a, b, c, def)
#instead of
c("a", "b", "c", "def")
# your fingers will be grateful
}

