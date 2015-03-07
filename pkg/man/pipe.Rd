\name{\%|\%}
\alias{\%|\%}
\title{
 Unix-like pipes
}
\usage{
  x \%|\% f
  x \%|\% f(y,z, ...)
  x \%|\% expr
}
\arguments{
  \item{x}{Any R object}
  \item{f}{Any R function}
  \item{y,z,...} {additional arguments to \code{f}}
  \item{expr}{An expression containing the special identifier \code{..}
}
\description{
Operator that uses the left argument as input to the right argument aka unix-like pipes without the quirks of magrittr.
}
}
\details{
  There are three possibilities, all shown in the examples below
  \enumerate{
    \item{The right argument is a function, then \code{f(x)} is evaluated}
    \item{The right argument is a call of the form \code{f(y,z, ...)}, then \code{f(x,y,z, ...)} is evaluated}
    \item{The right argument is an expression containing the special identifier \code{..}, then said expression is evaluated in an expanded environment where \code{..} evaluate to the value of \code{x}}
}}
\value{A character string}
\examples{
  4 \%|\% sqrt #2
  16 \%|\% sqrt(sqrt(..)) #2, magrittr fails on this one
  mtcars \%|\% transform(z = carb + cyl)
  #same as transform(mtcars, z = carb + cyl)
  #works with NSE, within reason
}

