\name{decorate}
\alias{decorate}
\title{
  Python-like decorators}
\usage{
decorate(f, pre = alist, post = identity,  add.args = NULL)
}
\arguments{
  \item{f} {A function (non-primitive, must work with \code{formals}) }
  \item{pre} {A function that takes in input the same arguments as function \code{f} and returns them, possibly transformed, in a named list}
  \item{post} {A function that accepts \code{f}'s return value as its only argument}
  \item{add.args} {Argument to add to the signature of \code{f}}
  \description{
  Closely modeled after Python's decorator, decorate transfrom a function into another with the same or expanded signature and adds optional preprocessing of the arguments and postprocessing of results.
  }
}
\details{
}
\value{Any R object}
\examples{

}


