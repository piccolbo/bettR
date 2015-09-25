help = function(x, ...) UseMethod("help")

help.Function = function(x, ...)
  attr(x, "help", exact = TRUE)

help.Argument = function(x, ...) x$help

help.default =
  function(x, ...) {
    x = as.character(substitute(x))
    utils::help(x, ...)}
