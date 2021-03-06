# function families
# create families of function around reusable element such as arguments

# a default value for a mandatory argument
nodefault =
  mandatory =
  function(name)
    stop("Argument ", name, " missing with no default")

# reusable argument
A = Argument =
  function(
    name, #name of the argument
    priority, #priority when deciding order
    default = ~mandatory(name), #default value
    validate = function(x) TRUE, #validate argument
    process = identity,
    help = NULL) { #transform argument
    args = all.args(A,  match.call())
    stopifnot(identical(default, ~mandatory(name)) || validate(default))
    structure(
      args,
      class = "Argument")}

as.Argument = function(x, ...) UseMethod("as.Argument")

as.Argument.default =
  function(x, ...)
    as.Argument(as.list(x))

# reusable function defs
F = Function =
  function(..., body = NULL, export = TRUE, help = NULL, tests = list(), precondition = NULL, postcondition = NULL, args = NULL) {
    fargs = list(...)
    names(fargs) = map(fargs, "name")
    if(is.null(body)){
      body = tail(fargs, 1)[[1]]
      fargs = fargs[-length(fargs)]}
    fargs = c(fargs, args)
    pre =
      function(){
        args = all.args(pre, match.call())
        setNames(
          lapply(
            names(args),
            function(n) {
              stopifnot(fargs[[n]]$validate(args[[n]]))
              stopifnot(is.null(precondition) || precondition(args))
              fargs[[n]]$process(args[[n]])}),
          nm = names(args))}
    core = function(){}
    body(core) = as.list(body)[[2]]
    retval = function() {
      retval = do.call(core, do.call(pre, all.args(retval, match.call())))
      stopifnot(is.null(postcondition) || postcondition(retval))
      retval}
    vals = map(fargs, "default")
    formals(pre) =
      formals(core) =
      formals(retval) =
      setNames(
        object = vals ,
        nm = map(fargs, "name"))
    if(is.null(help$args))
      help$args = paste("\n", names(fargs), "\n:   ", unlist(map(fargs, bettR::help)))
    if(is.null(help$usage))
      help$usage = paste(head(deparse(args(core)), -1), collapse = "\n")
    structure(
      retval,
      class = "Function",
      help = help,
      tests = tests)}
