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
  function(..., body = NULL, export = TRUE, help = NULL, tests = list()) {
F = Function =
    fargs = list(...)
    names(fargs) = map(fargs, "name")
    if(is.null(body)){
      body = tail(fargs, 1)[[1]]
      fargs = fargs[-length(fargs)]}
    pre =
      function(){
        args = all.args(pre, match.call())
        setNames(
          lapply(
            names(args),
            function(n) {
              stopifnot(fargs[[n]]$validate(args[[n]]))
              fargs[[n]]$process(args[[n]])}),
          nm = names(args))}
    core = function(){}
    body(core) = as.list(body)[[2]]
    retval = function() {
      do.call(core, do.call(pre, all.args(retval, match.call())))}
    vals = map(fargs, "default")
    formals(pre) =
      formals(core) =
      formals(retval) =
      setNames(
        object = vals ,
        nm = map(fargs, "name"))
    structure(
      retval,
      class = "Function",
      help = paste(help, names(fargs), unlist(map(fargs, bettR::help))),
      tests = tests)}
