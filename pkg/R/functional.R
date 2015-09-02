#decorators a la Python
# f function to decorate
# pre  preprocessing of args
# post postprocessing of function retval
# add.args additional args to the decorated function
decorate =
  function(f, pre = alist, post = identity, add.args = NULL) {
    g =
      function() {
        post(
          do.call(
            f,
            as.list(
              do.call(
                pre,
                arglist(TRUE)))))}
    formals(g) = c(formals(f), as.pairlist(add.args))
    g}

# function combinators HIGHLY EXPERIMENTAL
# function that does all that f and g and has all of their args
# returns a concatenation of retval of retvals
parallel =
  function(f,g) {
    h =
      function(){
        args = arglist(TRUE)
        c(
          do.call(f, args[names(formals(f))]),
          do.call(g, args[names(formals(g))]))}
    formals(h) = c(formals(f), formals(g))
    h}

# same as above but this take lists of arguments to pass to each function and returns retvals in a list
# the other one is flatter, this one more general
parallel2 =
  function(f,g)
    function(left, right)
      list(
        left = do.call(f, as.list(left)),
        right = do.call(g, as.list(right)))

#piping for list functions
#make the output of one function the argument list for the next
#allows renaming of list elements
pipe2 =
  function(g, f, argmap = NULL) {
    h =
      function(){
        args = as.list(sys.call())[-1]
        do.call(
          f,
          if(is.null(argmap))
            do.call(g, args)
          else
            plyr::rename(do.call(g, args), argmap))}
    formals(h) = formals(g)
    h}

#same for many functions
# original use of ...
# fun1 fun2 optional argmap fun3 optional argmap ...
pipe =
  function(...){
    args = list(...)
    first = {
      if(length(args) == 2 || is.function(args[[2]]))
        pipe2(args[[1]], args[[2]])
      else
        pipe2(args[[1]], args[[3]], argmap = args[[2]])}
    if(length(args) < 4)
      first
    else
      pipe(first, args[(if(is.function(args[[2]])) 3 else 4):length(args)])}

#pipe operator a la magrittr, only simple to define and understand
pipe2a =
  `%|%` =
  function(left, right){
    subsright = substitute(right)
    lazyright = lazy(right)
    if(".." %in% all.vars(subsright))
      lazy_eval(lazyright, list(.. = left))
    else {
      if(is.call(subsright)) {
        lsr = as.list(subsright)
        do.call(
          as.character(
            lsr[1]),
          c(substitute(left),
            lsr[-1]),
          envir = lazyright$env)}
      else{
        if(is.function(right))
          right(left)
        else
          stop("Don't know how to pipe THAT!")}}}

# the opposite of partial
# not sure what it means exactly yet
deapply =
  function(x, ..., .args = alist()) {
    formals(x) = c(formals(x), dots(...), .args)
    x }

# partial application done right
# keeps all the relevant args in the signature
# see http://piccolboni.info/2015/07/delicious-r-curry.html
partial =
  function(f, ..., .args = alist()) {
    #get  args to apply f to first from ... and .args via matching
    .applied =
      as.list(
        match.call(
          f,
          make_call("f", c(dots(...), .args))))[-1]
    #rest to be applied to later
    formf = formals(f)
    ii = discard(match(names(.applied), names(formf)), is.na)
    ii = if(length(ii) > 0) -ii else TRUE
    .unapplied = formf[ii]
    #make function of later args
    pf = parent.frame()
    make_function(
      .unapplied,
      make_call(
        f,
        c(.applied, lapply(names(.unapplied), as.name))),
      env = pf)}

# real curry
# see http://piccolboni.info/2015/07/delicious-r-curry.html
curry =
  function(f) {
    formf = formals(f)
    lff = length(formf)
    if(lff == 0 || (lff == 1 && names(formf) != "..."))
      f
    else {
      make_function(
        formf[1],
        quote({
          args = arglist(lazy = TRUE)
          if(length(args) > 0)
            curry(
              partial(
                f,
                .args = args))
          else
            f()}))}}

# make a function return a partial application when mandatory args are missing
autopartial =
  function(f) {
    ap =
      function() {
        args = arglist(lazy = TRUE)
        mandatory =
          keep(
            formals(f),
            function(x) is.name(x) && as.character(x) == "")
        if(all(names(mandatory) %in% names(args)))
          do.call(f, args)
        else
          do.call(partial, list(f = f, .args = args))}
    formals(ap) = formals(f)
    ap}
