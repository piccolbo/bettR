#"Compose"                "Curry"                  "CurryL"                 "Identity"               "multi.argument.Compose"
#Reduce(f, x, init, right = FALSE, accumulate = FALSE)
# Filter(f, x)
# Find(f, x, right = FALSE, nomatch = NULL)
# Map(f, ...)
# Negate(f)
# Position(f, x, right = FALSE, nomatch = NA_integer_)


#UnCurry adds argument to a function, instead of removing them
#workflow composes functions of multiple args by name
#
#

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

nodefault =
  function(argname)
    stop("Argument ", argname, "missing with no default")

deapply =
  function(x, ..., .args = alist()) {
    formals(x) = c(formals(x), pryr::dots(...), .args)
    x }

partial =
  function(x, ..., .args = list()) {
    #get matched args for x from ...
    args =
      as.list(
        match.call(
          x,
          do.call(
            call,
            c(list("x"), list(...), .args),
            quote = TRUE)))[-1]
    # tuck those args in an env under current x's env hierarchy
    environment(x) = list2env(args, NULL, environment(x))
    # zap them from signature
    ii = match(names(args), names(formals(x)))
    formals(x) = formals(x)[-ii]
    #dish out
    x}

partial2 =
  function(x, ..., .args = alist()) {
    #get matched args for x from ...
    args =
      as.list(
        match.call(
          x,
          do.call(
            call,
            c(list("x"), pryr::named_dots(...), .args),
            quote = TRUE)))[-1]
    ii = purrr::discard(match(names(args), names(formals(x))), is.na)
    ii = if(length(ii) > 0) -ii else T
    pryr::make_function(
      formals(x)[ii],
      pryr::make_call(
        x,
        c(args, lapply(names(formals(x)[ii]), as.name))))}

curry =
  function(f) {
    formf = formals(f)
    if(length(formf) <= 1)
      f
    else {
      pryr::make_function(
        formf[1],
        quote({
          args = arglist()
          curry(
            partial(
              f,
              .args = args))}))}}
