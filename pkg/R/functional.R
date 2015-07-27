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
    formals(x) = c(formals(x), dots(...), .args)
    x }

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
