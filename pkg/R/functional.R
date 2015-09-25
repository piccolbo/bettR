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
    names(.args) = {
      if(is.null(names(.args))) as.character(.args)
      else
        ifelse(names(.args) == "", as.character(.args), names(.args))}
    all.args = c(pryr::named_dots(...), .args)
    applied =
      as.list(
        match.call(
          f,
          make_call("f", all.args)))[-1]
    #      c(dots(...), .args)
    #rest to be applied to later
    formf = formals(f)
    ii = discard(match(names(applied), names(formf)), is.na)
    ii = if(length(ii) > 0) -ii else TRUE
    unapplied = formf[ii]
    #make function of later args
    pf = parent.frame()
    unapplied.names =  lapply(names(unapplied), as.name)
    make_function(
      unapplied,
      make_call(
        f,
        c(applied, unapplied.names)),
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

#return all args to a function, evaluated, as a list
all.args =
  function(fun, matched.call) {
    args = formals(fun)
    actual.args = as.list(matched.call)[-1]
    nact = names(actual.args)
    args[nact] = actual.args[nact]
    lapply(args, eval, envir = parent.frame(2))}

# a different version of the above that tries to do without arguments and has
# a lazy version
#
arglist =
  function(match = FALSE, lazy = FALSE) {
    call = {
      if(match)
        match.call(definition = sys.function(which = -1), call = sys.call(sys.parent()))
      else
        sys.call(which = -1)}
    lazy.args = as.list(call[-1])
    if(lazy)
      lazy.args
    else
      lapply(lazy.args, eval.parent)}




