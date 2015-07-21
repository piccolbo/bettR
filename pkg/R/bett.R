qw =
  function(...)
    as.character(match.call())[-1]

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


arglist =
  function(match = FALSE) {
    call = {
      if(match)
        match.call(definition = sys.function(which = -1), call = sys.call(sys.parent()))
      else
        sys.call(which = -1)}
    lapply(as.list(call[-1]), eval.parent)}

mandatory =
  function(name)
    stop("Argument ", name, " is missing with no default")

constructor =
  function(class, fields){
    f =
      function() {
        args = arglist(match = TRUE)
        stopifnot(all(sort(names(args)) == sort(fields)))
        structure(
          args,
          class = class)}
    args = lapply(fields, function(.) alist(. = )$.)
    names(args) = fields
    departial(f, .args = args)}


