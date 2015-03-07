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
                as.list(match.call()[-1])))))}
    formals(g) = c(formals(f), as.pairlist(add.args))
    g}


function (x, y = 1, ...)
{
  if (x > 1)
    print("greater")
  mc = match.call()
  do.call(f, as.list(mc)[-1])
}
