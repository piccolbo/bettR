qw =
  function(...)
    as.character(match.call())[-1]


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

named.args =
  function(...)
    lapply(pryr::named_dots(...), eval, envir = parent.frame())

#OOP

getters =
  function(x, names = NULL, create = FALSE) {
    if(missing(names)) {
      names ={
        if(isS4(x))
          slotNames(x)
        else
          names(x)}}
    funs =
      setNames(
      map(
        names,
        ~function(x) if(isS4(x)) slot(x, .) else x[[.]]),
      names)
    if(create)
      map(names, ~assign(., funs[[.]], envir = parent.frame()))}



