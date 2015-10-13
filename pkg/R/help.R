help = function(x, ...) UseMethod("help")

help.Function = function(x, ...)
  attr(x, "help", exact = TRUE)

help.Argument = function(x, ...) x$help

help.default =
  function(x, ...) {
    x = as.character(substitute(x))
    utils::help(x, ...)}

Help =
  function(
    title,
    description,
    value,
    arguments = NULL,
    details = NULL,
    see.also = NULL,
    examples = NULL)
    structure(
      all.args(Help, match.call()),
      class = "Help")


print.Help =
  function(x, ...)
    lapply(x, print)

title = function(x)  paste("#", x, "\n")
subtitle = function(x)  paste("##", x, "\n")
paragraph = function(x) paste(x, "\n")
view =
  function(x) {
    h = attributes(x)$help
    view = getOption("viewer")
    markdown =
      paste0(
        title(h$title),
        subtitle("Description"),
        paragraph(h$description),
        subtitle("Usage"),
        paragraph(h$usage),
        subtitle("Arguments"),
        paragraph(h$arguments),
        subtitle("Details"),
        paragraph(h$details),
        subtitle("See Also"),
        paragraph(h$see.also),
        subtitle("Examples"),
        paragraph(h$examples))
    tmpin = tempfile(fileext = ".md")
    tmpout = tempfile(fileext = ".html")
    writeLines(text = markdown, con = file(tmpin))
    renderMarkdown(tmpin, tmpout)
    view(tmpout)}
