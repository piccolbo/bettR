bettR is a collection of useful R language extensions that make a developer's life easier and his or her code better, that is smaller. Paired with fixR, which fixes  bugs in R or its definition, it makes life programming in R more bearable, and at times even pleasant. Right now it looks like a  number of helpful constructs from other languages: pipes from Unix, decorators from Python and `qw` from Perl. This is not by design, just the way things are going.

For example, imagine that we want to create an abstraction for `suppressWarnings(library(x))`. Easy-peasy right? `library.quiet = function(x) suppressWarnings(library(x))`. Well, it depends where we set the bar. For instance, `library.quiet` doesn't accept any of the other arguments that `library` does. Try this: `library.quiet = function(...) suppressWarnings(library(...))`. Great now `library.quiet` accepts the same arguments as `library`, much better, but it has a different signature. The completion, for instance, doesn't work. If we call `args` on it, it doesn't show anything interesting. This is `bettR` and we are aiming for perfection --- not achieveing it, just aiming. Enter `decorate`. 



```r
library(bettR)
library.quiet  = decorate(library, post = suppressPackageStartupMessages)
args(library)
```

```
## function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
##     logical.return = FALSE, warn.conflicts = TRUE, quietly = FALSE, 
##     verbose = getOption("verbose")) 
## NULL
```

```r
args(library.quiet)
```

```
## function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE, 
##     logical.return = FALSE, warn.conflicts = TRUE, quietly = FALSE, 
##     verbose = getOption("verbose")) 
## NULL
```

Now this new function has the same signature as library, accepts the same arguments and doesn't emit pesky startup messages. Eureka. With decorate we can pre-process the arguments, post-process the return value and add additional arguments. 
