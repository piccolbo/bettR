tests  = function(x, ...) UseMethod("tests")

tests.Function = function(x, ...) attr(x, "tests", exact = TRUE)

