


missing_fill_blank <- function(arg) {
  if (missing(arg)) {
    arg <- ""
  } else {
    arg <- deparse(substitute(arg))
  }
}


