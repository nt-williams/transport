check_binary <- function(x, any.missing = FALSE) {
  assert_numeric(x, any.missing = any.missing)
  ux <- unique(x)
  # Check if there are exactly two unique values and they are not 0 and 1
  if (length(ux) != 2 && !all(ux %in% c(0, 1))) {
    return("The variable must contain exactly two unique values: 0 and 1")
  }

  TRUE
}

assert_binary <- makeAssertionFunction(check_binary)
