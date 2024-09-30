#' Add to a variable in-place
#'
#' This operator adds the right-hand side to the left-hand side variable in-place.
#'
#' @param t The variable to be modified.
#' @param s The value to be added.
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- 5
#' x %+=% 3
#' print(x)  # Output: 8
#'
#' @export
`%+=%` <- function(t, s) eval.parent(substitute(t <- t + s))

#' Subtract from a variable in-place
#'
#' This operator subtracts the right-hand side from the left-hand side variable in-place.
#'
#' @param t The variable to be modified.
#' @param m The value to be subtracted.
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- 10
#' x %-=% 4
#' print(x)  # Output: 6
#'
#' @export
`%-=%` <- function(t, m) eval.parent(substitute(t <- t - m))

#' Concatenate to a variable in-place
#'
#' This operator concatenates the right-hand side to the left-hand side variable in-place.
#'
#' @param t The variable to be modified.
#' @param a The value(s) to be concatenated.
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- c(1, 2, 3)
#' x %c=% c(4, 5)
#' print(x)  # Output: 1 2 3 4 5
#'
#' @export
`%c=%` <- function(t, a) eval.parent(substitute(t <- c(t, a)))

#' Union with a variable in-place
#'
#' This operator performs a union of the right-hand side with the left-hand side variable in-place.
#'
#' @param t The variable to be modified.
#' @param a The value(s) to be unioned.
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- c(1, 2, 3)
#' x %union=% c(3, 4, 5)
#' print(x)  # Output: 1 2 3 4 5
#'
#' @export
`%union=%` <- function(t, a) eval.parent(substitute(t <- union(t, a)))

#' Add to a variable by name
#'
#' This function adds a value to a variable specified by its name.
#'
#' @param t A string containing the name of the variable to be modified.
#' @param s The value to be added.
#' @param env The environment in which to look for the variable (default: parent.frame()).
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- 5
#' assignPlus("x", 3)
#' print(x)  # Output: 8
#'
#' @export
assignPlus <- function(t, s, env = parent.frame()) {
  assertString(t)
  assign(t, get(t, env) + s, env)
}

#' Subtract from a variable by name
#'
#' This function subtracts a value from a variable specified by its name.
#'
#' @param t A string containing the name of the variable to be modified.
#' @param m The value to be subtracted.
#' @param env The environment in which to look for the variable (default: parent.frame()).
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- 10
#' assignMinus("x", 4)
#' print(x)  # Output: 6
#'
#' @export
assignMinus <- function(t, m, env = parent.frame()) {
  assertString(t)
  assign(t, get(t, env) - m, env)
}

#' Concatenate to a variable by name
#'
#' This function concatenates a value to a variable specified by its name.
#'
#' @param t A string containing the name of the variable to be modified.
#' @param a The value(s) to be concatenated.
#' @param env The environment in which to look for the variable (default: parent.frame()).
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- c(1, 2, 3)
#' assignC("x", c(4, 5))
#' print(x)  # Output: 1 2 3 4 5
#'
#' @export
assignC <- function(t, a, env = parent.frame()) {
  assertString(t)
  assign(t, c(get(t, env), a), env)
}

#' Union with a variable by name
#'
#' This function performs a union of a value with a variable specified by its name.
#'
#' @param t A string containing the name of the variable to be modified.
#' @param a The value(s) to be unioned.
#' @param env The environment in which to look for the variable (default: parent.frame()).
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- c(1, 2, 3)
#' assignUnion("x", c(3, 4, 5))
#' print(x)  # Output: 1 2 3 4 5
#'
#' @export
assignUnion <- function(t, a, env = parent.frame()) {
  assertString(t)
  assign(t, union(get(t, env), a), env)
}

#' Set difference with a variable in-place
#'
#' This operator performs a set difference of the left-hand side variable with the right-hand side in-place.
#'
#' @param t The variable to be modified.
#' @param a The value(s) to be removed from t.
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' x %setdiff=% c(3, 4)
#' print(x)  # Output: 1 2 5
#'
#' @export
`%setdiff=%` <- function(t, a) eval.parent(substitute(t <- setdiff(t, a)))

#' Set difference with a variable by name
#'
#' This function performs a set difference of a variable specified by its name with the given values.
#'
#' @param t A string containing the name of the variable to be modified.
#' @param a The value(s) to be removed from the variable.
#' @param env The environment in which to look for the variable (default: parent.frame()).
#'
#' @return The modified variable (invisibly).
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' assignSetdiff("x", c(3, 4))
#' print(x)  # Output: 1 2 5
#'
#' @export
assignSetdiff <- function(t, a, env = parent.frame()) {
  assertString(t)
  assign(t, setdiff(get(t, env), a), env)
}

