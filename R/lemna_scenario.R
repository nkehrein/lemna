
#' Lemna scenario constructor
#'
#' @param init initial state of the model variables
#' @param times numeric vector, output times for which model results are returned
#' @param param named list, Lemna model parameters
#' @param envir named list, contains time series data for each of the five
#'   environmental variables
#'
#' @return a `lemna_scenario` object
#' @export
new_lemna_scenario <- function(init=c(BM=0, M_int=0), times=c(), param=list(), envir=list()) {
  if(!is.numeric(init))
    stop("init state is not numeric")
  if(!all(c("BM", "M_int") %in% names(init)))
    stop("one or more init variables missing")
  if(!is.numeric(times))
    stop("times is not numeric")
  if(length(times) < 2)
    stop("times is too short")

  structure(list(init=init, times=times, param=param, envir=envir),
            class = c("lemna_scenario")
  )
}
