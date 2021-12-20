#' Effects on biomass
#'
#' Two endpoints are calculated which describe the effects on biomass:
#' - `BM`, percent effect on biomass at the last time step of the simulation
#' - `r`, percent effect on the average growth rate of biomass
#'
#' @param init initial state of the model variables
#' @param times numeric vector, output times for which model results are returned
#' @param param named list, Lemna model parameters
#' @param envir named list, contains time series data for each of the five
#'   environmental variables
#' @param duration optional `numeric`, length of the simulated period to consider
#'   for effect calculation, period starts at the beginning of the simulation
#' @param ... additional parameters passed on to [lemna()] and [deSolve::ode()]
#' @return `numeric`, effect on biomass in percent (%) \[0,100\]
#' @export
#'
#' @examples
#' # effects in sample scenario
#' effect(metsulfuron)
#'
#' # effects with modified environmental data
#' myenvir <- metsulfuron$envir
#' myenvir$tmp <- 20   # increase to 20°C
#' myenvir$conc <- 0.3  # constant exposure of 0.3 ug/L
#' effect(metsulfuron, envir=myenvir)
#'
#' # calculate effects for the first seven days
#' effect(metsulfuron, duration=7)
effect <- function(...) {
  UseMethod("effect")
}

# Default method to derive effects, all available info is given in the generic
#' @describeIn effect All scenario parameters supplied as arguments
#' @importFrom utils tail
#' @export
effect.default <- function(init, times, param, envir, duration, ...) {
  if(!missing(duration)) {
    if(max(times) - min(times) < duration)
      stop("duration is longer than simulated period")

    t_start <- min(times)
    times <- times[times <= t_start + duration]
    # make sure that the end of simulation hits the right time point
    if(max(times) < t_start + duration) {
      times <- c(times, t_start + duration)
    }
  }

  # simulate with exposure
  out <- lemna(init=init, times=times, param=param, envir=envir, ...)
  # simulate control w/o exposure
  envir$conc <- 0
  ctrl <- lemna(init=init, times=times, param=param, envir=envir, ...)

  # effect on biomass
  BM_exp <-  tail(out$BM, 1)
  BM_ctrl <- tail(ctrl$BM, 1)
  BM_efx <- ifelse(BM_ctrl == 0, 0, 1 - BM_exp / BM_ctrl)

  # effect on growth rate
  t_length <- max(times) - min(times)
  r_exp <-  log(BM_exp / out$BM[1]) / t_length
  r_ctrl <- log(BM_ctrl / ctrl$BM[1]) / t_length
  r_efx <- ifelse(r_ctrl == 0, 0, min(1, 1 - r_exp / r_ctrl))

  c(
    BM = BM_efx * 100,
    r  = r_efx * 100
  )
}

# Special case to derive effects for a Lemna scenario
#
# The method just pass its information to [effect.default()]
#' @param x a `lemna_scenario` object
#' @describeIn effect Scenario parameters supplied as a `lemna_scenario` object
#' @export
effect.lemna_scenario <- function(x, init, times, param, envir, duration, ...) {
  # Overwrite settings from scenario?
  if("init" %in% names(x) & missing(init)) {
    init <- x$init
  }
  if("times" %in% names(x) & missing(times)) {
    times <- x$times
  }
  if("param" %in% names(x) & missing(param)) {
    param <- x$param
  }
  if("envir" %in% names(x) & missing(envir)) {
    envir <- x$envir
  }

  effect.default(init=init, times=times, param=param, envir=envir, duration=duration, ...)
}
