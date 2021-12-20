#' Simulate a Lemna scenario
#'
#' The function numerically integrates the Lemna model using the supplied
#' parameters, environmental factor time series, output time points, and other
#' settings. Numerical integration is handled by [deSolve::lsoda()] by default
#' which is well suited to handle stiff and non-stiff numerical problems.
#'
#' ## Initial state
#' The model has two state variables:
#' - Biomass, `BM` (g dw m-2)
#' - Mass of toxicant in plant population, `M_int` (mass per m2, e.g. ug m-2)
#'
#' ## Output times
#' The function will only return results for the requested time points. The solver
#' may make additional time steps which are not returned, depending on the
#' solver settings such as numerical tolerance. The number of output times and
#' the distance between them can have influence on the numerical precision of
#' results. Please refer to the manual of the [deSolve] package for more
#' information on that topic.
#'
#' ## Parameters
#' The list of available model parameters, their units, and suggested default
#' values is documented in [param_defaults()].
#'
#' ## Environmental variables
#' The model requires a time series for each of the five environmental variables
#' For ease of use, a time series can be represented by either a constant
#' numeric value, a `data.frame` containing a time series, a character
#' string with a path to a CSV file containing a time series, or a function
#' returning a value for a certain time point.
#'
#' The five environmental variables are as follows:
#' - Exposure concentration, `conc` (mass per volume, e.g ug L-1)
#' - Temperature, `tmp` (°C)
#' - Irradiance, `irr` (kJ m-2 d-1)
#' - Phosphorus concentration, `P` (mg P L-1)
#' - Nitrogen concentration, `N` (mg N L-1)
#'
#' A `data.frame` containing a time series must consist of exactly two columns:
#' The first column contains the time in days, the second column the environmental
#' factor in question. The `data.frame` must contain at least one row. Time
#' points are not required to coincide with the solver's output times but
#' the user should take care that the solver's time step length is sufficiently
#' small so that it can consider the relevant dynamics of the time series.
#' This problem can be avoided by calling the model ODEs implemented in C, see
#' argument `ode_mode`.
#'
#' If a string is passed as an environmental factor, the function will interpret
#' the string as a file path and will try to load the contents of a CSV file.
#' The same requirements as for a `data.frame` apply to a time series loaded
#' from a file.
#'
#' When using the pure R code ODE, it is also possible to pass a function as
#' an environmental factor. The function must accept exactly one argument which
#' represents time and must return a single scalar value. Using a function
#' in combination with the compiled code solver will raise an error.
#'
#' ## R vs. compiled code
#' The model can be simulated using pure *R* code (the default) or an implementation
#' that uses the compiled code feature of [deSolve]. Compiled code is almost
#' always significantly faster than pure *R*. The solver for compiled ODEs also
#' handles environmental variables better than the pure *R* version. For optimal
#' performance, the time series of environmental variables should always be as
#' short as possible and as long as needed.
#'
#' To use the compiled code feature, set the argument `ode_mode = "c"`.
#'
#' ## Additional outputs
#' For reasons of convenience, the return value contains by default two additional
#' variables derived from simulation results: the internal concentration `C_int`
#' as well as the number of fronds `FrondNo`. These can be disabled by setting
#' the argument `nout = 0`.
#'
#' The compiled code model can return up to 18 additional variables which
#' represent intermediary variables, environmental variables, and derivatives
#' calculated within the model equations. Please refer to the model description
#' of *Klein et al.* for more information on these variables and how they
#' influence model behavior.
#'
#'
#' @param init initial state of the model variables
#' @param times numeric vector, output times for which model results are returned
#' @param param named list, Lemna model parameters
#' @param envir named list, contains time series data for each of the five
#'   environmental variables
#' @param ode_mode character, switch controls if ODE functions implemented in R or
#'   C are used, defaults to R
#' @param nout numeric, controls the number of additional output variables of the
#'   model, such as `C_int` (internal concentration) or `FrondNo` (the number
#'   of fronds), see e.g. [deSolve::lsoda()] for details. Defaults to `2`
#' @param ... additional parameters passed on to [deSolve::ode()]
#' @return `data.frame` with simulation results
#' @importFrom stats approxfun
#' @importFrom utils read.csv
#' @export
#' @examples
#' # Simulate the metsulfuron example scenario
#' lemna(metsulfuron)
#'
#' # Create a simple plot of the number of fronds
#' lemna(metsulfuron) -> result
#' plot(result$time, result$FrondNo)
#'
#' # Create a nicer plot using a dedicated plotting routine
#' plot(result)
#'
#' # Simulate the example scenario for a period of 30 days
#' lemna(metsulfuron, times=0:30) -> result30
#' plot(result30)
#'
#' ##
#' ## Create a custom Lemna scenario from scratch
#' ##
#'
#' # Initial state: 12 fronds, no toxicant mass
#' myinit <- c(BM=0.0012, M_int=0)
#'
#' # Output times: simulate 7 days with a ~2 hour time step
#' mytime <- seq(0, 7, 0.1)
#'
#' # Default model parameters + TKTD parameters of a hypothetical substance
#' myparam <- param_defaults(list(
#'   EC50_int = 0.1, # 50% effect level (ug L-1)
#'   b = 0.7,        # slope parameter (-)
#'   P = 0.01        # permeability (cm d-1)
#' ))
#'
#' # Custom environmental variables
#' myenvir <- list(
#'   # exposure step function:
#'   # 3 days no exposure, followed by 4 days of 10 ug L-1
#'   conc = data.frame(t=c(0,3,4,7), conc=c(0,0,10,10)),
#'   tmp = 18,    # constant temperature of 18 °C
#'   irr = 15000, # constant irradiance of 15,000 kJ m-2 d-1
#'   N = 0.6,     # constant Nitrogen concentration of 0.6 mg L-1
#'   P = 0.3      # constant Phosphorus concentration of 0.3 mg L-1
#' )
#'
#' # Simulate the custom scenario and plot results
#' lemna(init=myinit, times=mytime, param=myparam, envir=myenvir) -> result_custom
#' plot(result_custom)
#'
#' # Simulate again, forcing the solver to use smaller time steps of hmax=0.001.
#' # The resulting curves are almost identical for this example.
#' lemna(init=myinit, times=mytime, param=myparam, envir=myenvir, hmax=0.001) -> result_custom2
#' library(ggplot2)
#' ggplot(result_custom, aes(time, FrondNo)) +
#'   geom_line() +
#'   geom_line(data=result_custom2, color="red", style="dashed")
#'
#' # Combine all settings into a scenario object and simulate it
#' scenario <- new_lemna_scenario(
#'  init = myinit,
#'  param = myparam,
#'  times = mytime,
#'  envir = myenvir
#' )
#' lemna(scenario)
lemna <- function(...) {
  UseMethod("lemna")
}

# Default method for simulation, all available info is given in the generic
#' @describeIn lemna All scenario parameters supplied as arguments
#' @export
lemna.default <- function(init=c("BM"=0, "M_int"=0), times, param, envir, ode_mode=c("r", "c"), nout=2, ...) {
  ode_mode <- match.arg(ode_mode)

  # check initial state vector
  init_missing <- setdiff(c("BM", "M_int"), names(init))
  if(length(init_missing) > 0) {
    stop(paste("init vector elements missing:", paste(init_missing, collapse=",")))
  }
  if(length(init) != 2) {
    stop("init vector has invalid length")
  }

  # check output times argument
  if(length(times) < 2) {
    stop("times vector must have at least two elements")
  } else if(length(times) == 2) {
    # magic value: 0.01 d, default step length in time during simulation
    times <- seq(min(times) ,max(times), 0.01)
  }

  # completeness check of supplied parameters
  param <- as.list(param)
  param_missing <- setdiff(names(param_defaults()), names(param))
  if(length(param_missing) > 0) {
    stop(paste("model parameters missing:", paste(param_missing, collapse=",")))
  }

  # prepare time series of environmental variables
  envir_missing <- setdiff(c("conc","tmp","irr","P","N"), names(envir))
  if(length(envir_missing) > 0) {
    stop(paste("environmental factor(s) missing:",paste(envir_missing,collapse=",")))
  }
  envir$conc <- prepare_envir("conc", envir) # exposure concentration
  envir$tmp <- prepare_envir("tmp", envir) # temperature
  envir$irr <- prepare_envir("irr", envir) # irradiance
  envir$P <- prepare_envir("P", envir) # phosphorus concentration
  envir$N <- prepare_envir("N", envir) # nitrogen concentration

  # simulate ODE with R oder C code?
  if(ode_mode == "r") {
    # convert environmental variables to interpolated functions and pass them
    # as additional parameters to ODE
    for(nm in names(envir)) {
      v <- envir[[nm]]
      if(is.function(v)) { # a function
        param[[paste0("ts_",nm)]] <- v
      } else if(nrow(v)==1) { # constant time series
        param[[paste0("ts_",nm)]] <- local({ c <- v[[1,2]]; function(t) c })
      } else { # linear interpolation of time series
        param[[paste0("ts_",nm)]] <- approxfun(x=v[, 1], y=v[, 2], method="linear", f=0, rule=2, ties="ordered")
      }
    }
    rm(nm, v) # clean up namespace

    # call ODE solver
    out <- as.data.frame(deSolve::ode(y=init, times=times, func=lemna_ode, parms=param, ...))

    # additional output variables requested?
    # -> emulate (parts of) the behavior of the compiled ODE solver
    if(nout > 0) { # internal toxicant concentration (ug L-1) (Box 10)
      out$C_int <- ifelse(out$BM <= 0, 0, out$M_int * param$r_FW_V / (out$BM * param$r_FW_DW))
    }
    if(nout > 1) { # number of fronds
      out$FrondNo <- out$BM / param$r_DW_FN
    }
    if(nout > 2) {
      warning("additional outputs (nout > 2) only available for compiled ODE")
    }
  }
  else if(ode_mode == "c") {
    # derive mandatory ordering of parameters from empty parameter set
    param_order <- names(param_new())
    # reorder parameters
    param <- unlist(param[param_order])
    # reorder environmental variables
    envir <- envir[c("conc","tmp","irr","P","N")]
    # set names of additional output variables
    outnames <- c("C_int", "FrondNo", "f_loss", "f_photo", "fT_photo", "fI_photo",
                  "fP_photo", "fN_photo", "fBM_photo", "fCint_photo", "C_int_unb",
                  "C_ext", "Tmp", "Irr", "Phs", "Ntr", "dBM", "dM_int")
    # interpolation settings
    fcontrol <- list(method="linear", rule=2, f=0, ties="ordered")
    # call solver
    out <- deSolve::ode(y=init, times=times, parms=param, forcings=envir,
                        dllname="lemna", initfunc="lemna_init", func="lemna_func",
                        initforc="lemna_forc", fcontrol=fcontrol, nout=nout,
                        outnames=outnames, ...)
    out <- as.data.frame(out)
  }
  else {
    stop("unknown ode mode")
  }

  class(out) <- c("lemna_result", class(out))
  # additional data required for plots:
  attr(out, "r_DW_FN") <- param[["r_DW_FN"]]
  attr(out, "exposure") <- envir$conc
  out
}

# Special case to simulate a Lemna scenario
#
# The method just pass its information to [lemna.default()]
#' @param x a `lemna_scenario` object
#' @describeIn lemna Scenario parameters supplied as a `lemna_scenario` object
#' @export
lemna.lemna_scenario <- function(x, init, times, param, envir, ...) {
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

  lemna.default(init=init, times=times, param=param, envir=envir, ...)
}

# Prepare environmental factor time series
#
# Time series of environmental variables, such as exposure concentration, temperature,
# or irradiance, can be supplied by three individual means:
# - a single numeric value representing constant conditions
# - a `data.frame` with two numeric columns, the former representing time and
#   the latter the environmental factor that changes over time
# - a string representing the path to a .csv file. the file is read by [read.csv()]
#   and then treated as if the data.frame was provided as an argument to this
#   function
#
# @param key character, unique key of an element in `envir` list
# @param envir named list, contains environmental factor data, e.g. a data.frame
#    or a constant value
# @return data.frame with two columns
prepare_envir <- function(key, envir) {
  # check if key exists in environmental variables
  if(!(key %in% names(envir))) {
    stop(paste("environmental factor missing:", key))
  }

  envir <- as.list(envir)
  data <- envir[[key]]
  # if it's a function, such as an interpolated time series, return element as is
  if(is.function(data)) {
    return(data)
  }
  # if element is a string, try to read the .csv file
  if(is.character(data)) {
    data <- read.csv(data, stringsAsFactors=FALSE)
  }
  # if element is a numeric, use as constant time series
  if(is.numeric(data)) {
    if(length(data) > 1) {
      stop(paste("environmental factor", key, "has length > 1"))
    }
    data <- data.frame(t=0, V1=data)
  }
  # if element is a data.frame, use as time series
  if(is.data.frame(data)) {
    if(length(data) != 2) {
      stop(paste("environmental factor",key,"time series must have exactly two columns"))
    }
  } else {
    stop(paste("unknown data type for environmental factor", key))
  }
  data
}
