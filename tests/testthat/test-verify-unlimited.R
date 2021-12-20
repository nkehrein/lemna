#
# Helper functions and setup
#

# load Lemna model by Schmitt et al. (2013)
source("mmc2.r", local=TRUE)
source("mmc3.r", local=TRUE)

# simulate the 7d exposure, 7d recovery experiment
simulate_7d <- function(model=c("schmitt","klein"), ...) {
  model <- match.arg(model)
  time <- seq(0, 14, 0.2) # output time points
  levels <- c(0, 0.32, 0.56, 1, 1.8, 3.2, 5.6) # simulated exposure levels

  # set parameters as needed to replicate Figure 2 from Schmitt et al.
  if(model == "schmitt") {
    param$k_phot_fix <- TRUE # unlimited growth
    param$k_phot_max <- 0.4
    param$k_resp <- 0
  } else if(model == "klein") {
    param <- metsulfuron$param
    param$k_photo_fixed <- TRUE
    param$k_photo_max <- 0.4
    param$k_loss <- 0
    envir <- metsulfuron$envir
  }

  result <- data.frame()
  for(conc in levels) {
    exposure <- data.frame(time=c(0, 7, 7.01, 14), c=c(conc, conc, 0, 0))

    if(model == "schmitt") {
      param$Conc <- exposure
      out <- calcgrowth(time, c(BM=0.0012, E=1, M_int=0), param=param, ...)
    } else if(model == "klein") {
      envir$conc <- exposure
      out <- lemna(times=time, init=c(BM=0.0012, M_int=0), param=param, envir=envir, ...)
    }

    out$level = as.character(conc)
    result <- rbind(result, out)
  }
  result
}

# simulate the 2d exposure, 12d recovery experiment
simulate_2d <- function(model=c("schmitt","klein"), ...) {
  model <- match.arg(model)
  time <- seq(0, 14, 0.2) # output time points
  levels <- c(0, 0.35, 0.875, 1.75, 3.5) # simulated exposure levels

  # set parameters as needed to replicate Figure 3 from Hommen et al.
  if(model == "schmitt") {
    param$k_phot_fix <- TRUE # unlimited growth
    param$k_phot_max <- 0.295
    param$k_resp <- 0.00
    param$mass_per_frond <- 0.0004
  } else if(model == "klein") {
    param <- metsulfuron$param
    param$k_photo_fixed <- TRUE
    param$k_photo_max <- 0.295
    param$k_loss <- 0
    param$r_DW_FN <- 0.0004
    envir <- metsulfuron$envir
  }

  result <- data.frame()
  for(conc in levels) {
    exposure <- data.frame(time=c(0, 2, 2.01, 14), c=c(conc, conc, 0, 0))

    if(model == "schmitt") {
      param$Conc <- exposure
      out <- calcgrowth(time, c(BM=12 * param$mass_per_frond, E=1, M_int=0), param=param, ...)
    } else if(model == "klein") {
      envir$conc <- exposure
      out <- lemna(times=time, init=c(BM=12 * param$r_DW_FN, M_int=0), param=param, envir=envir, ...)
    }

    out$level = as.character(conc)
    result <- rbind(result, out)
  }
  result
}

# Figure 4a from Schmitt et al.: 50d period
# simulate a specific exposure pattern
simulate_pattern <- function(model=c("schmitt","klein"), levels, expo_fun, ...) {
  model <- match.arg(model)
  time <- seq(0, 50, 0.2) # output time points

  # set parameters as needed to replicate Figure 2 from Schmitt et al.
  if(model == "schmitt") {
    param$k_phot_fix <- TRUE # unlimited growth
    param$k_phot_max <- 0.295
    param$EC50 <- 0.39
    param$k_resp <- 0
  } else if(model == "klein") {
    param <- metsulfuron$param
    param$k_photo_fixed <- TRUE
    param$k_photo_max <- 0.295
    param$EC50_int <- 0.39
    param$k_loss <- 0
    envir <- metsulfuron$envir
  }

  result <- data.frame()
  for(conc in levels) {
    exposure <- expo_fun(conc)

    if(model == "schmitt") {
      param$Conc <- exposure
      out <- calcgrowth(time, c(BM=0.0006, E=1, M_int=0), param=param, hmax=0.01)
      out$Area <- out$BM * param$AperBM
    } else if(model == "klein") {
      envir$conc <- exposure
      out <- lemna(times=time, init=c(BM=0.0006, M_int=0), param=param, envir=envir, hmax=0.01, ...)
      out$Area <- out$BM * param$r_A_DW
    }

    out$level = as.character(conc)
    result <- rbind(result, out)
  }
  result
}

# 50d constant exposure
expo_const <- function(conc=1) {
  data.frame(time=c(0, 50), c=c(conc, conc))
}

# 42d with 4 of 7d exposure, 8d recovery
expo_4of7 <- function(conc=1) {
  tim <- c(0,       4, 4.01, 6.99)
  exp <- c(conc, conc,    0,    0)

  tim_all <- c()
  exp_all <- c()
  for(i in seq(0,35, 7)) {
    tim_all <- c(tim_all, tim+i)
    exp_all <- c(exp_all, exp)
  }
  data.frame("time"=c(tim_all, 50), "conc"=c(exp_all, 0))
}

# 42d with 2 of 7d exposure, 8d recovery
expo_2of7 <- function(conc=1) {
  tim <- c(0,       2, 2.01, 6.99)
  exp <- c(conc, conc,    0,    0)

  tim_all <- c()
  exp_all <- c()
  for(i in seq(0,35, 7)) {
    tim_all <- c(tim_all, tim+i)
    exp_all <- c(exp_all, exp)
  }
  data.frame("time"=c(tim_all, 50), "conc"=c(exp_all, 0))
}

#
# Tests for unlimited growth conditions
#
test_that("7d exp, 7 rec", {
  df_s <- simulate_7d(model="schmitt")
  df_kr <- simulate_7d(model="klein", ode_mode="r", hmax=0.01)
  df_kc <- simulate_7d(model="klein", ode_mode="c", hmax=0.01)

  expect_equal(df_kr, df_s,  tolerance=1e-4, ignore_attr=TRUE)
  expect_equal(df_kr, df_kc, tolerance=1e-4, ignore_attr=TRUE)
})

test_that("2d exp, 12, rec", {
  df_s <- simulate_2d(model="schmitt")
  df_kr <- simulate_2d(model="klein", ode_mode="r", hmax=0.01)
  df_kc <- simulate_2d(model="klein", ode_mode="c", hmax=0.01)

  expect_equal(df_kr, df_s,  tolerance=1e-4, ignore_attr=TRUE)
  expect_equal(df_kr, df_kc, tolerance=1e-4, ignore_attr=TRUE)
})

test_that("50d exp", {
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()
  levels <- c(0, 0.1, 0.25, 0.5, 1)

  # simulate constant exposure patterns
  df_s <- simulate_pattern("schmitt", levels, expo_fun=expo_const)
  df_kr <- simulate_pattern("klein", levels, expo_fun=expo_const, ode_mode="r")
  df_kc <- simulate_pattern("klein", levels, expo_fun=expo_const, ode_mode="c")

  expect_equal(df_kr, df_s,  tolerance=1e-4, ignore_attr=TRUE)
  expect_equal(df_kr, df_kc, tolerance=1e-4, ignore_attr=TRUE)
})

test_that("4 of 7d exposure", {
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  levels <- c(0, 0.175, 0.438, 0.875, 1.75)

  # simulate constant exposure patterns
  df_s <- simulate_pattern("schmitt", levels, expo_fun=expo_4of7)
  df_kr <- simulate_pattern("klein", levels, expo_fun=expo_4of7, ode_mode="r")
  df_kc <- simulate_pattern("klein", levels, expo_fun=expo_4of7, ode_mode="c")

  expect_equal(df_kr, df_s,  tolerance=1e-4, ignore_attr=TRUE)
  expect_equal(df_kr, df_kc, tolerance=1e-4, ignore_attr=TRUE)
})

test_that("2 of 7d exposure", {
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  levels <- c(0, 0.35, 0.875, 1.75, 3.5)

  # simulate constant exposure patterns
  df_s <- simulate_pattern("schmitt", levels, expo_fun=expo_2of7)
  df_kr <- simulate_pattern("klein", levels, expo_fun=expo_2of7, ode_mode="r")
  df_kc <- simulate_pattern("klein", levels, expo_fun=expo_2of7, ode_mode="c")

  expect_equal(df_kr, df_s,  tolerance=1e-3, ignore_attr=TRUE)
  expect_equal(df_kr, df_kc, tolerance=1e-4, ignore_attr=TRUE)
})

