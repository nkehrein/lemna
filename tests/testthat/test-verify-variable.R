#
# Helper functions and setup
#
future::plan(future::multisession())

# load Lemna model by Schmitt et al. (2013)
source("mmc2.r", local=TRUE)
source("mmc3.r", local=TRUE)

simulate_focus <- function(scale_f, model, scenario, ...) {
  if(model == "schmitt") {
    param$k_phot_fix <- FALSE
    param$mass_per_frond <- 0.0004
    param$Conc <- scenario$envir$conc
    param$Conc$Conc <- param$Conc$Conc * scale_f
    param$Temp <- scenario$envir$tmp
    param$Rad <- scenario$envir$irr
    res <- calcgrowth(scenario$times, c(BM=scenario$init[["BM"]], E=1, M_int=0), param, hmax=0.01)
  } else if(model == "klein") {
    envir <- scenario$envir
    envir$conc$Conc <- envir$conc$Conc * scale_f
    res <- lemna(scenario, envir=envir, hmax=0.01, ...)
  }
  res$factor <- as.character(scale_f)
  res
}


#
# Tests for conditions with environmental variability
#
test_that("FOCUS D1 Ditch", {
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()
  skip_if_not_installed("furrr")

  factors <- c(0, round(10^seq(0,2,0.2), 1))
  df_s <- furrr::future_map_dfr(factors, simulate_focus, model="schmitt", scenario=focusd1)
  df_kr <- furrr::future_map_dfr(factors, simulate_focus, model="klein", scenario=focusd1, ode_mode="r")
  df_kc <- furrr::future_map_dfr(factors, simulate_focus, model="klein", scenario=focusd1, ode_mode="c")

  # large tolerance because models equations differ
  expect_equal(df_kr, df_s,  tolerance=0.1, ignore_attr=TRUE)
  # C & R model cores should yield identical results
  expect_equal(df_kr, df_kc, tolerance=1e-4, ignore_attr=TRUE)
})

test_that("FOCUS D2 Ditch", {
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()
  skip_if_not_installed("furrr")

  factors <- c(0, round(10^seq(0,2,0.2), 1))
  df_kr <- furrr::future_map_dfr(factors, simulate_focus, model="klein", scenario=focusd2, ode_mode="r")
  df_kc <- furrr::future_map_dfr(factors, simulate_focus, model="klein", scenario=focusd2, ode_mode="c")

  expect_equal(df_kr, df_kc, tolerance=1e-4, ignore_attr=TRUE)
})

test_that("FOCUS R3 Stream", {
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()
  skip_if_not_installed("furrr")

  factors <- c(0, round(10^seq(0,3,0.3), 1))
  df_kr <- furrr::future_map_dfr(factors, simulate_focus, model="klein", scenario=focusr3, ode_mode="r")
  df_kc <- furrr::future_map_dfr(factors, simulate_focus, model="klein", scenario=focusr3, ode_mode="c")

  expect_equal(df_kr, df_kc, tolerance=1e-4, ignore_attr=TRUE)
})
