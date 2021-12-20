# Model parameters for the substance metsulfuron-methyl as reported by Schmitt et al. (2013)
metsulfuron_param <- within(param_defaults(), {
  # constant photosynthesis rates for backwards compatibility
  k_photo_fixed <- TRUE
  # growth model
  BM_threshold <- 0
  BM_L <- 176
  # toxicodynamics
  b <- 4.16
  EC50_int <- 0.3
  E_max <- 0.784
  # toxicokinetics
  P <- 0.0054
  K_pw <- 0.75
  k_met <- 0
})


metsulfuron_envir <- list(
  # simple exposure pattern representing a step-function
  conc = data.frame(time = c(0, 7, 7.01, 14), conc = c(1, 1, 0, 0)),
  tmp = 12,    # constant temperature of 12 °C
  irr = 15000, # constant irradiance of 15,000 kJ m-2 d-1
  P = 0.3,     # constant Phosphorus concentration of 0.3 mg L-1
  N = 0.6      # constant Nitrogen concentration of 0.6 mg L-1
)

metsulfuron <- new_lemna_scenario(
  init = c(BM=0.0012, M_int=0),
  times = 0:14,
  param = metsulfuron_param,
  envir = metsulfuron_envir
)

usethis::use_data(metsulfuron, overwrite = TRUE)

