

# Model parameters for the substance metsulfuron-methyl as reported by
# Schmitt et al. (2013) in combination with the modified dry-weight per frond
# as used by Hommen et al. (2015)
focus_param <- within(param_defaults(), {
  # variable environmental conditions
  k_photo_fixed <- FALSE
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
  r_DW_FN <- 0.0004
})



#
# Create a lemna_scenario using time series of the FOCUS D1 Ditch scenario
# as reported by Hommen et al. (2015)
#
d1_envir <- list(
  # simple exposure pattern representing a step-function
  conc = read.delim("data-raw/Conc_D1.txt") %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = (dplyr::row_number() - 1) / 24),
  tmp = read.delim("data-raw/D1_Temp_1.1.76-30.4.83.txt", header=FALSE, col.names=c("Time", "Temp")) %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = dplyr::row_number() - 1),
  irr = read.delim("data-raw/D1_Rad_1.1.76-30.4.83.txt", header=FALSE, col.names=c("Time", "Rad")) %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = dplyr::row_number() - 1),
  P = 0.3, # constant Phosphorus concentration of 0.3 mg L-1
  N = 0.6  # constant Nitrogen concentration of 0.6 mg L-1
)

focusd1 <- new_lemna_scenario(
  init = c(BM=80, M_int=0),
  times = 0:365,
  param = focus_param,
  envir = d1_envir
)

usethis::use_data(focusd1, overwrite = TRUE)

#
# Create a lemna_scenario using time series of the FOCUS D2 Ditch scenario
# as reported by Hommen et al. (2015)
#
d2_envir <- list(
  conc = read.delim("data-raw/Conc_D2.txt") %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = (dplyr::row_number() - 1) / 24),
  tmp = read.delim("data-raw/D2_Temp_1.1.80-30.4.87.txt", header=FALSE, col.names=c("Time", "Temp")) %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = dplyr::row_number() - 1),
  irr = read.delim("data-raw/D2_Rad_1.1.80-30.4.87.txt", header=FALSE, col.names=c("Time", "Rad")) %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = dplyr::row_number() - 1),
  P = 0.3, # constant Phosphorus concentration of 0.3 mg L-1
  N = 0.6  # constant Nitrogen concentration of 0.6 mg L-1
)

focusd2 <- new_lemna_scenario(
  init = c(BM=100, M_int=0),
  times = 0:365,
  param = focus_param,
  envir = d2_envir
)

usethis::use_data(focusd2, overwrite = TRUE)

#
# Create a lemna_scenario using time series of the FOCUS R3 Stream scenario
# as reported by Hommen et al. (2015)
#
r3_envir <- list(
  conc = read.delim("data-raw/Conc_R3.txt") %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = (dplyr::row_number() - 1) / 24),
  tmp = read.delim("data-raw/R3_Temp_1.1.75-30.4.82.txt", header=FALSE, col.names=c("Time", "Temp")) %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = dplyr::row_number() - 1),
  irr = read.delim("data-raw/R3_Rad_1.1.75-30.4.82.txt", header=FALSE, col.names=c("Time", "Rad")) %>%
    dplyr::filter(Time >= 2192 & Time <= 2557) %>%
    dplyr::mutate(Time = dplyr::row_number() - 1),
  P = 0.3, # constant Phosphorus concentration of 0.3 mg L-1
  N = 0.6  # constant Nitrogen concentration of 0.6 mg L-1
)

focusr3 <- new_lemna_scenario(
  init = c(BM=100, M_int=0),
  times = 0:365,
  param = focus_param,
  envir = r3_envir
)

usethis::use_data(focusr3, overwrite = TRUE)
