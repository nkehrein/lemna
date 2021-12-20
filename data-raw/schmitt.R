
# Observed frond numbers reported by Schmitt et al. (2013) for various
# exposure concentrations, exposure for 7 days, 7 days recovery
df <- read.csv("data-raw/schmitt_7d_exp_7d_rec.csv", stringsAsFactors = FALSE)
names(df) <- c("time", 0, 0.32, 0.56, 1, 1.8, 3.2, 5.6)
df %>%
  tidyr::pivot_longer(cols=2:8, names_to="level", values_to="FrondNo") -> schmitt77

usethis::use_data(schmitt77, overwrite = TRUE)
rm(df, schmitt77)


# Observed frond numbers reported by Hommen et al. (2015) for various
# exposure concentrations, exposure for 2 days, 12 days recovery
df <- read.csv("data-raw/hommen_2d_exp_12d_rec.csv", stringsAsFactors = FALSE)
names(df) <- c("time", 0, 0.35, 0.875, 1.75, 3.5)
df %>%
  tidyr::pivot_longer(cols=2:6, names_to="level", values_to="FrondNo") -> hommen212

usethis::use_data(hommen212, overwrite = TRUE)
rm(df, hommen212)
