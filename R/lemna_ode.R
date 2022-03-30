# Temperature response of biomass loss rate (Box 5)
# @param Tmp temperature (°C)
# @param Q10 temperature coefficient (-)
# @param T_ref ref temperature for response=1 (°C)
# @return value from the interval [0,1]
fT_loss <- function(Tmp, Q10, T_ref) {
  return(Q10 ^ ((Tmp - T_ref) / 10))
}

# Temperature response of photosynthesis (Box 4)
# @param Tmp temperature (°C)
# @param T_opt optimum growth temperature (°C)
# @param T_min minimum growth temperature (°C)
# @param T_max maximum growth temperature (°C)
# @return value from the interval [0,1]
fT_photo <- function(Tmp, T_opt, T_min, T_max) {
  T_m <- ifelse(Tmp <= T_opt, T_min, T_max)
  return(10 ^ (-(Tmp - T_opt) ^ 2 / (T_m - T_opt) ^ 2))
}

# Irradiance response of photosynthesis (Box 6)
# @param Irr irradiance (kJ m-2 d-1)
# @param alpha slope of irradiance response of photosynthesis (m2 d kJ-1)
# @param beta intercept of irradiance response of photosynthesis (-)
# @return value from the interval [0,1]
fI_photo <- function(Irr, alpha, beta) {
  return(min(1, alpha * Irr + beta))
}

# Nitrogen response of photosynthesis (Box 7)
# @param Ntr nitrogen concentration (mg N L-1)
# @param N_50 half-saturation constant of Nitrogen response (mg N L-1)
# @return value from the interval [0,1]
fN_photo <- function(Ntr, N_50) {
  return(Ntr / (Ntr + N_50))
}

# Phosphorus response of photosynthesis (Box 7)
# @param Phs phosphorus concentration (mg P L-1)
# @param P_50 half-saturation constant of Phosphorus response (mg P L-1)
# @return value from the interval [0,1]
fP_photo <- function(Phs, P_50) {
  return(Phs / (Phs + P_50))
}

# Density dependence of photosynthesis (Box 8)
# @param BM biomass (g dw m-2)
# @param BM_L carrying capacity (g dw m-2)
# @return value from the interval [0,1]
fBM_photo <- function(BM, BM_L) {
  return(1 - BM / BM_L)
}

# Concentration response of photosynthesis [Toxicodynamics] (Box 9)
# @param C_int internal toxicant concentration (mass per volume, e.g. ug L-1)
# @param E_max maximum inhibition (-)
# @param EC50_int int. conc. resulting in 50% effect (mass per volume, e.g. ug L-1)
# @param b slope parameter (-)
# @return value from the interval [0,1]
fCint_photo <- function(C_int, E_max, EC50_int, b) {
  return(1 - E_max * C_int ^ b / (EC50_int ^ b + C_int ^ b))
}

# ODE function
# @param t numeric, time
# @param state named vector of state variables
# @param param named vector of parameters
lemna_ode <- function(t, state, param) {
  with(as.list(c(state,param)), {
    ##
    ## Environmental variables
    ##
    C_ext <- `ts_conc`(t)
    Tmp <- `ts_tmp`(t)
    Irr <- `ts_irr`(t)
    Phs <- `ts_P`(t)
    Ntr <- `ts_N`(t)

    # Respiration dependency function (Box 3)
    if(k_photo_fixed) { # unlimited growth conditions
      f_loss <- 1
    } else {
      f_loss <-  fT_loss(Tmp, Q10, T_ref)
    }

    ##
    ## Toxicokinetics
    ##

    # Internal toxicant concentration (ug L-1) (Box 10)
    if(BM <= 0) { # avoid division by zero
      C_int <- 0
      C_int_unb <- 0
    } else {
      C_int <- M_int * r_FW_V / (BM * r_FW_DW)
      C_int_unb <- C_int / K_pw # unbound internal concentration
    }

    # TK model ODE (Box 10)
    dM_int <- P * BM * r_A_DW * (C_ext - C_int_unb) -
              M_int / K_pw * k_met - M_int * k_loss * f_loss

    ##
    ## Effects on photosynthesis
    ##

    # Photosynthesis dependency function including Liebig's Law (Box 2)
    if(k_photo_fixed) { # unlimited growth conditions, except exposure effects
      f_photo <- fCint_photo(C_int_unb, E_max, EC50_int, b)
    } else {
      f_photo <- min(fT_photo(Tmp, T_opt, T_min, T_max),
                     fI_photo(Irr, alpha, beta),
                     fP_photo(Phs, P_50),
                     fN_photo(Ntr, N_50)) * fBM_photo(BM, BM_L) * fCint_photo(C_int_unb, E_max, EC50_int, b)
    }

    ##
    ## Population growth
    ##

    # Growth model ODE (Box 1)
    dBM <- (k_photo_max * f_photo - k_loss * f_loss) * BM
    # avoid biomass decrease below BM_min
    if(BM <= BM_min & dBM < 0) {
      dBM <- 0
    }

    # Return derivatives
    list(c(dBM, dM_int))
  })
}
