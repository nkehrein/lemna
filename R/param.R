#' Default parameters
#'
#' Returns the default Lemna model parameters as reported by Klein et al. (2021).
#'
#' ## Model parameters
#'
#' ### Growth model
#' - Model switch for unlimited growth conditions, `k_photo_fixed` (TRUE/FALSE)
#' - Maximum photosynthesis rate, `k_photo_max` (d-1)
#' - Reference loss rate, `k_loss` (d-1)
#' - Lower biomass abundance threshold, `BM_threshold` (g dw m-2)
#' - Reservoir for biomass recovery, `BM_min` (g dw m-2)
#'
#' ### Temperature response of photosynthesis
#' - Optimum growth temperature, `T_opt` (°C)
#' - Minimum growth temperature, `T_min` (°C)
#' - Maximum growth temperature, `T_max` (°C)
#'
#' ### Temperature response of biomass loss rate
#' - Temperature coefficient, `Q10` (-)
#' - Reference temperature for response=1, `T_ref` (°C)
#'
#' ### Irradiance reponse of photosynthesis
#' - Slope of irradiance response, `alpha` (m2 d kJ-1)
#' - Intercept of irradiance response, `beta` (-)
#'
#' ### Nutrient response of photosynthesis
#' - Half-saturation constant of Nitrogen, `N_50` (mg N L-1)
#' - Half-saturation constant of Phosphorus, `P_50` (mg P L-1)
#'
#' ### Density dependence of photosynthesis
#' -  Carrying capacity, `BM_L` (g dw m-2)
#'
#' ### Concentration response (Toxicodynamics)
#' - Internal concentration resulting in 50% effect, `EC50_int` (ug L-1)
#' - Maximum inhibition c, `E_max` (-)
#' - Slope parameter, `b` (-)
#'
#' ### Internal concentration (Toxicokinetics)
#' - Permeability, `P` (cm d-1)
#' - Area per dry-weight ratio, `r_A_DW` (cm2 g-1)
#' - Fresh weight per dry weight ratio, `r_FW_DW` (-)
#' - Fresh weight density, `r_FW_V` (g cm-3)
#' - Dry weight per frond ratio, `r_DW_FN` (g dw)
#' - Partitioning coefficient plant:water, `K_pw` (-)
#' - Metabolisation rate, `k_met` (d-1)
#'
#' @param values optional named numeric `vector`, values will override any
#'   defaults
#' @return named `list`
#' @export
#' @examples
#' # Returns default model parameters, some parameters are not defined (NA)
#' param_defaults()
#'
#' # Overwrite one of the default parameters
#' param_defaults(list(k_photo_max = 0.42))
#'
#' # Provide values for substance specific TKTD parameters
#' param_defaults(list(
#'   EC50_int = 23, # 50% effect level (mass per volume)
#'   b = 1,         # slope parameter (-)
#'   P = 0.42       # permeability (cm d-1)
#' ))
#'
#' # Returns a list of required model parameters with all values set to NA
#' param_new()
param_defaults <- function(values) {
  l <- list(
    # growth model parameters
    k_photo_fixed = FALSE, # model switch, if TRUE then f_loss = 1 and f_photo = fCint_photo()
    k_photo_max = 0.47,    # max photosynthesis rate (d-1)
    k_loss = 0.05,         # reference loss rate (d-1)
    BM_threshold = 5e-4,   # lower biomass abundance threshold (g dw m-2)
    BM_min = 0,            # reservoir for biomass recovery (g dw m-2)

    # response parameters
    T_opt = 26.7,   # optimum growth temperature (°C)
    T_min = 8,      # minimum growth temperature (°C)
    T_max = 40.5,   # maximum growth temperature (°C)
    Q10 = 2,        # temperature coefficient (-)
    T_ref = 25,     # ref temperature for response=1 (°C)
    alpha = 5e-5,   # slope of irradiance response of photosynthesis (m2 d kJ-1)
    beta = 0.025,   # intercept of irradiance response of photosynthesis (-)
    N_50 = 0.034,   # half-saturation constant of Nitrogen response (mg N L-1)
    P_50 = 0.0043,  # half-saturation constant of Phosphorus response (mg P L-1)
    BM_L = 177,     # carrying capacity (g dw m-2)

    # toxicodynamic parameters
    E_max = 1,      # maximum inhibition (-), substance specific
    EC50_int = NA,  # int. conc. resulting in 50% effect (ug L-1), substance specific
    b = NA,         # slope parameter (-), substance specific

    # toxicokinetic parameters
    P = NA,         # permeability (cm d-1), substance specific
    r_A_DW = 1000,  # area per dry-weight ratio (cm2 g-1)
    r_FW_DW = 16.7, # fresh weight per dry weight ratio (-)
    r_FW_V = 1,     # fresh weight density (g cm-3)
    r_DW_FN = 1e-4, # dry weight per frond ratio (g dw)
    K_pw = 1,       # partitioning coefficient plant:water (-), generally substance specific
    k_met = 0       # metabolisation rate (d-1), generally substance specific
  )

  # overwrite any defaults with supplied values?
  if(!missing(values)) {
    for(nm in names(values)) {
      if(!nm %in% names(l)) {
        warning(paste("parameter",nm,"is not part of the Lemna model"))
      }
      l[[nm]] <- values[[nm]]
    }
  }

  l
}


#' @export
#' @describeIn param_defaults A parameter set without default values
param_new <- function(values) {
  l <- list(
    # growth model parameters
    k_photo_fixed = NA,
    k_photo_max = NA,
    k_loss = NA,
    BM_threshold = NA,
    BM_min = NA,

    # response parameters
    T_opt = NA,
    T_min = NA,
    T_max = NA,
    Q10 = NA,
    T_ref = NA,
    alpha = NA,
    beta = NA,
    N_50 = NA,
    P_50 = NA,
    BM_L = NA,

    # toxicodynamic parameters
    E_max = NA,
    EC50_int = NA,
    b = NA,

    # toxicokinetic parameters
    P = NA,
    r_A_DW = NA,
    r_FW_DW = NA,
    r_FW_V = NA,
    r_DW_FN = NA,
    K_pw = NA,
    k_met = NA
  )

  # overwrite any defaults with supplied values?
  if(!missing(values)) {
    for(nm in names(values)) {
      if(!nm %in% names(l)) {
        warning(paste("parameter",nm,"is not part of the Lemna model"))
      }
      l[[nm]] <- values[[nm]]
    }
  }

  l
}
