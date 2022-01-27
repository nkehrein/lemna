#' A Lemna scenario fitted to metsulfuron-methyl effect data
#'
#' The dataset consists of a named `list` which contains vectors describing
#' initial state, parameters, output times, and environmental variables of the
#' *Lemna* model.
#'
#' The scenario will simulate a period of 14 days with daily outputs, a start
#' population of 12 fronds, unlimited growth conditions, and an exposure pattern
#' represented by a step-function.
#'
#' The scenario setup was published by Schmitt *et al.* (2013).
#' A mechanistic combined toxicokinetic-toxicodynamic (TK/TD) and growth
#' model for the aquatic macrophytes *Lemna spp.* was parameterized by the authors
#' based on literature data. TK/TD parameters were determined by calibrating the
#' model using substance specific effect data of metsulfuron-methyl.
#'
#' @references
#' Schmitt W., Bruns E., Dollinger M., Sowig P., 2013: Mechanistic TK/TD-model
#'   simulating the effect of growth inhibitors on *Lemna* populations. Ecol Model
#'   255, pp. 1-10. \doi{10.1016/j.ecolmodel.2013.01.017}
#'
#' @examples
#' # Simulate the example scenario
#' lemna(metsulfuron)
#'
#' # Simulate a longer time period of 21 days
#' lemna(metsulfuron, times=0:21)
#'
#' # Print the scenario's exposure series
#' metsulfuron$envir$conc
#'
#' # Print all environmental variables
#' metsulfuron$envir
"metsulfuron"

#' Observed frond numbers reported by Schmitt *et al.* (2013)
#'
#' The data consists of observed frond numbers from experimental studies for
#' various time points and exposure concentrations. The *Lemna* population
#' was exposed to constant concentrations of *metsulfuron-methyl* for seven days,
#' followed by seven days of recovery.
#'
#' The dataset was presented in Schmitt *et al.* (2013),
#' cf. Figure 2, and was included in this package by courtesy of the authors.
#'
#' @references
#' Schmitt W., Bruns E., Dollinger M., Sowig P., 2013: Mechanistic TK/TD-model
#'   simulating the effect of growth inhibitors on *Lemna* populations. Ecol Model
#'   255, pp. 1-10. \doi{10.1016/j.ecolmodel.2013.01.017}
"schmitt77"

#' Observed frond numbers reported by Hommen *et al.* (2015)
#'
#' The data consists of observed frond numbers from experimental studies for
#' various time points and exposure concentrations. The *Lemna* population
#' was exposed to constant concentrations of *metsulfuron-methyl* for two days,
#' followed by twelve days of recovery.
#'
#' The dataset was presented in Hommen *et al.* (2015)],
#' cf. Figure 3, and was included in this package by courtesy of the authors.
#'
#' @references
#' Hommen U., Schmitt W., Heine S., Brock Theo C.M., Duquesne S., Manson P., Meregalli G.,
#'   Ochoa-Acuña H., van Vliet P., Arts G., 2015: How TK-TD and Population Models for
#'   Aquatic Macrophytes Could Support the Risk Assessment for Plant Protection
#'   Products. Integr Environ Assess Manag 12(1), pp. 82-95. \doi{10.1002/ieam.1715}

"hommen212"

#' A Lemna scenario using FOCUS D1 Ditch environmental conditions
#'
#' The dataset consists of a named `list` which contains vectors describing
#' initial state, parameters, output times, and environmental variables of the
#' Lemna model. The scenario represents conditions of the FOCUS D1 Ditch
#'  exposure scenario.
#'
#' The scenario will simulate a period of 365 days with hourly outputs, a start
#' population of 80 g/m² dry weight, variable environmental conditions, and a
#' complex, time-varying exposure pattern.
#'
#' The scenario setup was published by Hommen *et al*. (2015). Exposure pattern
#' and substance specific parameters are of exemplary character
#' and represent the herbicide *metsulfuron-methyl*.
#'
#' @references
#' Hommen U., Schmitt W., Heine S., Brock Theo C.M., Duquesne S., Manson P., Meregalli G.,
#'   Ochoa-Acuña H., van Vliet P., Arts G., 2015: How TK-TD and Population Models for
#'   Aquatic Macrophytes Could Support the Risk Assessment for Plant Protection
#'   Products. Integr Environ Assess Manag 12(1), pp. 82-95. \doi{10.1002/ieam.1715}
#'
#' @examples
#' # Simulate the example scenario
#' lemna(focusd1)
"focusd1"

#' A Lemna scenario using FOCUS D2 Ditch environmental conditions
#'
#' The dataset consists of a named `list` which contains vectors describing
#' initial state, parameters, output times, and environmental variables of the
#' Lemna model. The scenario represents conditions of the FOCUS D2 Ditch
#' exposure scenario.
#'
#' The scenario will simulate a period of 365 days with hourly outputs, a start
#' population of 100 g/m² dry weight, variable environmental conditions, and a
#' complex, time-varying exposure pattern.
#'
#' The scenario setup was published by Hommen *et al*. (2015). Exposure pattern
#' and substance specific parameters are of exemplary character
#' and represent the herbicide *metsulfuron-methyl*.
#'
#' @references
#' Hommen U., Schmitt W., Heine S., Brock Theo C.M., Duquesne S., Manson P., Meregalli G.,
#'   Ochoa-Acuña H., van Vliet P., Arts G., 2015: How TK-TD and Population Models for
#'   Aquatic Macrophytes Could Support the Risk Assessment for Plant Protection
#'   Products. Integr Environ Assess Manag 12(1), pp. 82-95. \doi{10.1002/ieam.1715}
#'
#' @examples
#' # Simulate the example scenario
#' lemna(focusd2)
"focusd2"

#' A Lemna scenario using FOCUS R3 Stream environmental conditions
#'
#' The dataset consists of a named `list` which contains vectors describing
#' initial state, parameters, output times, and environmental variables of the
#' Lemna model. The scenario represents conditions of the FOCUS R3 Stream
#' exposure scenario.
#'
#' The scenario will simulate a period of 365 days with hourly outputs, a start
#' population of 100 g/m² dry weight, variable environmental conditions, and a
#' complex, time-varying exposure pattern.
#'
#' The scenario setup was published by Hommen *et al*. (2015). Exposure pattern
#' and substance specific parameters are of exemplary character
#' and represent the herbicide *metsulfuron-methyl*.
#'
#' @references
#' Hommen U., Schmitt W., Heine S., Brock Theo C.M., Duquesne S., Manson P., Meregalli G.,
#'   Ochoa-Acuña H., van Vliet P., Arts G., 2015: How TK-TD and Population Models for
#'   Aquatic Macrophytes Could Support the Risk Assessment for Plant Protection
#'   Products. Integr Environ Assess Manag 12(1), pp. 82-95. \doi{10.1002/ieam.1715}
#'
#' @examples
#' # Simulate the example scenario
#' lemna(focusr3)
"focusr3"
