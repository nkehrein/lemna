#' Plot a Lemna scenario result
#'
#' Creates up to four plots in a gridded layout depicting
#' 1. Exposure (i.e. external) concentration, as well as damage (internal concentration, `C_int`)
#'    if available
#' 1. Internal toxicant mass (`M_int`)
#' 1. Population size as biomass (`BM`)
#' 1. Population size as number of fronds (`FrondNo`) if available
#'
#' @param x `lemna_result` object
#' @param y unused parameter
#' @param ... unused parameter
#' @param munit character, unit of internal mass, defaults to `ug/m2`
#' @param cunit character, unit of exposure, defaults to `ug/L`
#' @param legend logical, if `TRUE` then a legend is displayed, set to `FALSE`
#'   to hide legend, defaults to `TRUE`
#'
#' @return a gridded plot
#' @importFrom stats approx
#' @export
#' @examples
#' # Simulate a sample scenario and plot results
#' result <- lemna(metsulfuron)
#' plot(result)
#'
#' # Hide the legend of the concentration plot
#' plot(result, legend=FALSE)
#'
#' # Simulate and plot a scenario with changing environmental conditions
#' plot(lemna(focusd1))
plot.lemna_result <- function(x, y, munit="ug/m2", cunit="ug/L", legend=TRUE, ...) {
  has_fronds <- "FrondNo" %in% names(x)
  has_cint <- "C_int" %in% names(x)
  legend.pos <- "none" # disables the legend of the concentration plot
  if(legend) {
    legend.pos <- c(0.8, 0.8)
  }

  plist <- list()
  # BM to Fronds conversion factor
  r_DW_FN <- attr(x, "r_DW_FN")
  # original exposure series
  conc_ext <- attr(x, "exposure")
  if(nrow(conc_ext) == 1) { # duplicate single row or else approx() fails
    conc_ext <- rbind(conc_ext,conc_ext)
  }
  # interpolate external concentration to whole time frame
  xout <- sort(unique(c(x[, 1], conc_ext[, 1])))
  conc <- data.frame(time = xout,
                     conc = approx(x=conc_ext[, 1], y=conc_ext[, 2], xout=xout,
                                   method="linear", f=0, rule=2, ties="ordered")$y,
                     Compartment = c("External")
  )
  # add internal concentration if available
  if(has_cint) {
    cint <- x[, c("time", "C_int")]
    names(cint) <- c("time", "conc")
    cint$Compartment <- "Internal"
    conc <- rbind(conc, cint)
  }

  # plot concentrations
  ggplot2::ggplot(conc, ggplot2::aes_string("time", "conc", color="Compartment")) +
    ggplot2::geom_line() +
    ggplot2::labs(x="Time (days)", y=paste0("Concentration (",cunit,")"), title="Toxicant concentration") +
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position=legend.pos) -> plist$c

  # plot internal mass
  ggplot2::ggplot(x, ggplot2::aes_string("time", "M_int")) +
    ggplot2::geom_line() +
    ggplot2::labs(x="Time (days)", y=paste0("Internal mass (",munit,")"), title="Internal toxicant mass (M_int)") +
    ggplot2::theme_bw() -> plist$m

  # plot biomass
  ggplot2::ggplot(x, ggplot2::aes_string("time", "BM")) +
    ggplot2::geom_line() +
    ggplot2::labs(x="Time (days)", y="Biomass (g dw/m2)", title="Population size (BM)") +
    ggplot2::theme_bw() -> plist$bm

  # plot fronds axis
  if(has_fronds) {
    plist$bm <- plist$bm +
      ggplot2::scale_y_continuous(sec.axis=ggplot2::sec_axis(trans=~./r_DW_FN, name="Number of Fronds (-)"))
  }

  gridExtra::grid.arrange(
    grobs = plist,
    widths = c(5, 1, 4),
    layout_matrix = rbind(c(1, 2, 2), c(3, 3, NA))
  )
}
