#' Medianize
#'
#' Categorizes a continous array in three groups (low-mid-high) using median as criteria. \cr
#'    The boundary values are included to lowest group
#'
#' @param  variable Array Values list
#' @param  withLabels Boolean Check if returns range fields/low-mid-high names
#'
#' @return c Grouped values
#'
#' @export
#'
#' @example tertilize(c(1,2,3,4)); tertilize(c(1,2,3,4, withLabels  = TRUE));
#'
#'
tertilize <- function(variable, withLabels = FALSE){
  values = cut(variable,
      breaks = stats::quantile(variable, c(0, 1/3, 2/3, 1), na.rm=TRUE),
      include.lowest = TRUE)

  if(withLabels){
    levels(values) = c('LOW', 'MID', 'HIGH')
  }
  return(values)
}
