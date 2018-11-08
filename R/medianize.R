#' Medianize
#'
#' Categorizes a continous array in two groups (low-high) using median as criteria. \cr
#'    With odd numbers, middle point is assigned to "low" group
#'
#' @param  val Array Values lista
#'
#' @return c Grouped values
#'
#' @export
#'
#' @example medianize(c(1,2,3,4))
#'
#'
medianize <- function(val){
  medVal = stats::median(val, na.rm = TRUE)
  return(
    factor(
        ifelse(val <= medVal, 'LOW', 'HIGH'),
        levels = c('LOW', 'HIGH')
      )
  )
}
