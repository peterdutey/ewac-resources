.validate_auditc_input <- function(x) {
  x_valid <- c("audit1_label", "audit2_label", "audit3_label") %in% colnames(x)
  if ( any(!x_valid) ) {
    stop("`x` must contain columns:\n",
         paste(c("audit1_label", "audit2_label", "audit3_label")[!x_valid], collapse = ", "),
         call. = FALSE)
  }
}

#' Compute the AUDIT-C score
#'
#' @description Compute the traditional AUDIT-C score from Extended AUDIT-C data
#'
#' @param x a data frame or matrix with three character columns containing, in order: \describe{
#'    \item{audit1_label}{a value in \code{'Never'}, \code{'Monthly or less'},
#'                        \code{'2 to 4 times a month'}, \code{'2 to 3 times a week'},
#'                        \code{'4 to 5 times a week'}, \code{'6 or more times a week'}}
#'    \item{audit2_label}{a value in \code{"1 to 2"}, \code{"3 to 4"}, \code{"5 to 6"},
#'                        \code{"7 to 9"}, \code{"10 to 12"}, \code{"13 to 15"}, \code{"16 or more"}}
#'    \item{audit3_label}{a value in \code{"Never"}, \code{"Less than monthly"}, \code{"Monthly"},
#'                        \code{"Weekly"}, \code{"Daily or almost daily"}}
#' }
#' @param audit_coefficients a list of coefficients (default: \code{\link[EWAC]{default_coefficients}})
#' @param zero_if_never_drinks a boolean indicating whether an AUDIT-C score of zero should be
#' given by default if AUDIT-1 = 'Never'. Default is FALSE.
#'
#' @return a vector of AUDIT-C score between 0 and 12.
#' @export
#' @examples
#' compute_auditc_score(data.frame(list(
#'    audit1_label = "2 to 3 times a week",
#'    audit2_label = "1 to 2",
#'    audit3_label = "Monthly"
#' )))
compute_auditc_score <- function(x,
                                 audit_coefficients = default_coefficients,
                                 zero_if_never_drinks = FALSE){

  .validate_auditc_input(x)

  x[,c("audit1_score")] <- audit_coefficients$audit1[match(x[["audit1_label"]], audit_coefficients$audit1$audit1_label), c("audit1_score")]
  x[,c("audit2_score")] <- audit_coefficients$audit2[match(x[["audit2_label"]], audit_coefficients$audit2$audit2_label), c("audit2_score")]
  x[,c("audit3_score")] <- audit_coefficients$audit3[match(x[["audit3_label"]], audit_coefficients$audit3$audit3_label), c("audit3_score")]

  if(zero_if_never_drinks){
    x$audit1_score + ifelse(x$audit1_score == 0, 0, x$audit2_score + x$audit3_score)
  } else {
    x$audit1_score + x$audit2_score + x$audit3_score
  }
}

#' Compute the EWAC
#'
#' @description Compute the Estimated Weekly Alcohol Consumption from Extended AUDIT-C data
#'
#' @param x a data frame or matrix with three character columns containing, in order: \describe{
#'    \item{audit1_label}{a value in \code{'Never'}, \code{'Monthly or less'},
#'                        \code{'2 to 4 times a month'}, \code{'2 to 3 times a week'},
#'                        \code{'4 to 5 times a week'}, \code{'6 or more times a week'}}
#'    \item{audit2_label}{a value in \code{"1 to 2"}, \code{"3 to 4"}, \code{"5 to 6"},
#'                        \code{"7 to 9"}, \code{"10 to 12"}, \code{"13 to 15"}, \code{"16 or more"}}
#'    \item{audit3_label}{a value in \code{"Never"}, \code{"Less than monthly"}, \code{"Monthly"},
#'                        \code{"Weekly"}, \code{"Daily or almost daily"}}
#' }
#' @param audit_coefficients a list of coefficients (default: \code{\link[EWAC]{default_coefficients}})
#' @param uk_units if \code{TRUE} (the default), the result is expressed in
#' alcohol units (8g or 10mL of pure ethanol) per week.
#' If \code{FALSE}, the result is expressed in grams per week.
#' @return a numeric vector
#' @export
#' @examples
#' compute_ewac(data.frame(list(
#'    audit1_label = "2 to 3 times a week",
#'    audit2_label = "1 to 2",
#'    audit3_label = "Monthly"
#' )))
compute_ewac <- function(x, audit_coefficients = default_coefficients, uk_units = TRUE){

  .validate_auditc_input(x)
  stopifnot(is.logical(uk_units))

  x[,c("audit1_value")] <- audit_coefficients$audit1[match(x[["audit1_label"]], audit_coefficients$audit1$audit1_label), c("audit1_value")]
  x[,c("audit2_value")] <- audit_coefficients$audit2[match(x[["audit2_label"]], audit_coefficients$audit2$audit2_label), c("audit2_value")]
  x[,c("audit3_value")] <- audit_coefficients$audit3[match(x[["audit3_label"]], audit_coefficients$audit3$audit3_label), c("audit3_value")]
  binge.val <- audit_coefficients$binge_value

  ewac_uk <- ( (x$audit1_value * x$audit2_value ) + (x$audit3_value * binge.val ) )

  if (uk_units) {
    ewac_uk
  } else {
    ewac_uk * 8.0
  }
}

