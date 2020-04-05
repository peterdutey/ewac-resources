

#' Compute the AUDIT-C score
#' 
#' @description Compute the traditional AUDIT-C score from Extended AUDIT-C data 
#'
#' @param data a data frame containing Extended AUDIT-C data
#' @param audit1  
#' @param coefficients a list of EWAC coefficients (see \code{\link[EWAC]{get_ewac_coefficients}()})
#' @param zero_if_never_drinks a boolean indicating whether an AUDIT-C score of zero should be 
#' given by default if AUDIT-1 = 'Never'. Default is FALSE.
#'
#' @return a vector of AUDIT-C score between 0 and 12.
#' @export
#' @examples
compute_AUDITC_score <- function(audit1, audit2, audit3, 
                                 coefficients = get_ewac_coefficients(), zero_if_never_drinks = FALSE){
  
  
  
  X[,c("audit1_score")] <- audit_coef$audit1[match(X[,1], audit_coef$audit1$audit1_label), c("audit1_score")]
  X[,c("audit2_score")] <- audit_coef$audit2[match(X[,2], audit_coef$audit2$audit2_label), c("audit2_score")]
  X[,c("audit3_score")] <- audit_coef$audit3[match(X[,3], audit_coef$audit3$audit3_label), c("audit3_score")]
  
  if(zero_if_never_drinks){
    X$audit1_score + ifelse(X$audit1_score == 0, 0, X$audit2_score + X$audit3_score )
  } else {
    X$audit1_score + X$audit2_score + X$audit3_score 
  }
}

proc_AUDIT_risk <- function(X){
  # Compute Public Health England alcohol risk levels
  # X: vector of AUDIT-C scores
  cut(X, breaks = c(0, 5, 8, 13), right = F,
      labels = c("Low", "Increasing", "High"))
}


proc_EWAC <- function(X, audit_coef, method = "qfv"){
  # Compute the EWAC
  # X:                 data.frame containing AUDIT1, AUDIT2, and AUDIT3 columns in order 
  # audit_coef:        list of coefficients to apply to AUDIT response items (see audit_weights.R)
  # method:            character string indicating the estimation method: 
  #                      - "qfv" for the quantity-frequency-variably method
  #                      - "qf" for the simple quantity-frequency method.
  # binge.val:         number of UK units assumed for a bringe drinking session. Default is 8.
  
  X[,c("audit1_value")] <- audit_coef$audit1[match(X[,1], audit_coef$audit1$audit1_label), c("audit1_value")]
  X[,c("audit2_value")] <- audit_coef$audit2[match(X[,2], audit_coef$audit2$audit2_label), c("audit2_value")]
  X[,c("audit3_value")] <- audit_coef$audit3[match(X[,3], audit_coef$audit3$audit3_label), c("audit3_value")]
  binge.val <- audit_coef$binge_value
  if (method == "qf"){
    
    ( X$audit1_value * ifelse(X$audit1_value==0, NA, 
                              X$audit2_value) ) /52.1 
    
  } else if (method == "qv"){
    
    ( X$audit2_value * X$audit3_value ) / 52.1 
    
  } else if (method == "qfv") {
    
    ( (X$audit1_value * X$audit2_value ) + 
        (X$audit3_value * binge.val ) ) / 52.1
    
  } else if (method == "qb") {
    
    ( X$audit3_value * binge.val ) / 52.1
    
  } else if (method == "ewac") {
    
    ifelse(X$audit2_value >= binge.val,
           ifelse(X$audit1_value >= X$audit3_value,
                  #QF:
                  X$audit1_value * X$audit2_value  / 52.1,
                  #QV:
                  X$audit3_value * X$audit2_value  / 52.1),
           ifelse(X$audit1_value >= X$audit3_value,
                  #binge x F
                  X$audit1_value * binge.val / 52.1, 
                  #QF + binge x V
                  ((X$audit1_value * X$audit2_value ) + (X$audit3_value * binge.val ) ) /52.1))
    
    
  } else {
    
    stop("Invalid method input.")
    
  }
  
}


get_ewac_coefficients <- function(){
  list(
    audit1= data.frame(list(
      audit1_label = c('Never', 'Monthly or less',
                       '2 to 4 times a month', '2 to 3 times a week',
                       '4 to 5 times a week', '6 or more times a week'),
      audit1_value = c(0,
                       9.509,
                       29.292,
                       84.089,
                       173.247,
                       269.874),
      audit1_score = c( 0L, 1L, 2L, 3L, 4L, 4L))),
    
    audit2 = data.frame(list(
      audit2_label = c("1 to 2", "3 to 4", "5 to 6",
                       "7 to 9", "10 to 12", "13 to 15", "16 or more"),
      audit2_value = c(2.422,
                       4.342,
                       5.842,
                       6.891,
                       9.67,
                       9.725,
                       17.832),
      audit2_score = c(0L, 1L, 2L, 3L, 4L, 4L, 4L))),
    
    audit3 = data.frame(list(
      audit3_label = c("Never", "Less than monthly", "Monthly",
                       "Weekly", "Daily or almost daily"),
      audit3_value = c(4.383,
                       15.154,
                       29.542,
                       63.061,
                       5 * 365 / 7),
      audit3_score = 0:4)),
    
    binge_value = 6.448
  )
}