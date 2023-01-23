#' Create confidence intervals for success and failures - binomial responses
#'
#' @param conf desired confidence inverals, defaults to 80%
#' @param data a data.frame with the values, works well as a list of dataframes
#' @param outcome a column with the outcomes as a boolean (T/F) logical outcome, only
#' required if more than one logical column exists in the data set
#'
#' @example 
#' df <- data.frame(
#'   Stratum = rep('Prussia', n = 20),
#'   Outcome = as.logical( sample(c(T,F), size = 20, replace = T) ),
#'   Water = as.logical(sample(c(T,F), size = 20, replace = T))
#" )
#' out <- WilsonCanHaveFun( data = df, outcome = 'Outcome' )
#' @export

WilsonCanHaveFun <- function(data, outcome, conf, ...){
  
  if(missing(conf)){conf <- 0.2}
  if(missing(outcome)){
    
    col_classes <- sapply(FUN = class, data)
    lgl_cols <- grep('logical', col_classes)
    
    if(length(lgl_cols) > 1){
      stop('No "success" columns specified to function, and input data frame contains more than one "logical" type column; please specify the "outcome" column')
    } else {
      outcome <- colnames(data)[lgl_cols] 
      message('No column supplied to the "Success" argument, `', 
              substitute(outcome),  '` used as the outcome variable') } 
  }
  
  outcome_col <- as.character(substitute(outcome))
  
  inType <- class(data)
  out <- if(length(grep('sf', inType)) == 0){
    outcome_var <- data[outcome_col] == T 
  } else{ 
    data <- st_drop_geometry(data)
    outcome_var <- data[outcome_col] == T
  }
  prediction <- Hmisc::binconf(x = length(outcome_var[outcome_var==T]) , 
                               n = length(outcome_var), alpha = (1 - conf), ... )
  #  prediction <-
  #    data.frame(cbind(Variable = deparse(substitute(data)), prediction))
  
  return(prediction)
}
