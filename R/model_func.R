#' Calculate poly model predicted values for data
#'
#' This function applied polynomial model to input data and
#' returns a vector with the associated predicted values.
#'
#' @param model_x model object
#' @param x input data
#' @return A dataframe with model predicted values


model_func = function(model_x,x){

  model_curve = c()

  for ( i in seq_along(x) ) {

    model_val = 0

    for( q in rev(seq_along(model_x$coefficients))[-length(model_x$coefficients)] ){
      p = q - 1
      model_val = model_val + model_x$coefficients[q] * x[i] ^ p
    }
    model_curve[i] = model_val + model_x$coefficients[1]
  }

  return(model_curve)
}
