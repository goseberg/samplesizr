# .stopcheck performs a check to return an error if the users input is not
# a valid input for samplesize calculation.
# In case of binary = TRUE, effect equals p_Y and sd equals p_X

.stopcheck <- function(
  var_1,
  var_2,
  var_3,
  alpha,
  power,
  binary = FALSE,
  two.groups = TRUE
) {

  if ( is.numeric(c(var_1, var_2, var_3, alpha, power)) == FALSE ){
    stop("Non numeric argument used.")
  }

  if (two.groups == FALSE){

    if (binary == FALSE){ # f Test scenario
      if (var_2 <= 0){
        stop("sd must be a numeric greater than 0.") }
    } else { #chi square mult. groups scenario
      if (any(var_1 < 0) || any(var_1 > 1)) {
        stop("All entrys of p_A must be between 0 and 1.")
      }
    }
    if (var_3 <= 2 || var_3 %% 1 != 0){
      stop("n.groups must be an integer bigger than 2.")
    }

  } else{

    if (binary == TRUE){ # Chi Square Scenario
      if (var_1 <= var_2) {
        stop("p_Y must be larger than p_X.")
      }
      if (var_1 < 0 || var_1 > 1){
        stop("p_Y must be between 0 and 1.")
      }
      if (var_2 <= 0 || var_2 >= 1){
        stop("p_X must be between 0 and 1.")
      }
    } else { # z test t test scenario
      if (var_1 <= 0){ stop("effect must be a numeric greater than 0.") }
      if (var_2 <= 0){ stop("sd must be a numeric greater than 0.") }
    }

    # Check allocation
    num <- .get_fraction(var_3)$numerator
    den <- .get_fraction(var_3)$denominator
    if ( (var_3 %% 1) == 0 ) {den <- 1}
    if (num > 50 || den > 50){
      stop("Allocation r is not chosen practicably.
           Numerator or denominator is to big.")
    }

  }

  # Check significance level
  if (alpha <= 0 || alpha >= 1){
    stop("alpha must be a numeric between 0 an 1.")
  }

  # Check power
  if (power <= 0 || power >= 1){
    stop("power must be a numeric between 0 an 1.")
  }
}
