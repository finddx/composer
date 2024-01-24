#' Valid country codes and names
#'
#' This is a data frame of ISO3c codes and names of countries. It is used to
#' check the user input, to see whether the codes are ISO3 codes, and therefore
#' we can plot the data on the map. The codes and names were taken from the
#' "countrycode" package on 26/06 and may need to be updated every now and then.
#' Note that while ISO3 codes are a standard, country names can vary significantly.
#'
#' @format A data frame with 249 rows and 4 variables.
#'
#' @source \url{https://github.com/vincentarelbundock/countrycode}
"valid_countries"
