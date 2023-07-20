#' Calculate Log Or Simple Returns
#'
#' @description
#' This function calculates log or simple returns for a data frame.
#' It is designed to be used with the `get_price()` function from the `assetr` package,
#' but can also be applied to any data frame that has a date column in the first column and prices in the remaining columns.
#'
#' @param stock_price A data.frame() containing dates in the first column and stock prices in the remaining columns.
#' @param type Type of returns to calculate: "log" for logarithmic returns and "simple" for simple returns.
#'
#' @return Returns a data.frame() containing historical closing return observations for the provided data.frame()
#' @export
#'
#' @author Christian André Berntsen
#'
#' @examples
#'
#'# Obtaining historical price data
#' df_prices <- get_price(c("AAPL", "TSLA"),
#'                     "2017-01-01",
#'                     "2023-01-01",
#'                     omit_na = T,
#'                     frequency = "daily")
#'
#' # Calculate log returns
#' df_returns <- returns(prices, type = "log")
#'
returns <- function(get_price, type = "log") {

  if (!is.data.frame(get_price)) {
    message(paste0("Input data must be of type dataframe, and have a date column in the first row, or be of type",
                   "get_price, or be",
                   "from the assetr package"))
    return()
  }
  # If only one price variable
  if (ncol(get_price) == 2){

    if (type == "simple") {
      returns <- head(as.vector(suppressWarnings(diff(get_price[, 2]) / lag(get_price[, 2]))), -1)
    }
    if (type == "log") {
      returns <- diff(log(get_price[,2]))
    }
    date <- get_price[-1, 1]
    return_data <- data.frame(date, returns)
    colnames(return_data)[2] <- colnames(get_price)[2]
    return(return_data)

    # If more than one price variable
  } else if (ncol(get_price) > 2){

    if (type == "simple") {
      suppressed_returns <- suppressWarnings({
        sapply(get_price[, -1], function(col) {
          lagged_col <- lag(col)
          if (is.null(lagged_col)) return(NA)  # Return NA for the first element
          diff(col) / lagged_col
        })
      })

      returns <- suppressed_returns[-1, , drop = FALSE]  # Remove the first row

    } else if (type == "log") {
      returns <- apply(log(get_price[, -1]), 2, diff)
    } else {
      message("Invalid type. Allowed values are 'log' or 'simple'.")
      return()
    }
    date <- get_price[-1, 1]
    return_data <- data.frame(date, returns)

    return(return_data)
  }
}


