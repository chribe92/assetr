#' Get Stock Price Data
#'
#' @description
#' The function extends on the `getSymbols` function from the `quantmod` package,
#' developed by Jeffrey A. Ryan, to retrieve historical closing prices from Yahoo Finance.
#' The function provides the flexibility to retrieve historical price data for multiple assets.
#' It allows you to choose between retrieving daily data or aggregating the data into monthly or yearly averages.
#'
#'
#'
#' @param tickers A character vector or a concatenated string specifying one or several asset symbols.
#' @param start_date A string in the format "YYYY-MM-DD" indicating the start date of the data retrieval.
#' @param end_date A string in the format "YYYY-MM-DD" indicating the end date of the data retrieval.
#' @param omit_na A logical value indicating whether or not to omit missing observations. Set to TRUE to exclude any missing values from the retrieved data, or set to FALSE to include them.
#' @param frequency A character argument specifying the frequency of the data retrieval. Accepts the values "daily", "monthly", or "yearly" to indicate the desired frequency of the retrieved data.
#'
#'
#'
#' @return Returns a data.frame() containing historical closing prices for the provided asset symbol(s).
#' @export
#'
#' @author Christian André Berntsen
#'
#' @examples
#'
#' # Return daily historical closing prices for one asset ticker/symbol
#' get_price(
#'   tickers = "AAPL",
#'   start_date = "2020-01-01",
#'   end_date = "2020-05-01",
#'   omit_na = TRUE,
#'   frequency = "daily"
#'             )
#'
#' # Return monthly historical closing prices for three asset tickers/symbols
#' get_price(
#'   tickers = c("AAPL", "MSFT", "TSLA"),
#'   start_date = "2020-01-01",
#'   end_date = "2020-05-01",
#'   omit_na = TRUE,
#'   frequency = "monthly"
#'             )
#'
get_price <- function(tickers,
                      start_date= (lubridate::today()-365),
                      end_date = lubridate::today(),
                      omit_na = FALSE,
                      frequency = "daily") {

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  valid_frequencies <- c("daily", "monthly", "yearly")
  if (!(frequency %in% valid_frequencies)) {
    warning("Invalid frequency. Please use 'daily', 'monthly', or 'yearly'.")
    return(NULL)
  }

  dataframes <- list()

  for (ticker in tickers) {
    stock_data <- quantmod::getSymbols(ticker,
                                       from = start_date,
                                       to = end_date,
                                       auto.assign = FALSE
                                       )

    price_col <- paste0(ticker,
                        ".Close")

    df <- data.frame(date = zoo::index(stock_data),
                     Close = zoo::coredata(stock_data[, price_col]))

    colnames(df)[2] <- ticker

    dataframes[[ticker]] <- df
  }

  merged_df <- Reduce(function(x, y) merge(x, y,
                                           by = "date",
                                           all = TRUE),
                      dataframes)

  if (omit_na) {
    merged_df <- merged_df[complete.cases(merged_df), ]
  }
  rownames(merged_df) <- NULL

  if (frequency == "monthly") {
    # Set the date to the first day of each month
    merged_df$date <- as.Date(format(merged_df$date, "%Y-%m-01"))
    merged_df <- aggregate(. ~ date, data = merged_df, FUN = mean)
  } else if (frequency == "yearly") {
    # Set the date to the first day of each year
    merged_df$date <- as.Date(format(merged_df$date, "%Y-01-01"))
    merged_df <- aggregate(. ~ date, data = merged_df, FUN = mean)
  }

  return(merged_df)
}


