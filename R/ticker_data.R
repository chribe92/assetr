#' Ticker Data Of Several Assets Listed On Yahoo Finance
#'
#' The dataset comprises 106,431 unique tickers retrieved from Yahoo Finance. It includes the following columns:
#'
#' Ticker: The ticker symbol associated with the stock.
#' Name: The name or description of the stock.
#' Exchange: The stock exchange where the stock is traded.
#' Category Name: The category or sector to which the stock belongs.
#' Country: The country where the stock is listed or headquartered.
#' These columns provide comprehensive information about each ticker, allowing for analysis and exploration of a wide range of stocks listed on Yahoo Finance.
#'
#'
#' @format A data frame with 106 431 tickers from Yahoo Finance
#'
#'
#'
#' @source https://finance.yahoo.com/screener
#'
#' @examples
#' # Retrieve price data from World Indices
#' price <- get_price(world_indices, omit_na = T)
"tickers"
