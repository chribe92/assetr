% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/world_indices_data.R
\docType{data}
\name{world_indices}
\alias{world_indices}
\title{Ticker And Symbol Data Of World Indices As Listed On Yahoo Finance}
\format{
A data frame with 33 of large world indices
}
\source{
https://finance.yahoo.com/world-indices/
}
\usage{
world_indices
}
\description{
The request_symbols variable contains the input symbols requested by the user,
while the tickers variable provides information on the corresponding ticker
symbols for the respective indices. You can use the symbols in the format
"world_indices$request_symbols" as input for functions like get_price in the assetr package.
}
\examples{
# Retrieve price data from World Indices
price <- get_price(world_indices, omit_na = T)
}
\keyword{datasets}
