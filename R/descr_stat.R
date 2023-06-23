#' Generate Descriptive Statistics For A Specific Period Or The Entire Dataset
#'
#' @param data A data.frame() containing dates in the first column and numbers in the remaining columns
#' @param start_date An optional string in the format "YYYY-MM-DD" indicating the start date of the data retrieval
#' @param end_date An optional string in the format "YYYY-MM-DD" indicating the end date of the data retrieval
#' @param day_count Annualised measure for return and standard deviation
#' @param digits A number that determines the decimal places for rounding numbers
#'
#' @usage descr_stat(prices, start_date, end_date, day_count, digits)
#'
#' @return Returns a data.frame() that includes a range of descriptive statistics for the given input data.frame()
#' @export
#'
#' @examples
#'
#' df <- descr_stat(prices,
#'                  start_date = "2022-01-01",
#'                  end_date = "2023-01-01",
#'                  day_count = 252,
#'                  digits = 2)
#'
descr_stat <- function(data,
                       start_date = data[1,1],
                       end_date = data[,1][nrow(data)],
                       day_count = 1,
                       digits = 2) {

  n <- ncol(data)

  if (is.null(start_date)) {
    filtered_data <- data
  } else {
    filtered_data <- data[data[, 1] >= start_date & data[, 1] <= end_date, ]
  }

  if (ncol(filtered_data) == 2) {
    mean_val <- mean(filtered_data[, 2])
    stdDev_val <- sd(filtered_data[, 2])

    df <- data.frame(
      name = colnames(filtered_data)[2:ncol(filtered_data)],
      mean = mean_val*day_count,
      stdDev = stdDev_val*sqrt(day_count),
      sharpe = mean_val / stdDev_val,
      min = min(filtered_data[, 2]),
      max = max(filtered_data[, 2]),
      kurtosis = moments::kurtosis(filtered_data[, 2]),
      skewness = moments::skewness(filtered_data[, 2]),
      row.names = NULL,
      check.names = TRUE
    )

    df[, 2:8] <- round(df[, 2:8], digits)

    return(df)

  } else {

    # Calculate mean
    mean_values <- colMeans(filtered_data[, -1], na.rm = TRUE)*day_count
    # Calculate standard deviation
    std_dev_values <- apply(filtered_data[, -1], 2, sd, na.rm = TRUE)*sqrt(day_count)
    # Calculate Sharpe ratio
    sharpe_ratio <- mean_values / std_dev_values
    # Calculate minimum
    min_values <- apply(filtered_data[, -1], 2, min, na.rm = TRUE)
    # Calculate maximum
    max_values <- apply(filtered_data[, -1], 2, max, na.rm = TRUE)
    # Calculate kurtosis using moments package
    kurtosis_values <- moments::kurtosis(filtered_data[, -1], na.rm = TRUE)
    # Calculate skewness using moments package
    skewness_values <- moments::skewness(filtered_data[, -1], na.rm = TRUE)

    # Create a new data.frame with the calculated statistics
    df <- data.frame(
      name = colnames(filtered_data)[2:ncol(filtered_data)],
      mean = mean_values,
      stdDev = std_dev_values,
      sharpe = sharpe_ratio,
      min = min_values,
      max = max_values,
      kurtosis = kurtosis_values,
      skewness = skewness_values,
      row.names = NULL,
      check.names = TRUE
    )

    df[, 2:8] <- round(df[, 2:8], digits)

    return(df)

  }
}


