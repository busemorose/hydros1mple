#' Compute the Indicateur Piézométrique Standardisé (IPS)
#'
#' This function calculates a standardized index (IPS) for groundwater or other time series
#' by comparing the current situation to the distribution of past conditions over a defined
#' reference period. The method follows a density-based approach using a kernel estimator
#' and projection onto the standard normal distribution.
#'
#' The index is computed for multiple rolling monthly windows (1 to 12 months), with normalization
#' performed separately for each calendar month based on the selected reference period. The result
#' is a list of data.tables containing the IPS time series for each window size.
#'
#' @param date A vector of class `Date` indicating the time axis of the data.
#' @param value A numeric vector of the same length as `date`, giving the values to analyze (e.g., water level).
#' @param min_months Integer. Minimum number of years required for the target month across the time series
#'        (default: 15). If fewer years are available, the function stops.
#' @param min_days_in_month Integer. Minimum number of daily values required in the selected target month
#'        for it to be used (default: 15).
#' @param target_month Optional integer (1–12). Target month to compute IPS up to. If `NULL`, the latest
#'        month in the data is used.
#' @param target_year Optional integer. Target year to compute IPS up to. If `NULL`, the latest year
#'        in the data is used.
#' @param ref_year_min Optional integer. First year of the reference period used for normalization.
#'        If `NULL`, the earliest year in the dataset is used.
#' @param ref_year_max Optional integer. Last year of the reference period used for normalization.
#'        If `NULL`, the latest year in the dataset is used.
#'
#' @return A named list of `data.table` objects. Each list element corresponds to a moving average
#' window (e.g., `"m1"`, `"m2"`, `"m3"`, etc.), and contains two columns:
#' \describe{
#'   \item{date}{The timestamp at the start of each window.}
#'   \item{IPS}{The standardized value (normally distributed), computed month-by-month over the reference period.}
#' }
#'
#' @details
#' The function follows these main steps for each moving average window:
#' \enumerate{
#'   \item Compute rolling mean of monthly values over the selected window size.
#'   \item For each calendar month (January to December), extract values in that month for all years.
#'   \item Use a smoothed kernel density (Epanechnikov) to estimate the probability density function.
#'   \item Convert the density to a cumulative distribution function (CDF).
#'   \item Project the current values onto the standard normal using the inverse CDF (via `qnorm`).
#' }
#'
#' This process allows month-specific normalization, preserving the seasonal signal.
#'
#' @examples
#' # Example with synthetic data
#' set.seed(123)
#' dates <- seq(as.Date("1980-01-01"), as.Date("2023-12-31"), by = "day")
#' values <- sin(2 * pi * yday(dates) / 365) + rnorm(length(dates), 0, 0.5) + 5
#' ips_result <- IPS(dates, values)
#'
#' @import data.table
#' @importFrom lubridate year month make_date
#' @importFrom padr pad
#' @importFrom stats density approxfun qnorm
#' @export

IPS <- function(date,                   # Vector of dates
                value,                  # Vector of values
                min_months = 15,        # Minimum number of years for the selected month
                min_days_in_month = 15, # Minimum number of daily values in the selected month
                target_month = NULL,    # Default: last month of available data
                target_year = NULL,     # Default: last year of available data
                ref_year_min = NULL,    # Default: earliest year in dataset
                ref_year_max = NULL) {  # Default: latest year in dataset

  stopifnot(inherits(date, "Date"))
  if (!is.numeric(value) || length(date) != length(value)) stop("`value` must be numeric and same length as `date`.")

  # Combine date and value into a data.table
  data <- data.table(date = date, value = value)

  # Define window lengths (in months)
  window_lengths <- c(1, 2, 3, 4, 6, 12)
  IPS <- list()

  # Assign defaults if NULL
  if (is.null(target_month)) target_month <- month(max(data$date, na.rm = TRUE))
  if (is.null(target_year))  target_year  <- year(max(data$date, na.rm = TRUE))
  if (is.null(ref_year_min)) ref_year_min <- min(year(data$date), na.rm = TRUE)
  if (is.null(ref_year_max)) ref_year_max <- max(year(data$date), na.rm = TRUE)

  # Limit data strictly to selected year/month, even if the series is longer
  data_filtered <- data[date <= max(data[year(date) == target_year & month(date) == target_month, date])]
  if (nrow(data_filtered) == 0) stop("No data available up to the end of the selected month.", call. = F)

  # Ensure at least `min_days_in_month` daily values in the selected month
  month_data <- data[year(date) == target_year & month(date) == target_month]
  month_valid <- na.omit(month_data$value)
  if (length(month_valid) < min_days_in_month) stop("Not enough daily values in selected month.", call. = F)

  # Ensure the selected year exists in the filtered data
  available_years <- unique(year(data_filtered$date))
  if (tail(available_years, 1) != target_year) stop("Selected year not present in filtered data.", call. = F)

  # Compute monthly means (even if not target month)
  monthly_data <- data_filtered[, .(value = mean(value, na.rm = TRUE)), by = .(year = year(date), month = month(date))]
  if (monthly_data[.N, month] < target_month) stop("Target month not found in selected year.", call. = F)

  # Require at least `min_months` values for the selected month across the series
  n_month <- nrow(monthly_data[month == target_month])
  if (n_month < min_months) stop(paste0("Fewer than ", min_months, " available values for target month."))

  # IPS calculation loop
  for (n_months in window_lengths) {
    name <- paste0("m", n_months)

    # Compute rolling average (monthly scale)
    rolling_avg <- frollmean(monthly_data$value, n = n_months, align = "right", algo = "exact")
    rolling_dates <- make_date(monthly_data$year, monthly_data$month, 1)[!is.na(rolling_avg)]
    rolling_vals <- rolling_avg[!is.na(rolling_avg)]

    # Get index
    month_index <- month(rolling_dates)
    year_index  <- year(rolling_dates)

    # Determine min/max bounds for density estimation
    minval <- min(rolling_vals)
    maxval <- max(rolling_vals)
    if ((maxval - minval) >= 1) {
      dmin <- round(minval, 1) - 0.5
      dmax <- round(maxval, 1) + 0.5
    } else {
      dmin <- minval - 1
      dmax <- maxval + 1
    }

    # Initialize IPS table
    IPS[[name]] <- data.table(date = rolling_dates, IPS = rep(NA_real_, length(rolling_dates)))

    # Loop over all 12 calendar months
    for (m in 1:12) {
      # Step 1: Select values for the current month and define reference period
      i_all <- month_index == m
      i_ref <- i_all & year_index >= ref_year_min & year_index <= ref_year_max

      # Step 2: Compute smoothed density function (Epanechnikov kernel)
      density_fit <- density(rolling_vals[i_ref], kernel = "e", from = dmin, to = dmax, na.rm = TRUE)

      # Step 3: Compute cumulative distribution (empirical CDF)
      cdf_values <- cumsum(density_fit$y) / sum(density_fit$y)

      # Step 4: Interpolate CDF to obtain continuous mapping
      # This allows projection of any value to its cumulative probability
      cdf_function <- approxfun(density_fit$x, cdf_values)

      # Step 5: Project cumulative probability onto standard normal distribution
      # Result is the IPS standardized index
      IPS[[name]]$IPS[i_all] <- qnorm(cdf_function(rolling_vals[i_all]))
    }

    # Add missing NAs in the resulting dt
    IPS[[name]] <- suppressMessages(padr::pad(IPS[[name]]))
  }

  return(IPS)
}
