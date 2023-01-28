#' Run the snow routine with P and T data
#'
#' @param precip A numeric vector for precipitation.
#' @param temp A numeric vector for temperature
#' @param param A numeric vector for model parameters: 1. Ts = threshold temperature [C]; 2. MF = melt factor [mm/C]; 3. CFR = refreezing factor [-]; 4. CWHv= Water holding capacity of snow [-]
#' @param subcatchment_table A file path to a subctachment table.
#'
#' @return A vector of the output of the snow routine (Psr).
#' @export

run_snow_routine <- function(precip, temp, param, subcatchment_table = NULL) {

  # check if subcatchment table exists or not
  if (!is.null(subcatchment_table)) {

    # if the user decides to provide subcatchments
    # get table
    subcatchment_table <- read.delim(subcatchment_table,
                                     sep = "\t")

    # apply snow routine for each subcatchment
    n_catchment <- max(subcatchment_table$id)

    # create empty list
    subc_results <- list()

    # for each subcatchment
    for (i in 1:n_catchment) {
      # get T vector with corresponding shift with temp_shift value
      temp_sub <- temp + subcatchment_table$temp_shift[i]

      # calculate snow routine
      snow_routine_results <- snow_routine(temp_sub, precip, param)

      # calculate snow routine output with corresponding proportion with proportion value
      subc_results[[i]] <- snow_routine_results$P * subcatchment_table$proportion[i]
    }

    # sum all the different vectors of the subcatchment to get total snow routine output Psr
    Psr <- Reduce(`+`, subc_results)

    return(Psr)

  } else {
    # calculate snow routine
    snow_routine_results <- snow_routine(temp, precip, param)

    # get snow routine output Psr
    Psr <- snow_routine_results$P

    return(Psr)
  }
}

