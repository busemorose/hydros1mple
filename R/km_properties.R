#' Helpfer function to extract information from KarstMod .properties file
#'
#' @param path A string vector corresponding to the path of the .properties file.
#'
#' @return A list with different information extracted from the file.
#' @export
#'
#' @examples # km_properties("C:/users/karstmod/model.properties")

km_properties <- function(path) {

  base_file <- file(path)
  file <- readLines(base_file)
  close(base_file)

  out <- list()

  out[["data_path"]] <- gsub(".*=", "", file[grepl("input-data.data-file-absolute=", file)])
  out[["output_path"]] <- gsub(".*=", "", file[grepl("run-parameters.output-directory=", file)])
  out[["warmup"]] <- gsub(".*=", "", file[grepl("run-parameters.warmup-beginning=", file)])
  out[["cal"]] <- gsub(".*=", "", file[grepl("run-parameters.calibration-stages=", file)])
  out[["val"]] <- gsub(".*=", "", file[grepl("run-parameters.validations-stages=", file)])
  out[["max_time"]] <- gsub(".*=", "", file[grepl("run-parameters.max-time=", file)])
  out[["sim_num"]] <- gsub(".*=", "", file[grepl("run-parameters.simulations-number=", file)])
  out[["wobj_min"]] <- gsub(".*=", "", file[grepl("run-parameters.wobj-min=", file)])

  x <- file[grepl("run-parameters.wobj-function.f", file)]

  for (i in 1:(length(x) / 5)) {
    out[[paste0("wobj-class", i)]] <-
      gsub(".*=", "", file[grepl(paste0("run-parameters.wobj-function.f", i, ".class="), file)])
    out[[paste0("wobj-converter", i)]] <-
      gsub(".*=", "", file[grepl(paste0("run-parameters.wobj-function.f", i, ".converter="), file)])
    out[[paste0("wobj-elt-name", i)]] <-
      gsub(".*=", "", file[grepl(paste0("run-parameters.wobj-function.f", i, ".elt-name="), file)])
    out[[paste0("wobj-parameters", i)]] <-
      gsub("\\\\", "", gsub(".*parameters=", "",
                            file[grepl(paste0("run-parameters.wobj-function.f", i, ".parameters="), file)]))
    out[[paste0("wobj-w", i)]] <-
      gsub(".*=", "", file[grepl(paste0("run-parameters.wobj-function.f", i, ".w="), file)])

    out[["wobj"]] <-
      paste0(round(as.numeric(out[[paste0("wobj-w", i)]]), 3), " * ",
             gsub(".*obj.", "", out[[paste0("wobj-class", i)]]),
             if (out[[paste0("wobj-parameters", i)]] != "") paste0("[",
                                                                 out[[paste0("wobj-parameters", i)]],
                                                                 "]"), "(",
             out[[paste0("wobj-converter", i)]], "(",
             out[[paste0("wobj-elt-name", i)]], "))",
             if (i < (length(x) / 5)) " + ")

  }

  return(out)
}



