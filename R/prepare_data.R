#' Prepare input data for exeval
#'
#' Renames user-defined dataset columns to the standardized naming
#' convention used internally by \pkg{exeval}.
#'
#' This helper function allows external datasets with arbitrary column
#' names to be reformatted for compatibility with the external evaluation
#' workflow.
#'
#' @param data A data frame containing the input dataset.
#' 
#' @param name_id Character. Name of the column containing subject IDs.
#' 
#' @param name_time Character string. Name of the sampling or event time
#' column.
#' 
#' @param name_occ Optional character. Name of the occasion column.
#' 
#' @param name_date Optional character. Name of the date column.
#' 
#' @param name_cmt Optional character. Name of the compartment column.
#' 
#' @param name_dv Optional character string. Name of the dependent variable
#' (observed concentration/response) column.
#' 
#' @param name_mdv Optional character string. Name of the missing dependent
#' variable indicator column.
#' 
#' @param name_amt Optional character. Name of the dose amount column.
#' 
#' @param name_evid Optional character. Name of the event ID column.
#' @param name_ss Optional character string. Name of the steady-state indicator
#' column.
#' 
#' @param name_dur Optional character string. Name of the infusion duration
#' column.' 
#' @param name_lag Optional character string. Name of the lag time column.
#' 
#' @param name_rate Optional character string. Name of the infusion rate
#' column.
#' 
#' @param name_ii Optional character string. Name of the interdose interval
#' column. 
#' 
#' @param name_addl Optional character string. Name of the additional doses
#' column.
#' 
#' @details
#' At minimum, \code{ID} and \code{TIME} mappings must be provided.
#'
#' Additional columns can be optionally mapped depending on the analysis
#' workflow and model requirements.
#'
#' @return A data frame with standardized column names compatible with
#' \pkg{exeval}.
#'
#' @examples
#' df <- data.frame(
#'   patient = c(1, 1, 2),
#'   time = c(0, 12, 24),
#'   conc = c(NA, 8.4, 6.1),
#'   visit = c(1, 1, 2)
#' )
#'
#' df_std <- prepare_data(
#'   data = df,
#'   name_id = "patient",
#'   name_time = "time",
#'   name_dv = "conc",
#'   name_occ = "visit"
#' )
#'
#' head(df_std)
#'
#' @seealso [exeval_ppk()]
#' @export

prepare_data <- function (data,
                          name_id = NULL,
                          name_time = NULL,
                          name_occ = NULL,
                          name_date = NULL,
                          name_cmt = NULL,
                          name_dv = NULL,
                          name_mdv = NULL,
                          name_amt = NULL,
                          name_evid = NULL,
                          name_ss = NULL,
                          name_dur = NULL,
                          name_lag = NULL,
                          name_rate = NULL,
                          name_ii = NULL,
                          name_addl = NULL
                          ){
  # Check for data frames
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  if (is.null(name_id)) {
    stop("'name_id' must be provided.")
  }

  if (is.null(name_time)) {
    stop("'name_time' must be provided.")
  }

  rename_map <- c(
    ID = name_id,
    TIME = name_time,
    OCC = name_occ,
    DATE = name_date,
    CMT = name_cmt,
    DV = name_dv,
    MDV = name_mdv,
    AMT =name_amt,
    II= name_ii,
    EVID = name_evid,
    RATE= name_rate,
    DUR = name_dur,
    ADDL = name_addl,
    SS= name_ss,
    ALAG = name_lag
  )
  # Remove NULL Values
  rename_map <- rename_map[!is.na(rename_map)]
  
  if (any(duplicated(rename_map))) {
    stop("Input column mappings must be unique.")
  }
  
  missing_cols <- rename_map[!rename_map %in% names(data)]

  if (length(missing_cols) > 0) {
    stop(
      "These columns were not found in data: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  # rename columns
  data <- dplyr::rename(data, !!!rename_map)

  return(data)
}
