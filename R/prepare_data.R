#' Prepare input data for preDose
#'
#' Standardizes user-defined column names to the internal naming
#' convention used in preDose.
#'
#' @param data A data frame containing the input dataset.
#' @param name_id Character. Name of the column containing subject IDs.
#' @param name_time Character. Name of the column containing sampling or event times.
#' @param name_occ Optional character. Name of the occasion column.
#' @param name_date Optional character. Name of the date column.
#' @param name_cmt Optional character. Name of the compartment column.
#' @param name_dv Optional character. Name of the dependent variable column.
#' @param name_mdv Optional character. Name of the missing dependent variable indicator column.
#' @param name_amt Optional character. Name of the dose amount column.
#' @param name_evid Optional character. Name of the event ID column.
#' @param name_ss Optional character. Name of the steady-state indicator column.
#' @param name_dur Optional character. Name of the infusion duration column.
#' @param name_lag Optional character. Name of the lag time column.
#' @param name_rate Optional character. Name of the infusion rate column.
#' @param name_ii Optional character. Name of the interdose interval column.
#' @param name_addl Optional character. Name of the additional doses column.
#'
#' @return A data frame with standardized column names.
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
