#' Export UFC data for a fighter to a CSV
#'
#' @param fighter_name Character. Name of the fighter.
#' @param file_path Character. Path to the CSV file.
#'
#' @examples
#' \dontrun{
#'   # This example fetches data for Israel Adesanya and saves it to a CSV file
#'   export_ufc_data_csv("Israel Adesanya", "Israel_Adesanya_Fight_Data.csv")
#' }
#'
#' @export
export_ufc_data_csv <- function(fighter_name, file_path) {
  df <- get_ufc_data(fighter_name)
  write.csv(df, file_path, row.names = FALSE)
  invisible(TRUE)
}
