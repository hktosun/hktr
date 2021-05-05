#' Read all columns as character
#'
#' Read all columns using read_csv of readr as character.
#'
#' @param path file path
#'
#' @return A tibble
#' @export
#'



read_csv_char <- function(path){
	df <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))
	df
}
