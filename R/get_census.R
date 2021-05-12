create_url <- function(year, variable, county, CENSUS_API_KEY){
	url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=block:*&in=state:27%20county:", county,  "&key=", CENSUS_API_KEY)
}

read_url <- function(url){
	a <- jsonlite::fromJSON(url) %>%
		tibble::as_tibble()
	names(a) <- c("var", as.character(a[1, 2:ncol(a)]))
	a <- a[-1, ]

}


#' Get Census Data
#'
#' Get Census data about population, income, etc.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @param variable Census variable
#' @param year Census vintage
#' @param CENSUS_API_KEY Census API Key
#'
#' @return A tibble of Census data.

#' @export

get_census <- function(variable, year = 2010, CENSUS_API_KEY = Sys.getenv("CENSUS_API_KEY")){

	if(!year %in% c(2010)){
		stop("`year` should be 2010. 2020 will be made available later.")
	}

	if(CENSUS_API_KEY == "" | is.null(CENSUS_API_KEY) | is.na(CENSUS_API_KEY)){
		stop("Provide a valid API Key.")
	}

	if(variable == "population"){
		variable_code <- "P001001"
		vars <- tibble::tibble(variable_code = variable_code, variable_label = "total_pop")
	}

	if(variable == "race"){
		variable_code <- c("P006002", "P006003", "P006004", "P006005", "P006006", "P006007")
		vars <- tibble::tibble(variable_code = variable_code, variable_label = c("white", "black", "aian", "asian", "nhpi", "other"))
	}


	mn <- tidyr::expand_grid(id = 1:87, vars) %>%
		dplyr::mutate(id = stringr::str_pad(2 * .data$id - 1, width = 3, side = "left", pad = "0")) %>%
		dplyr::mutate(url = purrr::map2_chr(.data$id, .data$variable_code, ~create_url(year, .y, .x, CENSUS_API_KEY))) %>%
		dplyr::mutate(data = purrr::map(url, ~read_url(.x)))

	mn <- mn %>%
		dplyr::select(-.data$id, -.data$url) %>%
		tidyr::unnest(.data$data) %>%
		dplyr::mutate(census_block_2010 = paste0(.data$state, .data$county, .data$tract, .data$block)) %>%
		dplyr::select(-.data$state, -.data$county, -.data$tract, -.data$block, -.data$NAME, -.data$variable_code) %>%
		dplyr::mutate(var = as.numeric(.data$var)) %>%
		tidyr::pivot_wider(names_from = .data$variable_label, values_from = .data$var)

	# mn <- mn %>%
	# 	dplyr::mutate(var = as.numeric(var)) %>%
	# 	dplyr::rename(!!variable := var)

	return(mn)

}
