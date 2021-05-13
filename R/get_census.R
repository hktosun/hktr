create_census_url <- function(variable, year, CENSUS_API_KEY){
	url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=block:*&in=state:27&in=county:*&in=tract:*&&key=", CENSUS_API_KEY)
}

read_url <- function(url){
	a <- jsonlite::fromJSON(url) %>%
		tibble::as_tibble()
	names(a) <- c("var", as.character(a[1, 2:ncol(a)]))
	a <- a[-1, ]

}

census_pivot_wider <- function(data){
	data %>%
		dplyr::select(-.data$url, -.data$variable_cat) %>%
		tidyr::unnest(.data$data) %>%
		dplyr::mutate(census_block_2010 = paste0(.data$state, .data$county, .data$tract, .data$block)) %>%
		dplyr::select(-.data$state, -.data$county, -.data$tract, -.data$block, -.data$NAME, -.data$variable_code) %>%
		dplyr::mutate(var = as.numeric(.data$var)) %>%
		tidyr::pivot_wider(names_from = .data$variable_label, values_from = .data$var)
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

	vars <- census_vars %>%
		dplyr::filter(.data$variable_cat %in% variable)


	mn <- vars %>%
		dplyr::mutate(url = purrr::map_chr(.data$variable_code, ~create_census_url(.x, year, CENSUS_API_KEY))) %>%
		dplyr::mutate(data = purrr::map(url, ~read_url(.x)))

	mn <- mn %>%
		census_pivot_wider()

	return(mn)

}





