create_acs_url <- function(variable, year, CENSUS_API_KEY){
	url <- paste0("https://api.census.gov/data/", year, "/acs/acs5?get=NAME,", variable, "&for=block%20group:*&in=state:27&in=county:*&in=tract:*&key=", CENSUS_API_KEY)
}

acs_read_url <- function(url){
	a <- jsonlite::fromJSON(url) %>%
		tibble::as_tibble()
	names(a) <- c(a[1, 1], "var", a[1, 3:ncol(a)])
	a <- a[-1, ]

}

acs_pivot_wider <- function(data){
	data %>%
		dplyr::select(-.data$url, -.data$variable_cat) %>%
		tidyr::unnest(.data$data) %>%
		dplyr::mutate(census_block_group_2010 = paste0(.data$state, .data$county, .data$tract, .data$`block group`)) %>%
		dplyr::select(-.data$state, -.data$county, -.data$tract, -.data$`block group`, -.data$NAME, -.data$variable_code) %>%
		dplyr::mutate(var = as.numeric(.data$var)) %>%
		tidyr::pivot_wider(names_from = .data$variable_label, values_from = .data$var)
}


#' Get ACS Data
#'
#' Get ACS data about population, income, etc.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @param variable ACS variable
#' @param year ACS 5-year vintage (end year)
#' @param CENSUS_API_KEY Census API Key
#'
#' @return A tibble

#' @export

get_acs <- function(variable, year = 2019, CENSUS_API_KEY = Sys.getenv("CENSUS_API_KEY")){


	if(CENSUS_API_KEY == "" | is.null(CENSUS_API_KEY) | is.na(CENSUS_API_KEY)){
		stop("Provide a valid API Key.")
	}

	vars <- acs_vars %>%
		dplyr::filter(.data$variable_cat %in% variable)


	mn <- vars %>%
		dplyr::mutate(url = purrr::map_chr(.data$variable_code, ~create_acs_url(.x, year, CENSUS_API_KEY))) %>%
		dplyr::mutate(data = purrr::map(url, ~acs_read_url(.x)))

	mn <- mn %>%
		acs_pivot_wider()

	return(mn)

}
