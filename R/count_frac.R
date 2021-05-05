#' Create percentages within group
#'
#' One-line solution for count -> group_by -> mutate -> ungroup.

#' @importFrom rlang .data
#' @param data Input data.
#' @param ... variables. Last one is the one we do not group_by.
#'
#' @return A tibble
#' @export
#'
#'
count_frac <- function(data, ...){

	d <- dplyr::count(data, !!!rlang::ensyms(...))

	m <- ncol(d)

	d <- dplyr::group_by_at(d, 1:(m-2))
	d <- dplyr::mutate(d, frac = .data$n / sum(.data$n))
	d <- dplyr::ungroup(d)
}
