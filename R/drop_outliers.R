#' Drop outliers
#'
#' Drop outliers based on certain percentiles

#' @param data Input data
#' @param var Variable to use for calculating percentiles
#' @param prob Bottom & top percentile to drop. If 0.05, top and bottom 5 percentiles will be dropped.
#' @param ... Further arguments to pass on `quantile`
#'
#' @return A tibble
#' @export
#'
drop_outliers <- function(data, var, prob, ...){

	vec <- dplyr::pull(data, {{var}})

	if(length(prob) > 2){
		stop("`prob` length should be 1 or 2.")
	}

	if(!is.numeric(prob)){
		stop("`prob` should be numeric.")
	}

	var_name <- deparse(substitute(var))

	if(length(prob) == 1){
		prob <- rep(prob, 2)
	}

	bottom_quantile <- as.numeric(stats::quantile(vec, prob[1], ...))
	top_quantile <- as.numeric(stats::quantile(vec, 1 - prob[2], ...))

	text <- paste0("Dropping bottom ", 100 * prob[1], " and top ", 100 * prob[2], " percentiles using ", var_name, ".")
	message(text)

	dplyr::filter(data, {{var}} >= bottom_quantile, {{var}} <= top_quantile)

}
