#' @title Plot daily fishing mortality dynamics

#' @description \code{plot_daily_fdyn} plots the daily fishing mortality
#' dynamics by year.

#' @param results is output from the function \link{run_sim}.

#' @return is a matplot of the daily fishing mortality dynamics

#' @examples
#' plot_daily_fdyn(results = results)

#' @export

plot_daily_fdyn <- function (results) {
	
	n_spp <- length(res[["pop_summary"]]) 
	res_spp <- lapply(seq_len(n_spp), function(x) {
 			 x1_res <- tidyr::gather(as.data.frame(t(results[["pop_summary"]][[x]][["F.mat"]])), key = "year", factor_key = T)
			 F_df <- data.frame("pop" = paste0("spp_", x), 
					    "day" = rep(1:362,length.out = nrow(x1_res)),
					    "year" = x1_res$year,
					    data = x1_res$value)
			 
				  })
	res_out <- do.call(rbind, res_spp)

	require(ggplot2)
	print(ggplot(res_out, aes(x = day, y = data)) + geom_line(aes(colour = year)) + facet_wrap(~pop, scale = "free"))

}
