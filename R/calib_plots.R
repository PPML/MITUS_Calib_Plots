#' Produce a tibble with calibration plots from MITUS
#'
#' Given a location (`loc`), `calib_plots(loc)` produces a tibble containing
#' four columns, the last of which is plots and contains ggplot2 plots. The
#' plots are provided description in the other columns.
#'
#' @param loc A two-letter code for the location
#' @param plots_subset (optional) a subset of shortnames to use to select which
#' plots are computed and not included in the return value plots.
#' @return A tibble with 6 columns: category, shortname, name, plot, target_data, output_data
#' @export
calib_plots<-function(loc, plots_subset = NULL){

  # create a dataframe to hold plots and accompanying data
  possible_plots<-all_possible_plots_df(loc)

  # check that the data exists for all the plots
  plots<-data_check(loc, possible_plots) #returns list of all available plots short name

  # filter the plots data frame for shortname == plots_subset to only compute a
  # subset if not plots all are needed
  if (! is.null(plots_subset)) plots <- plots[plots$shortname %in% plots_subset, ]

  #make the plots
  for (i in 1:nrow(plots)){
    print(i)
    plots[[i,4]]<-list(make_plots(loc,as.character(plots[i,2])))
  }

  # rename target_data_list(loc) -> target_data and
  # output_data_list(loc) -> model_estimate for brevity
  #
  colnames(plots)[5:6] <- c('target_data', 'model_estimate')

  return(plots)
}
