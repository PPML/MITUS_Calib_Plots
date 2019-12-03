#' Produce a tibble with calibration plots from MITUS
#' 
#' Given a location (`loc`), `calib_plots(loc)` produces a tibble containing
#' four columns, the last of which is plots and contains ggplot2 plots. The
#' plots are provided description in the other columns. 
#' 
#' @param loc A two-letter code for the location 
#' @export
calib_plots<-function(loc){
  #create a dataframe to hold plots and accompanying data
  possible_plots<-all_possible_plots_df(loc)
  #check that the data exists for all the plots
  plots<-data_check(loc, possible_plots) #returns list of all available plots short name
  #make the plots
  for (i in 1:nrow(plots)){
    plots[[i,4]]<-make_plots(loc,as.character(plots[i,2]))
  }
  return(plots)
}
