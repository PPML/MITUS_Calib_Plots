#add in some dependencies
calib_plots<-function(loc){
#create a dataframe to hold plots and accompanying data
  calib_plots_ma<-all_possible_plots_df(loc)
#check that the data exists for all the plots
  plots<-data_check(loc, calib_plots) #returns list of all available plots short name
#make the plots
  for (i in 1:nrow(plots)){
    plots[[i,4]]<-make_plots(loc,as.character(plots[i,2]))
  }
  return(plots)
}
