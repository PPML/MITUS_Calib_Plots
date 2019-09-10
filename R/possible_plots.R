all_possible_plots_df<-function(){

  short_names_vec<-c("Total_Population",
                     "Pop_Nat_Time",
                     "Deaths_Time",
                     "TB_Cases_Time",
                     "TB_Cases_Recent_Time",
                     "TB_Cases_Age",
                     "TB_Cases_Age_Time",
                     "TB_NUS_percent",
                     "TB_recent_NUS_percent",
                     "ltbi_prev_US",
                     "ltbi_prev_NUS",
                     "TB_Deaths_Year",
                     "TB_Deaths_Age_Time",
                     "Treatment_Outcomes")
  plot_cats<-c(rep("Demography",3),rep("Epidemiology",11))
  long_names_vec<-c("Population:Total, US, and Non-US Born",
                   "Population by Age for Non-US Born and US Born",
                   "Mortality: Total, US, and Non-US Born",
                   "Total TB Cases Identified",
                   "Total TB Cases Identified in Recent Years",
                   "Total TB Cases by Age",
                   "TB Cases by Age in Recent Years",
                   "Percent of TB Cases in Non-US Born Population",
                   "Percent of Non-US Born TB Cases Arrived in Past 2 Years",
                   "LTBI in US Born Population by Age",
                   "LTBI in Non-US Born Population by Age",
                   "TB Deaths in Recent Years",
                   "TB Deaths by Age Group",
                   "TLTBI Treatment Outcomes")

  category<-as.factor(plot_cats)
  shortname<-as.vector(short_names_vec)
  name<-as.vector(long_names_vec)
  plot<-lapply(1:14, function(x) list())
  possible_plots<-tibble(category,shortname,name,plot)
return(possible_plots)
}
