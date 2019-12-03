#' @inheritParams calib_plots
output_data_list<-function(loc){
  ## "Population: Total, US, and Non-US Born",
  pop_output<-list.files(pattern="pop_yr_nat",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  pop_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/", pop_output), package="MITUS"))
  pop_output<-as.data.frame(cbind(1950:2017,(pop_output[,1]+pop_output[,2]),pop_output[,1],pop_output[,2]))
  colnames(pop_output)<-c("year", "total pop.", "US born pop.", "non-US born pop.")
  ## "Population by Age for Non-US Born and US Born",
  pop_age_output<-list.files(pattern="pop_ag_nat",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  pop_age_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/", pop_age_output), package="MITUS"))

  ## "Mortality: Total, US, and Non-US Born",
  mortality_output<-list.files(pattern="mort_yr_nat",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  mortality_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/", mortality_output), package="MITUS"))
  mortality_output<-as.data.frame(cbind(1950:2016,mortality_output))

  ##  "Total TB Cases Identified",
  TB_cases_output<-list.files(pattern="TBcases",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  TB_cases_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/", TB_cases_output), package="MITUS"))
  TB_cases_output<-cbind(TB_cases_output[[1]]*1e3,
                      c(rep(NA,length(TB_cases_output[[1]])-length(TB_cases_output[[2]])),TB_cases_output[[2]]*1e3),
                      c(rep(NA,length(TB_cases_output[[1]])-length(TB_cases_output[[3]])),TB_cases_output[[3]]*1e3))
  # TB_cases_output<-TB_cases_output[,]
  # TB_cases_output<-cbind(years,TB_cases_output)
  # TB_cases_output<-as.data.frame(TB_cases_output)
  # colnames(TB_cases_output)<-c("year", "total TB cases", "US born TB cases", "non-US born TB cases")

  ##  "Total TB Cases Identified in Recent Years",
  TB_cases_10yr_output<-TB_cases_output[(nrow(TB_cases_output)-9):nrow(TB_cases_output),]

  ##  "Total TB Cases by Age",
  TB_cases_age_output<-list.files(pattern="age_cases_tot",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  outcomes_df0<-readRDS(system.file(paste0(loc, "/calibration_outputs/", TB_cases_age_output), package="MITUS"))
  #get the last ten years
  outcomes_df0<-outcomes_df0[(nrow(outcomes_df0)-10):nrow(outcomes_df0),] #last 10 years
  #add the last two age groups
  outcomes_df1<-outcomes_df0[,-12]; outcomes_df1[,11]<-outcomes_df1[,11]+outcomes_df0[,12]
  #sum across years
  TB_cases_age_output<-(colSums(outcomes_df1[,2:11])/sum(outcomes_df1[,2:11]))*100
  TB_cases_age_output<-as.data.frame(TB_cases_age_output)
  rownames(TB_cases_age_output)<-c("0-4 years","5-14 years","15-24 years ",
                                   "25-34 years","35-44 years","45-54 years",
                                   "55-64 years","65-74 years","75-84 years","85+ years")
  colnames(TB_cases_age_output)<-"percentage"
  ##  "TB Cases by Age in Recent Years",
  TB_cases_age_time_output<-list.files(pattern="age_cases_4grp",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  TB_cases_age_time_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/", TB_cases_age_time_output), package="MITUS"))
  TB_cases_age_time_output<-cbind(2000:2016,TB_cases_age_time_output)

  ##  "Percent of TB Cases in Non-US Born Population",
  NUS_cases_output<-list.files(pattern="TBcases",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  NUS_cases_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/", NUS_cases_output), package="MITUS"))
  NUS_cases_output<-cbind(2006:2016,(NUS_cases_output[[3]][14:24]/NUS_cases_output[[1]][54:64])*100)
  NUS_cases_output<-as.data.frame(NUS_cases_output)
  colnames(NUS_cases_output)<-c("year", "% cases non-US born model output")
  ##  "Percent of Non-US Born TB Cases Arrived in Past 2 Years",
  NUS_recent_cases_output<-list.files(pattern="percentRecentFBcases",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  NUS_recent_cases_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/",   NUS_recent_cases_output), package="MITUS"))
  NUS_recent_cases_output<-cbind(2004:2014,NUS_recent_cases_output[12:22])
  NUS_recent_cases_output<-as.data.frame(NUS_recent_cases_output)
  colnames(NUS_recent_cases_output)<-c("year", "% non-US born cases from recent immigrants (<2yrs) model output")
  ##  "LTBI in US Born Population by Age",
  US_LTBI_output<-list.files(pattern = "_USB_LTBI_pct", system.file(paste0(loc, "/calibration_outputs/"), package="MITUS"))
  US_LTBI_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/",   US_LTBI_output), package="MITUS"))

  ##  "LTBI in Non-US Born Population by Age",
  NUS_LTBI_output<-list.files(pattern = "NUSB_LTBI_pct", system.file(paste0(loc, "/calibration_outputs/"), package="MITUS"))
  NUS_LTBI_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/",   NUS_LTBI_output), package="MITUS"))

  ##  "TB Deaths in Recent Years",
  TB_deaths_output<-list.files(pattern="TBdeaths_",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  TB_deaths_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/",   TB_deaths_output), package="MITUS"))
  TB_deaths_output<-cbind(2004:2014,TB_deaths_output*1e6)

  ##  "TB Deaths by Age Group",
  TB_deaths_age_output<-list.files(pattern="TBdeathsAge",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  TB_deaths_age_output<-readRDS(system.file(paste0(loc, "/calibration_outputs/",   TB_deaths_age_output), package="MITUS"))

  ##  "TLTBI Treatment Outcomes"
  Treatment_outputs<-list.files(pattern="txOutcomes",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS"))
  Treatment_outputs<-readRDS(system.file(paste0(loc, "/calibration_outputs/",Treatment_outputs), package="MITUS"))
  Treatment_outputs<-cbind(1993:2014,Treatment_outputs[[1]],Treatment_outputs[[2]])
  Treatment_outputs<-as.data.frame(Treatment_outputs)
  colnames(Treatment_outputs)<-c("year","% discontinued tx", "% died on tx")


  output_list<-list(
    pop_output,
    pop_age_output,
    mortality_output,
    TB_cases_output,
    TB_cases_10yr_output,
    TB_cases_age_output,
    TB_cases_age_time_output,
    NUS_cases_output,
    NUS_recent_cases_output,
    US_LTBI_output,
    NUS_LTBI_output,
    TB_deaths_output,
    TB_deaths_age_output,
    Treatment_outputs
  )
  return(output_list)
}
