#' @inheritParams calib_plots
target_data_list<-function(loc){
  loc1<-loc
  if (loc1 != "US") {loc2<-"ST"} else {loc2<-loc1}

  ## "Population: Total, US, and Non-US Born",
  pop_target<-list.files(pattern="tot_pop_yr_fb",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  pop_target<-readRDS(system.file(paste0(loc, "/calibration_targets/", pop_target), package="MITUS"))
  ## "Population by Age for Non-US Born and US Born",
  pop_age_target<-list.files(pattern="tot_pop16_ag_fb",system.file(paste0(loc1,"/calibration_targets/"),package = "MITUS"))
  pop_age_target<-readRDS(system.file(paste0(loc1, "/calibration_targets/", pop_age_target), package="MITUS"))

  ## "Mortality: Total, US, and Non-US Born",
  mortality_target<-list.files(pattern="tot_mort",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  mortality_target<-readRDS(system.file(paste0(loc1, "/calibration_targets/", mortality_target), package="MITUS"))
##  "Total TB Cases Identified",
  fb_TB_cases_target<-list.files(pattern=paste0(loc, "_fb_cases"),system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  fb_TB_cases_target<-readRDS(system.file(paste0(loc, "/calibration_targets/", fb_TB_cases_target), package="MITUS"))
  if (loc=="US"){
  TB_cases_target<-list.files(pattern=paste0(loc, "_cases_yr"),system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  TB_cases_target<-readRDS(system.file(paste0(loc, "/calibration_targets/", TB_cases_target), package="MITUS"))[41:67,]
  } else {
    TB_cases_target<-list.files(pattern=paste0(loc, "_cases_yr"),system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
    TB_cases_target<-readRDS(system.file(paste0(loc, "/calibration_targets/", TB_cases_target), package="MITUS"))
  }
  TB_cases_target<-cbind(TB_cases_target[,1], ((1-fb_TB_cases_target[,2])*TB_cases_target[,2]*1e6),(fb_TB_cases_target[,2]*TB_cases_target[,2]*1e6))
  colnames(TB_cases_target)<-c("Year", "USB Cases", "NUSB Cases")

  ##  "Total TB Cases Identified in Recent Years",
  TB_cases_10yr_target<-TB_cases_target[(nrow(TB_cases_target)-9):nrow(TB_cases_target),]

  ##  "Total TB Cases by Nativity",
  fn<-list.files(pattern="ag_nat_cases_5yr",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  #subset into 20010-2019
  target_df0 <-readRDS(system.file(paste0(loc,"/calibration_targets/",fn),package="MITUS"))

  target_df0<-rbind(target_df0[((nrow(target_df0)/2)-1):(nrow(target_df0)/2),], #last 10 years
                      target_df0[(nrow(target_df0)-1):nrow(target_df0),] )#last 10 years

  target_df <-cbind(rowSums(target_df0[1,5:14]), rowSums(target_df0[3,5:14]),
                    rowSums(target_df0[2,5:14]), rowSums(target_df0[4,5:14]))
  #set the years
  # years<-target_df0[,1]
  years <-2010:2019
  #update the column names for legend use
  label<-c("2010-2014 USB", "2010-2014 NUSB", "2015-2019 USB", "2015-2019 NUSB")
  TB_cases_nativity_target<-data.frame("label" = label,
                        "cases" = t(target_df))
  colnames(TB_cases_nativity_target) <-c("label", "cases")

  ##  "Total TB Cases by Age",
  TB_cases_age_target<-list.files(pattern="age_cases_tot",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  TB_cases_age_target<-readRDS(system.file(paste0(loc1, "/calibration_targets/", TB_cases_age_target), package="MITUS"))

  ##  "TB Cases by Age in Recent Years",
  TB_cases_age_time_target<-list.files(pattern="age_cases_tot",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  TB_cases_age_time_target<-readRDS(system.file(paste0(loc1, "/calibration_targets/", TB_cases_age_time_target), package="MITUS"))

  ##  "Percent of TB Cases in Non-US Born Population",
  NUS_cases_target<-list.files(pattern=paste0(loc, "_fb_cases"),system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  NUS_cases_target<-readRDS(system.file(paste0(loc1, "/calibration_targets/", NUS_cases_target), package="MITUS"))

  ##  "Percent of Non-US Born TB Cases Arrived in Past 2 Years",
  #CHECK THAT THIS IS THE RIGHT OUTPUT
  if (loc1 == "US"){
  NUS_recent_cases_target<-list.files(pattern="fb_recent_cases2",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  } else {
    NUS_recent_cases_target<-list.files(pattern="fb_recent_cases",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS"))
  }
  NUS_recent_cases_target<-readRDS(system.file(paste0(loc1, "/calibration_targets/",   NUS_recent_cases_target), package="MITUS"))

  ##  "LTBI in US Born Population by Age",
  US_LTBI_target<-list.files(pattern="prev_USB_11_IGRA",system.file(paste0(loc2,"/calibration_targets/"),package = "MITUS"))
  US_LTBI_target<-readRDS(system.file(paste0(loc2, "/calibration_targets/",   US_LTBI_target), package="MITUS"))

  ##  "LTBI in Non-US Born Population by Age",
  NUS_LTBI_target<-list.files(pattern="prev_NUSB_11_IGRA",system.file(paste0(loc2,"/calibration_targets/"),package = "MITUS"))
  NUS_LTBI_target<-readRDS(system.file(paste0(loc2, "/calibration_targets/",   NUS_LTBI_target), package="MITUS"))

  ##  "TB Deaths in Recent Years",
  TB_deaths_target<-list.files(pattern="tb_deaths",system.file(paste0(loc2,"/calibration_targets/"),package = "MITUS"))
  TB_deaths_target<-readRDS(system.file(paste0(loc2, "/calibration_targets/",   TB_deaths_target), package="MITUS"))

  ##  "TB Deaths by Age Group",
  TB_deaths_age_target<-list.files(pattern="tb_deaths",system.file(paste0(loc2,"/calibration_targets/"),package = "MITUS"))
  TB_deaths_age_target<-readRDS(system.file(paste0(loc2, "/calibration_targets/",   TB_deaths_age_target), package="MITUS"))

  ##  "TLTBI Treatment Outcomes"
  Treatment_targets<-list.files(pattern="tx_outcomes",system.file(paste0(loc2,"/calibration_targets/"),package = "MITUS"))
  Treatment_targets<-readRDS(system.file(paste0(loc2, "/calibration_targets/",Treatment_targets), package="MITUS"))

  target_list<-list(
    pop_target,
    pop_age_target,
    mortality_target,
    TB_cases_target,
    TB_cases_10yr_target,
    TB_cases_nativity_target,
    TB_cases_age_target,
    TB_cases_age_time_target,
    NUS_cases_target,
    NUS_recent_cases_target,
    US_LTBI_target,
    NUS_LTBI_target,
    TB_deaths_target,
    TB_deaths_age_target,
    Treatment_targets
  )
  return(target_list)
}
