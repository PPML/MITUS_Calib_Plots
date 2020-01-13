#' Check that Data Exist in MITUS for Each Possible Plot
#'
#' @inheritParams calib_plots
#' @param possible_plots A vector of possible plot shortnames
#' @importFrom dplyr pull
#' @return A logical vector indicating if data exist for each of possible_plots
#' @export
data_check<-function(loc, possible_plots){
  loc1<-loc
  if (loc1 != "US") {loc2<-"ST"} else {loc2<-loc1}
  # plot_names<-as.vector(possible_plots[,2])
  available_plots<-rep(NA,14)
  names(available_plots)<-dplyr::pull(possible_plots,shortname)
  #LTBI Prevalence in US Born Individuals by Age
  if(
     length(list.files(pattern="prev_USB_11_IGRA",system.file(paste0(loc2,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="_USB_LTBI_pct",system.file(paste0(loc1,"/calibration_outputs/"),package = "MITUS")))==1 ){
    available_plots["ltbi_prev_US"]<-TRUE
  } else {
    available_plots["ltbi_prev_US"]<-FALSE
  }
  #LTBI Prevalence in Non-US-Born Individuals by Age
  if(
     length(list.files(pattern="prev_NUSB_11_IGRA",system.file(paste0(loc2,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="NUSB_LTBI_pct",system.file(paste0(loc1,"/calibration_outputs/"),package = "MITUS")))==1){
    available_plots["ltbi_prev_NUS"]<-TRUE
    } else {    available_plots["ltbi_prev_NUS"]<-FALSE
  }
  #Percent of TB Cases in Non-US-Born Individuals
  if(
     length(list.files(pattern=paste0(loc, "_fb_cases"),system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="TBcases",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1 ){
    available_plots["TB_NUS_percent"]<-TRUE
  } else{
    available_plots["TB_NUS_percent"]<-FALSE
  }
  #Percent of non-US born TB cases from recent immigrants (<2 yrs)
  if(
     length(list.files(pattern="fb_recent_cases",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="percentRecentFBcases",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1 ){
    available_plots["TB_recent_NUS_percent"]<-TRUE
    } else {    available_plots["TB_recent_NUS_percent"]<-FALSE
  }
  #Total Population by Age and Nativity
  if(
     length(list.files(pattern="tot_pop16_ag_fb",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="pop_ag_nat",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1){
    available_plots["Total_Population"]<-TRUE
  } else {
    available_plots["Total_Population"]<-FALSE
  }
  #TB Cases by Age
  if(
     length(list.files(pattern="age_cases_tot",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="age_cases_tot",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1){
    available_plots["TB_Cases_Age"]<-TRUE
    } else {    available_plots["TB_Cases_Age"]<-FALSE
  }
  #TB Cases by Age over Time
  if(
     length(list.files(pattern="age_cases_tot",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="age_cases_4grp",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1){
    available_plots["TB_Cases_Age_Time"]<-TRUE
  } else{
    available_plots["TB_Cases_Age_Time"]<-FALSE
  }
  #TB Deaths by Year
  if(
     length(list.files(pattern="tb_deaths",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="TBdeaths_",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1){
    available_plots["TB_Deaths_Year"]<-TRUE
    } else {    available_plots["TB_Deaths_Year"]<-FALSE
  }
  #TB Deaths by Age Over Time
  if(
     length(list.files(pattern="tb_deaths",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="TBdeathsAge",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1){
    available_plots["TB_Deaths_Age_Time"]<-TRUE
    } else {    available_plots["TB_Deaths_Age_Time"]<-FALSE
  }
  #TB Cases Last 10 Years and TB Cases Nativity Over Time
  if(
     length(list.files(pattern="cases_yr",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern=paste0(loc, "_fb_cases"),system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="TBcases",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1){
    available_plots["TB_Cases_Time"]<-TRUE; available_plots["TB_Cases_Recent_Time"]<-TRUE
    } else{    available_plots["TB_Cases_Time"]<-FALSE; available_plots["TB_Cases_Recent_Time"]<-FALSE
  }
  #Population by Nativity Over Time
  if(
     length(list.files(pattern="tot_pop_yr_fb",system.file(paste0(loc,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="pop_yr_nat",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1) {
    available_plots["Pop_Nat_Time"]<-TRUE
  } else{
    available_plots["Pop_Nat_Time"]<-FALSE
  }
  #Total Deaths over Time
  if(
     length(list.files(pattern="tot_mort",system.file(paste0(loc,"/cali(bration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="mort_yr_nat",system.file(paste0(loc,"/calibration_outputs/"),package = "MITUS")))==1){
    available_plots["Deaths_Time"]<-TRUE
    } else{    available_plots["Deaths_Time"]<-FALSE
  }
  #Treatment Outcomes
  if(
     length(list.files(pattern="tx_outcomes",system.file(paste0(loc2,"/calibration_targets/"),package = "MITUS")))==1 &
       length(list.files(pattern="txOutcomes",system.file(paste0(loc1,"/calibration_outputs/"),package = "MITUS")))==1){
    available_plots["Treatment_Outcomes"]<-TRUE
  } else{
    available_plots["Treatment_Outcomes"]<-FALSE
  }

  for (i in 1:length(available_plots)){
    if(available_plots[i]==FALSE){
      possible_plots<-possible_plots[-(which(possible_plots[,2]==names(available_plots)[i])),]
    }
  }
  return(possible_plots)
}

