make_plots<-function(loc,short_name){
# if (loc == "US"){
  plot<-  switch(short_name,

                 "Total_Population" = calib_plt_pop_by_age_nat(loc),

                 # "Pop_Nat_Time"= calib_plt_pop_by_nat_over_time(loc),

                 # "Deaths_Time"= calib_plt_deaths_over_time(loc),

                 # "TB_Cases_Time"=calib_plt_tb_cases_nat_over_time(loc),

                 "TB_Cases_Recent_Time"=calib_plt_tb_cases_identified_over_ten_years(loc),

                 "TB_Cases_Nativity"=calib_plt_tb_cases_nat_dist(loc),

                 "TB_Cases_Age"=calib_plt_tb_cases_age_dist(loc),

                 # "TB_Cases_Age_Time"=calib_plt_tb_cases_age_over_time(loc),

                 # "TB_NUS_percent"=calib_plt_pct_cases_nusb(loc),

                 # "TB_recent_NUS_percent"=calib_plt_pct_cases_nusb_recent(loc),

                 "ltbi_prev_US"=calib_plt_us_ltbi_by_age(loc),

                 "ltbi_prev_NUS"=calib_plt_nus_ltbi_by_age(loc),

                 "TB_Deaths_Year"=calib_plt_tb_deaths_by_year(loc)

                 # "TB_Deaths_Age_Time"=calib_plt_tb_deaths_by_age_over_time(loc),

                 # "Treatment_Outcomes"=calib_plt_trt_outcomes(loc)
  )
# } else {
#   plot<-  switch(short_name,
#
#            "Total_Population" = calib_plt_pop_by_age_nat(loc),
#
#            # "Pop_Nat_Time"= calib_plt_pop_by_nat_over_time(loc),
#
#            # "Deaths_Time"= calib_plt_deaths_over_time(loc),
#
#            # "TB_Cases_Time"=calib_plt_tb_cases_nat_over_time(loc),
#
#            "TB_Cases_Recent_Time"=calib_plt_tb_cases_identified_over_ten_years(loc),
#
#            "TB_Cases_Nativity"=calib_plt_tb_cases_5yr_nat(loc),
#
#            "TB_Cases_Age"=calib_plt_tb_cases_age_dist(loc),
#
#            # "TB_Cases_Age_Time"=calib_plt_tb_cases_age_over_time(loc),
#
#            # "TB_NUS_percent"=calib_plt_pct_cases_nusb(loc),
#
#            # "TB_recent_NUS_percent"=calib_plt_pct_cases_nusb_recent(loc),
#
#            "ltbi_prev_US"=calib_plt_us_ltbi_by_age(loc),
#
#            "ltbi_prev_NUS"=calib_plt_nus_ltbi_by_age(loc),
#
#            "TB_Deaths_Year"=calib_plt_tb_deaths_by_year(loc)
#
#            # "TB_Deaths_Age_Time"=calib_plt_tb_deaths_by_age_over_time(loc),
#
#            # "Treatment_Outcomes"=calib_plt_trt_outcomes(loc)
#     )
# }
  return(plot)
  }

