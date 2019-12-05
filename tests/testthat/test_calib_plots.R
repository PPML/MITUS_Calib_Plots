library(testthat)
library(here)
library(dplyr)

# check calib_plots function
context("checking calib_plots(location) for non-empty results")

# locations is a vector of c('US', 'CA', 'FL', 'GA', ..., 'WA') that has the top 11
# states by incidence, which are the states which are currently calibrated and
# intended for public access from MITUS (production2 branch) in Tabby2.

# code to update locations: 
# 
# devtools::load_all()
# locations <- 
#   c("US", "CA", "FL", "GA", "IL", "MA", "NJ", "NY", "PA", "TX",
#   "VA", "WA")
# readr::write_lines(x=locations, path='inst/test_locations.txt')
# 
locations <- readr::read_lines(system.file('test_locations.txt', package='MITUSCalibPlots'))

# test that in every location from the locations vector that the results of
# calib_plots render properly. 
# 
# this includes testing for: the output is a tibble, has non-empty rows, 6
# columns as specified in this test, that the plots column is filled with
# ggplot objects, and that the plots required for tabby2 are available.
#

for (location in locations) {
  df <- calib_plots(location)

  test_that(paste0('test that calib_plots returns a properly formatted tibble for ', location), {

    # check that this is a tibble
    expect_true(all(c('tbl_df', 'tbl', 'data.frame') %in% class(df)))

    # check that the tibble has at least 1 row 
    expect_true(nrow(df) > 0)

    ### specification for the column structure

    # 6 columns
    expect_true(ncol(df) == 6)

    # columns are: category, shortname, name, plot, target data, model data
    # TODO: add the final names for target data and model data
    expect_equal(colnames(df)[1:4], c('category', 'shortname', 'name', 'plot'))

    # column types are: factor, character, character, list, list, list
    # reasoning: 
    #   - category is a factor since it is highly repetitive
    #   - shortname and name are characters since they should not repeat
    #   - plot is a list so that ggplot2 objects can be inserted into this
    #       column (the typeof a ggplot2 object is list)
    #   - target data and model data are lists so we can insert tibbles into
    #       them (tibble being built on top of data.frame, being build on top of
    #       lists.)
    expect_identical(unname(sapply(df, class)), c('factor', 'character', 'character', 'list', 'list', 'list'))
  })

  # compute the unique shortnames for checking required outcomes 
  unique_plots <- unique(df$shortname)

  # Test that the df$plots which are returned from calib_plots have the ggplot class
  for (plt in unique_plots) {
    test_that(paste0("test that ", plt, " in ", location, " is a ggplot"), {
      expect_true(
        'ggplot' %in% (df %>% filter(shortname == plt) %>% `[[`(1, 'plot') %>% class)
      )
    })
  }

  # test that the comparison to recent data plots intended to be available in tabby2 
  # are available in calib_plots when plots_subset is not used.
  test_that(paste0("test that the plots required for tabby2 are available in calib_plots(", location, ")"), {
    # expect that calib_plots(location) includes the plots with the following shortnames:
    required_outcomes_for_tabby2 <- 
      c('Total_Population', 'TB_Cases_Time', 'TB_Cases_Age', 'ltbi_prev_US',
        'ltbi_prev_NUS', 'TB_Deaths_Year')

    # test that tabby2 outcomes are available for every location 
    expect_true(all(required_outcomes_for_tabby2 %in% unique_plots))
  })
}


context("testing calib_plots with its optional plots_subset argument")

# test that the optional parameter to calib_plots works as intended. 
# 
# this includes testing that the outputs of calib_plots works for each 
# location when used with a random subset of the possible plots$shortname
# options. 

df <- calib_plots('US') # pick an example here to get unique plots from
unique_plots <- unique(df$shortname) # compute unique shortnames 
n_unique_plots <- length(unique_plots)

for (location in locations) {

  # sample a positive random integer number of the possible plots
  plots_subset <- unique_plots[sample.int(n_unique_plots, sample.int(n_unique_plots, 1))]

  # use plot_subset arg
  df <- calib_plots(location, plots_subset = plots_subset)

  test_that(paste0('test that calib_plots with plots_subset arg returns a properly formatted tibble for ', location), {

    # check that this is a tibble
    expect_true(all(c('tbl_df', 'tbl', 'data.frame') %in% class(df)))

    # check that the tibble has at least 1 row 
    expect_true(nrow(df) > 0)

    ### specification for the column structure

    # 6 columns
    expect_true(ncol(df) == 6)

    # columns are: category, shortname, name, plot, target data, model data
    # TODO: add the final names for target data and model data
    expect_equal(colnames(df)[1:4], c('category', 'shortname', 'name', 'plot'))

    # column types are: factor, character, character, list, list, list
    # reasoning: 
    #   - category is a factor since it is highly repetitive
    #   - shortname and name are characters since they should not repeat
    #   - plot is a list so that ggplot2 objects can be inserted into this
    #       column (the typeof a ggplot2 object is list)
    #   - target data and model data are lists so we can insert tibbles into
    #       them (tibble being built on top of data.frame, being build on top of
    #       lists.)
    expect_identical(unname(sapply(df, class)), c('factor', 'character', 'character', 'list', 'list', 'list'))
  })
}

