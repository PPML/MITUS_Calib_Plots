library(testthat)
library(here)

# check calib_plots function
context("checking calib_plots(location) for non-empty results")

# locations is a vector of c('US', 'CA', 'FL', 'GA', ..., 'WA') that has the top 11
# states by incidence, which are the states which are currently calibrated and
# intended for public access from MITUS (production2 branch) in Tabby2.

locations <- readRDS(here('tests/testthat/top11_and_us_list.rds'))

for (location in locations) {
  test_that(paste0('calib_plots returns a properly formatted tibble for ', location), {
    plots <- calib_plots(location)

    # check that this is a tibble
    expect_true(all(c('tbl_df', 'tbl', 'data.frame') %in% class(plots)))

    # check that the tibble has at least 1 row 
    expect_true(nrow(plots) > 0)

    ### specification for the column structure

    # 6 columns
    expect_true(ncol(plots) == 6)

    # columns are: category, shortname, name, plot, target data, model data
    # TODO: add the final names for target data and model data
    expect_equal(colnames(plots)[1:4], c('category', 'shortname', 'name', 'plot'))

    # column types are: factor, character, character, list, list, list
    # reasoning: 
    #   - category is a factor since it is highly repetitive
    #   - shortname and name are characters since they should not repeat
    #   - plot is a list so that ggplot2 objects can be inserted into this
    #       column (the typeof a ggplot2 object is list)
    #   - target data and model data are lists so we can insert tibbles into
    #       them (tibble being built on top of data.frame, being build on top of
    #       lists.)
    expect_identical(unname(sapply(plots, class)), c('factor', 'character', 'character', 'list', 'list', 'list'))
  })
}
