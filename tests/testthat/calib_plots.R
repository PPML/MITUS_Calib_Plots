
library(testthat)

context("calib plots")

test_that("calib_plots('US') returns a tibble", {
    plots <- calib_plots('US')
    expect_true(all(c('tbl_df', 'tbl', 'data.frame') %in% class(plots)))
    expect_true(dim(plots)[[1]] > 0)
})

test_that('calib_plots returns a tibble for the top 11 states by incidence', {
    for (state in c('CA', 'FL', 'GA', 'IL', 'MA', 'NJ', 'NY', 'PA', 'TX', 'VA', 'WA')) {
      plots <- calib_plots(state)
      expect_true(all(c('tbl_df', 'tbl', 'data.frame') %in% class(plots)))
      expect_true(dim(plots)[[1]] > 0)
    }
})
