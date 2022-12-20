
test_that("TestErrors - NAs", {
  data <- get_test_data(sn_ds_lbl)
  odd <- seq(1,nrow(data), 2)
  data[odd, names(data)[1]] <- NA
  testthat::expect_error(get_directional_effect(data, dv = 'var'))
  testthat::expect_error(test_directional_effect(data, dv = 'var'))
  testthat::expect_error(get_sign_consistency(data, dv = 'var'))
  testthat::expect_error(test_sign_consistency(data, dv = 'var'))
  testthat::expect_error(get_condition_classification(data, dv = 'var'))
  testthat::expect_error(test_condition_classification(data, dv = 'var'))
})

test_that("TestErrors - NaNs", {
  data <- get_test_data(sn_ds_lbl)
  odd <- seq(1,nrow(data), 2)
  data[odd, names(data)[2]] <- NaN
  testthat::expect_error(get_directional_effect(data, dv = 'var'))
  testthat::expect_error(test_directional_effect(data, dv = 'var'))
  testthat::expect_error(get_sign_consistency(data, dv = 'var'))
  testthat::expect_error(test_sign_consistency(data, dv = 'var'))
  testthat::expect_error(get_condition_classification(data, dv = 'var'))
  testthat::expect_error(test_condition_classification(data, dv = 'var'))
})

test_that("TestErrors - missing column", {
  data <- get_test_data(sn_ds_lbl)
  data <- data[, names(data)[1:2]]
  testthat::expect_error(get_directional_effect(data, dv = 'var'))
  testthat::expect_error(test_directional_effect(data, dv = 'var'))
  testthat::expect_error(get_sign_consistency(data, dv = 'var'))
  testthat::expect_error(test_sign_consistency(data, dv = 'var'))
  testthat::expect_error(get_condition_classification(data, dv = 'var'))
  testthat::expect_error(test_condition_classification(data, dv = 'var'))
})

test_that("TestErrors - wrong column names", {
  data <- get_test_data(sn_ds_lbl)
  colnames(data)[3] <- colnames(data)[2]
  testthat::expect_error(get_directional_effect(data, dv = 'var'))
  testthat::expect_error(get_condition_classification(data, id = 'var12'))

  colnames(data)[1] <- 'fake'
  testthat::expect_error(get_directional_effect(data, dv = 'var'))
})


test_that("TestErrors - wrong column names - MULTIVARIATE", {
  data <- get_test_data(sn_ds_lbl)
  data$var2 <- data$var^2
  # expect no error (using only one dv)
  get_directional_effect(data, dv = 'var')
  # expect error (wrong dv2)
  testthat::expect_error(get_directional_effect(data, dv = c('var', 'rt')))

  # expect error (expecting dv2 but there is no such column)
  data <- data[, names(data)[1:length(names(data)) - 1]]
  testthat::expect_error(get_directional_effect(data, dv = c('var', 'dv2')))
})

