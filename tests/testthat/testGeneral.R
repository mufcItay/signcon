# "Tests for the directional effect functions"
seed_test <- 1001
nNullSamples <- 1000
alpha <- .05
nSubj = 20
nTrials = 30
chance <- 50 # percent accuracy (assuming two levels of the independent variable)
# a helper function to apply to results of different analyses
null_rejected <- function(res, alpha) {res$p < alpha}

test_that("TestGeneral.Test - Positive Effect", {
  # test 'test' functions for each directional / no directional test function exposed in the package
  posEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  res_pe_nd_sign_consistency <- test_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe_nd_condition_classification <- test_condition_classification(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe_d_directional_effect <- test_directional_effect(posEffectData, idv = "id", dv = 'var', iv = 'condition')

  results <- list(res_pe_nd_sign_consistency, res_pe_nd_condition_classification, res_pe_d_directional_effect)
  decisions <- all(sapply(results, null_rejected, alpha))
  testthat::expect_equal(decisions, TRUE)
})

test_that("TestGeneral.Test - Strong Null Effect", {
  # test 'test' functions for each directional / no directional test function exposed in the package
  snEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  res_sn_nd_sign_consistency <- test_sign_consistency(snEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_sn_nd_condition_classification <- test_condition_classification(snEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_sn_d_directional_effect <- test_directional_effect(snEffectData, idv = "id", dv = 'var', iv = 'condition')

  results <- list(res_sn_nd_sign_consistency, res_sn_nd_condition_classification, res_sn_d_directional_effect)
  decisions <- all(!sapply(results, null_rejected, alpha))
  testthat::expect_equal(decisions, TRUE)
})

test_that("TestGeneral.Test - Weak Null Effect", {
  # test 'test' functions for each directional / no directional test function exposed in the package
  wnEffectData <- create_sample_data(0,2, wSEsd = .1, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  res_wn_nd_sign_consistency <- test_sign_consistency(wnEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_wn_nd_condition_classification <- test_condition_classification(wnEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_wn_d_directional_effect <- test_directional_effect(wnEffectData, idv = "id", dv = 'var', iv = 'condition')

  results <- list(res_wn_nd_sign_consistency, res_wn_nd_condition_classification, res_wn_d_directional_effect)
  decisions <- sapply(results, null_rejected, alpha)
  testthat::expect_equal(decisions[1], TRUE)
  testthat::expect_equal(decisions[2], TRUE)
  testthat::expect_equal(decisions[3], FALSE)
})
