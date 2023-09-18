
test_that("TestGeneral.Test - Positive Effect", {
  # test 'test' functions for each directional / no directional test function exposed in the package
  posEffectData <- get_test_data(pe_ds_lbl)
  res_pe_nd_sign_consistency <- test_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe_d_directional_effect <- test_directional_effect(posEffectData, idv = "id", dv = 'var', iv = 'condition')

  results <- list(res_pe_nd_sign_consistency, res_pe_d_directional_effect)
  decisions <- all(sapply(results, null_rejected, alpha))
  testthat::expect_equal(decisions, TRUE)
})

test_that("TestGeneral.Test - Strong Null Effect", {
  # test 'test' functions for each directional / no directional test function exposed in the package
  snEffectData <- get_test_data(sn_ds_lbl)
  res_sn_nd_sign_consistency <- test_sign_consistency(snEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_sn_d_directional_effect <- test_directional_effect(snEffectData, idv = "id", dv = 'var', iv = 'condition')

  results <- list(res_sn_nd_sign_consistency, res_sn_d_directional_effect)
  decisions <- all(!sapply(results, null_rejected, alpha))
  testthat::expect_equal(decisions, TRUE)
})

test_that("TestGeneral.Test - Weak Null Effect", {
  # test 'test' functions for each directional / no directional test function exposed in the package
  wnEffectData <- get_test_data(wn_ds_lbl)
  res_wn_nd_sign_consistency <- test_sign_consistency(wnEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_wn_d_directional_effect <- test_directional_effect(wnEffectData, idv = "id", dv = 'var', iv = 'condition')

  results <- list(res_wn_nd_sign_consistency, res_wn_d_directional_effect)
  decisions <- sapply(results, null_rejected, alpha)
  testthat::expect_equal(decisions[1], TRUE)
  testthat::expect_equal(decisions[2], FALSE)
})

test_that("TestGeneral.Test - P Value Correction (p=0)", {
  # test 'test' functions for each directional / no directional test function exposed in the package
  posEffectData <- create_sample_data(2,0, 0, N = 4,
                                      trials_per_cnd = 5, seed = seed_test)

  res_pe_sc <-test_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_lt(res_pe_sc$p, .05)
  testthat::expect_lt(0, res_pe_sc$p)
})

test_that("TestGeneral.Test - P Value Correction (sanity)", {
  mock_statistic <- 1
  mock_null_dist <- c(0,0.5,2)
  uncorrected_p <- sum(mock_statistic <= mock_null_dist) / length(mock_null_dist)
  corrected_p <- get_corrected_pval(mock_statistic, mock_null_dist)

  testthat::expect_lt(uncorrected_p, corrected_p)

  B <- sum(mock_statistic <= mock_null_dist, na.rm = TRUE)
  M <- sum(!is.na(mock_null_dist))
  testthat::expect_equal(corrected_p, (B+1)/(M+1) )
})
