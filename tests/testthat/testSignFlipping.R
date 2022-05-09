# "Tests for the sign consistency functions"
seed_test <- 1001
nNullSamples <- 1000
alpha <- .05
nSubj = 20
nTrials = 30

test_that("TestSignFlipping.GetSignConsistency - Positive Effect", {
  # test get sign consistency with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id, nSubj)
  testthat::expect_lt( .5, res_pe$statistic)
})

test_that("TestSignFlipping.TestSignConsistency - Weak Null", {
  # test for significant results when we expect a weak null
  weakNullData <- create_sample_data(0,2, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_wn <- test_sign_consistency(weakNullData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type( res_wn$p, "double")
  testthat::expect_length(res_wn$null_dist, nNullSamples)
  testthat::expect_lt( res_wn$p,alpha)
})

test_that("TestSignFlipping.TestSignConsistency - Strong Null", {
  # test for false positives high noise
  strongNullData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_sn <- test_sign_consistency(strongNullData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_sn$p, "double")
  testthat::expect_length(res_sn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_sn$p)
})

test_that("TestSignFlipping.TestSignConsistency - Positive Effect - Small IDs", {
  # test for positive effect with small individual differences
  posEffectData_smallIds <- create_sample_data(1,.1, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_pe_smallIds <- test_sign_consistency(posEffectData_smallIds, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_pe_smallIds$p, "double")
  testthat::expect_length(res_pe_smallIds$null_dist, nNullSamples)
  testthat::expect_lt(res_pe_smallIds$p, alpha)
})

test_that("TestSignFlipping.TestSignConsistency - Positive Effect - Small IDs", {
  # test for positive effect with high individual differences
  posEffectData_highIds <- create_sample_data(1,1.5, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_pe_highIds <- test_sign_consistency(posEffectData_highIds, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_pe_highIds$p, "double")
  testthat::expect_length(res_pe_highIds$null_dist, nNullSamples)
  testthat::expect_lt(res_pe_highIds$p, alpha)
})
