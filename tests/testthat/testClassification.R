"Tests for the condition classification functions"
seed_test <- 1001
nNullSamples <- 1000
alpha <- .05
nSubj = 20
nTrials = 30


test_that("TestClassification.GetConditionClassification - Positive Effect", {
  # test get condition classification with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = .5, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_pe <- get_condition_classification(posEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$accuracy_per_id, nSubj)
  testthat::expect_lt( .5, res_pe$statistic)
})

test_that("TestClassification.TestConditionClassification - Positive Effect", {
  # test for significant condition classification with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = .5, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_pe <- test_condition_classification(posEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = 1000)

  testthat::expect_type(res_pe$p, "double")
  testthat::expect_length(res_pe$null_dist, nNullSamples)
  testthat::expect_lt(res_pe$p, alpha)
})

test_that("TestClassification.TestConditionClassification - Strong Null", {
  # test non significant results from test  condition classification with a strong null
  strongNullEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_sn <- test_condition_classification(strongNullEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = 1000)

  testthat::expect_type(res_sn$p, "double")
  testthat::expect_length(res_sn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_sn$p)
})

test_that("TestClassification.TestConditionClassification - Weak Null", {
  # test significant results from test  condition classification with a weak null
  weakNullEffectData <- create_sample_data(0,2, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_wn <- test_condition_classification(weakNullEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = 1000)

  testthat::expect_type(res_wn$p, "double")
  testthat::expect_length(res_wn$null_dist, nNullSamples)
  testthat::expect_lt(res_wn$p, alpha)
})


