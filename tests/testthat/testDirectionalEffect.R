# "Tests for the directional effect functions"
seed_test <- 1001
nNullSamples <- 1000
alpha <- .05
nSubj = 20
nTrials = 30
chance <- 50 # percent accuracy (assuming two levels of the indepdent variable)

test_that("TestDirectionalEffect.GetDirectionalEffect - Positive Effect", {
  # test get directional effect with a true positive effect
  posEffectData <- create_sample_data(-1,.1, wSEsd = 1, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_pe <- get_directional_effect(posEffectData, idv = "id", dv = 'var', iv = 'condition')

  res_standatd <- posEffectData %>% dplyr::group_by(id,condition) %>% dplyr::summarise(mRT = mean(var)) %>% dplyr::summarise(effect = -diff(mRT))

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$effect_per_id$score, nSubj)
  diff.t <- t.test(res_pe$effect_per_id$score,)
  testthat::expect_lt(diff.t$p.value, alpha)
  testthat::expect_equal(res_pe$statistic, mean(res_standatd$effect))
})

test_that("TestDirectionalEffect.GetDirectionalEffect - Strong null", {
  # test get directional effect with a strong null effect
  snEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_sn <- get_directional_effect(snEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$effect_per_id$score, nSubj)
  diff.t <- t.test(res_sn$effect_per_id$score,)
  testthat::expect_lt(alpha, diff.t$p.value)
})


test_that("TestDirectionalEffect.GetDirectionalEffect - Weak null", {
  # test get directional effect with a weak null effect
  weakNullEffectData <- create_sample_data(0,2, wSEsd = .1, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_wn <- get_directional_effect(weakNullEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_type(res_wn$statistic, "double")
  testthat::expect_length(res_wn$effect_per_id$score, nSubj)
  diff.t <- t.test(res_wn$effect_per_id$score,)
  testthat::expect_lt(alpha, diff.t$p.value)
})


test_that("TestDirectionalEffect.TestDirectionalEffect - Positive Effect", {
  # test 'test directional effect' with a true positive effect
  posEffectData <- create_sample_data(-1,.1, wSEsd = 1, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_pe <- test_directional_effect(posEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples =  nNullSamples)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$null_dist, nNullSamples)
  testthat::expect_lt(res_pe$p, alpha)
})

test_that("TestDirectionalEffect.TestDirectionalEffect - Strong Null", {
  # test 'test directional effect' with a true positive effect
  snEffectData <- create_sample_data(0,0, wSEsd = 1, N = nSubj * 2, trialsPerCnd = nTrials, seed = seed_test)
  res_sn <- test_directional_effect(snEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_sn$p)
})


test_that("TestDirectionalEffect.TestDirectionalEffect - Weak Null", {
  # test get directional effect with a weak null effect
  weakNullEffectData <- create_sample_data(0,2, wSEsd = .1, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_wn <- test_directional_effect(weakNullEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_wn$statistic, "double")
  testthat::expect_length(res_wn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_wn$p)
})
