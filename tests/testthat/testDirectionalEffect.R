test_that("TestDirectionalEffect.GetDirectionalEffect - Positive Effect", {
  # test get directional effect with a true positive effect
  posEffectData <- get_test_data(pe_ds_lbl)
  res_pe <- get_directional_effect(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_standatd <- posEffectData %>% dplyr::group_by(id,condition) %>% dplyr::summarise(mRT = mean(var)) %>% dplyr::summarise(effect = diff(mRT))

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$effect_per_id$score, nSubj)
  diff.t <- t.test(res_pe$effect_per_id$score,)
  testthat::expect_lt(diff.t$p.value, alpha)
  testthat::expect_equal(res_pe$statistic, mean(res_standatd$effect))
})

test_that("TestDirectionalEffect.GetDirectionalEffect - Strong null", {
  # test get directional effect with a strong null effect
  snEffectData <- get_test_data(sn_ds_lbl)
  res_sn <- get_directional_effect(snEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$effect_per_id$score, nSubj)
  diff.t <- t.test(res_sn$effect_per_id$score,)
  testthat::expect_lt(alpha, diff.t$p.value)
})


test_that("TestDirectionalEffect.GetDirectionalEffect - Weak null", {
  # test get directional effect with a weak null effect
  weakNullEffectData <- get_test_data(wn_ds_lbl)
  res_wn <- get_directional_effect(weakNullEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_type(res_wn$statistic, "double")
  testthat::expect_length(res_wn$effect_per_id$score, nSubj)
  diff.t <- t.test(res_wn$effect_per_id$score,)
  testthat::expect_lt(alpha, diff.t$p.value)
})


test_that("TestDirectionalEffect.TestDirectionalEffect - Positive Effect", {
  # test 'test directional effect' with a true positive effect
  posEffectData <- get_test_data(pe_ds_lbl)
  res_pe <- test_directional_effect(posEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples =  nNullSamples)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$null_dist, nNullSamples)
  testthat::expect_lt(res_pe$p, alpha)
})

test_that("TestDirectionalEffect.TestDirectionalEffect - Strong Null", {
  # test 'test directional effect' with a true positive effect
  snEffectData <- create_sample_data(0,0, wSEsd = 1, N = nSubj * 2, trials_per_cnd = nTrials, seed = seed_test)
  res_sn <- test_directional_effect(snEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_sn$p)
})


test_that("TestDirectionalEffect.TestDirectionalEffect - Weak Null", {
  # test get directional effect with a weak null effect
  weakNullEffectData <- get_test_data(wn_ds_lbl)
  res_wn <- test_directional_effect(weakNullEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_wn$statistic, "double")
  testthat::expect_length(res_wn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_wn$p)
})


####################################################
################ MULTIVARIATE TESTS ################
####################################################

test_that("TestDirectionalEffect.GetDirectionalEffect - Multivariate, Strong Null", {
  # test get directional effect with a strong null effect, and 3 dependent variables
  median_func_mv <- function(mat) {stats::median(c(mat[,'var'], mat[,'var2'], mat[,'var3']))}
  snEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  snEffectData$var2 <- otherSnEffectData$var
  snEffectData$var3 <- anotherSnEffectData$var
  res_sn <- get_directional_effect(snEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function = median_func_mv)

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$effect_per_id$score, nSubj)
  diff.t <- t.test(res_sn$effect_per_id$score)
  testthat::expect_lt(alpha, diff.t$p.value)
})

test_that("TestDirectionalEffect.TestDirectionalEffect - Multivariate, Strong Null", {
  # test 'test directional effect' with a strong null effect
  median_func_mv <- function(mat) {stats::median(c(mat[,'var'], mat[,'var2'], mat[,'var3']))}
  snEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  snEffectData$var2 <- otherSnEffectData$var
  snEffectData$var3 <- anotherSnEffectData$var
  res_sn <- test_directional_effect(snEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function = median_func_mv, null_dist_samples = nNullSamples)

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_sn$p)
})


test_that("TestDirectionalEffect.GetDirectionalEffect - Multivariate, Positive Effect, 2nd null", {
  # test get directional effect with a positive effect for one dependent variable, and a 2nd uninformative dependent variable.
  posEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  posEffectData$var2 <- rnorm(nrow(posEffectData), 0, 2)
  res_pe_one_var <- get_directional_effect(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_directional_effect(posEffectData, idv = "id", dv = c('var','var2'), iv = 'condition')

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$effect_per_id$score, nSubj)
  diff.t <- t.test(res_pe$effect_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
  diff.t <- t.test(res_pe$effect_per_id$score, res_pe_one_var$effect_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
  testthat::expect_lt(diff.t$statistic, 0)
})


test_that("TestDirectionalEffect.TestDirectionalEffect - Multivariate, Positive Effect", {
  # test 'test directional effect' with a positive effect, and 3 informative variables
  posEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(2,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  posEffectData$var2 <- otherPosEffectData$var
  res_pe_one_var <- test_directional_effect(posEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)
  res_pe <- test_directional_effect(posEffectData, idv = "id", dv = c('var','var2'), iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$null_dist, nNullSamples)
  testthat::expect_lt(res_pe$p, alpha)
  testthat::expect_lt(res_pe_one_var$statistic, res_pe$statistic)
})

test_that("TestDirectionalEffect.GetDirectionalEffect - Multivariate, Positive Effect, random summary function", {
  rnd_func_mv <- function(mat) {rnorm(1,0,1)}
  # test get directional effect with a a random summary function, expecting null results
  posEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj , trials_per_cnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherPosEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  posEffectData$var2 <- otherPosEffectData$var
  posEffectData$var3 <- anotherPosEffectData$var
  res_pe_one_var_mean <- get_directional_effect(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_directional_effect(posEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function =  rnd_func_mv)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$effect_per_id$score, nSubj)
  diff.t <- t.test(res_pe_one_var_mean$effect_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
  diff.t <- t.test(res_pe$effect_per_id$score)
  testthat::expect_lt(alpha, diff.t$p.value)
  diff.t <- t.test(res_pe$effect_per_id$score, res_pe_one_var_mean$effect_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
  testthat::expect_lt(diff.t$statistic, 0)
})
