# "Tests for the sign consistency functions"
seed_test <- 1001
nNullSamples <- 1000
alpha <- .05
nSubj = 20
nTrials = 30
chance <- 50 # in percents

test_that("TestSignFlipping.GetSignConsistency - Multivariate, Strong Null", {
  # test get condition classification with a true positive effect
  summ_func_mv <- function(df) {stats::median(c(df$var, df$var2, df$var3))}
  snEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  otherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test + 1)
  anotherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test + 2)
  snEffectData$var2 <- otherSnEffectData$var
  snEffectData$var3 <- anotherSnEffectData$var
  res_sn <- get_sign_consistency(snEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function = summ_func_mv)

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$consistency_per_id$score, nSubj)
  diff.t <- t.test(res_sn$consistency_per_id$score - chance/100)
  testthat::expect_lt(alpha, diff.t$p.value)
})

test_that("TestSignFlipping.TestSignConsistency - Multivariate, Strong Null", {
  # test get condition classification with a true positive effect
  summ_func_mv <- function(df) {stats::median(c(df$var, df$var2, df$var3))}
  snEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  otherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test + 1)
  anotherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test + 2)
  snEffectData$var2 <- otherSnEffectData$var
  snEffectData$var3 <- anotherSnEffectData$var
  res_sn <- test_sign_consistency(snEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function = summ_func_mv)

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$consistency_per_id$score, nSubj)
  testthat::expect_lt(alpha, res_sn$p)
  qChance <= quantile(chance,res_sn$null_dist)
  testthat::expect_lt(alpha, qChance)
})

test_that("TestSignFlipping.GetSignConsistency - Multivariate, Positive Effect, 2nd null", {
  # test get condition classification with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  posEffectData$var2 <- rep(c(1,2), nrow(posEffectData)/2)
  res_pe_one_var <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = c('var','var2'), iv = 'condition')

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id$score, nSubj)
  diff.t <- t.test(res_pe$consistency_per_id$score - chance/100)
  testthat::expect_lt(diff.t$p.value, alpha)
  diff.t <- t.test(res_pe$consistency_per_id$score, res_pe_one_var$consistency_per_id$score)
  testthat::expect_lt(alpha, diff.t$p.value)
})


test_that("TestSignFlipping.GetSignConsistency - Multivariate, Positive Effect", {
  summ_func_mv <- function(df) {stats::median(c(df$var, df$var2, df$var3))}
  # test get condition classification with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trialsPerCnd = nTrials, seed = seed_test + 1)
  anotherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trialsPerCnd = nTrials, seed = seed_test + 2)
  posEffectData$var2 <- otherPosEffectData$var
  posEffectData$var3 <- anotherPosEffectData$var
  res_pe_one_var <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function =  summ_func_mv)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id$score, nSubj)
  testthat::expect_lt(res_pe_one_var$statistic, res_pe$statistic)
  diff.t <- t.test(res_pe$consistency_per_id$score - chance/100)
  testthat::expect_lt(diff.t$p.value, alpha)
  diff.t <- t.test(res_pe$consistency_per_id$score, res_pe_one_var$consistency_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
})


test_that("TestSignFlipping.GetSignConsistency - Multivariate, Positive Effect, Sum", {
  summ_func_mv <- function(df) {base::sum(c(df$var, df$var2, df$var3))}
  # test get condition classification with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj , trialsPerCnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trialsPerCnd = nTrials, seed = seed_test + 1)
  anotherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trialsPerCnd = nTrials, seed = seed_test + 2)
  posEffectData$var2 <- otherPosEffectData$var
  posEffectData$var3 <- anotherPosEffectData$var
  res_pe_one_var <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function =  summ_func_mv)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id$score, nSubj)
  testthat::expect_lt(res_pe_one_var$statistic, res_pe$statistic)
  diff.t <- t.test(res_pe$consistency_per_id$score - chance/100)
  testthat::expect_lt(diff.t$p.value, alpha)
  diff.t <- t.test(res_pe$consistency_per_id$score, res_pe_one_var$consistency_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
})

test_that("TestSignFlipping.GetSignConsistency - Positive Effect", {
  # test get sign consistency with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = 2, N = nSubj, trialsPerCnd = nTrials, seed = seed_test)
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id$score, nSubj)
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
