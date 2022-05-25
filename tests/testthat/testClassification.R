"Tests for the condition classification functions"
seed_test <- 1001
nNullSamples <- 1000
alpha <- .05
nSubj = 20
nTrials = 30
chance <- 50 # percent accuracy (assuming two levels of the independent variable)


test_that("TestClassification.GetConditionClassification - Positive Effect", {
  # test get condition classification with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = .5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  res_pe <- get_condition_classification(posEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$accuracy_per_id$score, nSubj)
  testthat::expect_lt( .5, res_pe$statistic)
})

test_that("TestClassification.TestConditionClassification - Positive Effect", {
  # test for significant condition classification with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = .5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  res_pe <- test_condition_classification(posEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = 1000)

  testthat::expect_type(res_pe$p, "double")
  testthat::expect_length(res_pe$null_dist, nNullSamples)
  testthat::expect_lt(res_pe$p, alpha)
})

test_that("TestClassification.TestConditionClassification - Strong Null", {
  # test non significant results from test  condition classification with a strong null
  strongNullEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  res_sn <- test_condition_classification(strongNullEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = 1000)

  testthat::expect_type(res_sn$p, "double")
  testthat::expect_length(res_sn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_sn$p)
})

test_that("TestClassification.TestConditionClassification - Weak Null", {
  # test significant results from test  condition classification with a weak null
  weakNullEffectData <- create_sample_data(0,2, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  res_wn <- test_condition_classification(weakNullEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = 1000)

  testthat::expect_type(res_wn$p, "double")
  testthat::expect_length(res_wn$null_dist, nNullSamples)
  testthat::expect_lt(res_wn$p, alpha)
})


test_that("TestClassification.TestConditionClassification - Imbalance - Effect", {
  # test significant results from test  condition classification with a weak null
  imbEffectData <- create_sample_data(1,.1, wSEsd = .5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  # create class imbalance
  imbEffectData <- imbEffectData |>
    dplyr::group_by('idv') |>
    dplyr::slice(ceiling(nTrials/2):dplyr::n())
  res_imbEff <- test_condition_classification(imbEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = 1000)

  testthat::expect_type(res_imbEff$p, "double")
  testthat::expect_length(res_imbEff$null_dist, nNullSamples)
  testthat::expect_lt(res_imbEff$p, alpha)
})


test_that("TestClassification.TestConditionClassification - Imbalance - Null Effect", {
  # test significant results from test  condition classification with a weak null
  imbNullEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  # create class imbalance
  imbNullEffectData <- imbNullEffectData |>
    dplyr::group_by('idv') |>
    dplyr::slice(ceiling(nTrials/2):dplyr::n())
  res_imbNullEff <- test_condition_classification(imbNullEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = 1000)

  testthat::expect_type(res_imbNullEff$p, "double")
  testthat::expect_length(res_imbNullEff$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_imbNullEff$p)
})

test_that("TestClassification.TestConditionClassification - Imbalance - Null Effect, No Adjustement", {
  # test significant results from test  condition classification with a weak null
  imbNullEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  # create class imbalance
  imbNullEffectNoAdjData <- imbNullEffectData |>
    dplyr::group_by('idv') |>
    dplyr::slice(ceiling(nTrials/2):dplyr::n())
  res_imbNullEffNoAdj <- test_condition_classification(imbNullEffectNoAdjData, idv = "id", dv = 'var', iv = 'condition',
                                                       null_dist_samples = 1000, handleImbalance = FALSE)
  res_imbNullEffAdj <- test_condition_classification(imbNullEffectNoAdjData, idv = "id", dv = 'var', iv = 'condition',
                                                       null_dist_samples = 1000)

  testthat::expect_type(res_imbNullEffAdj$p, "double")
  testthat::expect_length(res_imbNullEffAdj$null_dist, nNullSamples)
  testthat::expect_type(res_imbNullEffNoAdj$p, "double")
  testthat::expect_length(res_imbNullEffNoAdj$null_dist, nNullSamples)

  # we expect p_value results to be similar (no effect for both)
  testthat::expect_lt(alpha, res_imbNullEffAdj$p)
  testthat::expect_lt(alpha, res_imbNullEffNoAdj$p)

  # we expect the statstic to defer (adj < no adj)
  testthat::expect_lt(res_imbNullEffAdj$statistic, res_imbNullEffNoAdj$statistic)
})

####################################################
################ MULTIVARIATE TESTS ################
####################################################

test_that("TestClassification.GetConditionClassification - Multivariate, Strong Null", {
  # test get condition classification with a strong null effect, and 3 dependent variables
  snEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  snEffectData$var2 <- otherSnEffectData$var
  snEffectData$var3 <- anotherSnEffectData$var
  res_sn <- get_condition_classification(snEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition')

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$accuracy_per_id$score, nSubj)
  diff.t <- t.test(res_sn$accuracy_per_id$score - chance / 100)
  testthat::expect_lt(alpha, diff.t$p.value)
})

test_that("TestClassification.GetConditionClassification - Multivariate, Positive Effect, 2nd null", {
  # test get condition classification with a positive effect for one dependent variable, and a 2nd uninformative dependent variable.
  posEffectData <- create_sample_data(1,.1, wSEsd = .5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  posEffectData$var2 <- rep(c(1,2), nrow(posEffectData)/2)
  res_pe_one_var <- get_condition_classification(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_condition_classification(posEffectData, idv = "id", dv = c('var','var2'), iv = 'condition')

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$accuracy_per_id$score, nSubj)
  diff.t <- t.test(res_pe$accuracy_per_id$score, res_pe_one_var$accuracy_per_id$score)
  testthat::expect_lt(alpha, diff.t$p.value)
})


test_that("TestClassification.GetConditionClassification - Multivariate, Positive Effect", {
  # test get condition classification with a true positive effect, and 3 informative dependent variables.
  # we compare the average classification accuracy of a classifier trained on only 1 dependent variable, with a classifier trained on all 3 dependent variables.
  posEffectData <- create_sample_data(1,.1, wSEsd = .5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(1,.1, wSEsd = .5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherPosEffectData <- create_sample_data(1,.1, wSEsd = .5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  posEffectData$var2 <- otherPosEffectData$var
  posEffectData$var3 <- anotherPosEffectData$var
  res_pe_one_var <- get_condition_classification(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_condition_classification(posEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition')

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$accuracy_per_id$score, nSubj)
  testthat::expect_lt(res_pe_one_var$statistic, res_pe$statistic)
  diff.t <- t.test(res_pe$accuracy_per_id$score, res_pe_one_var$accuracy_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
})

test_that("TestClassification.TestConditionClassification - Multivariate, Positive Effect", {
  # test 'test condition classification' with a true positive effect, and 3 informative dependent variables.
  # we compare the average classification accuracy of a classifier trained on only 1 dependent variable, with a classifier trained on all 3 dependent variables.
  posEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherPosEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  posEffectData$var2 <- otherPosEffectData$var
  posEffectData$var3 <- anotherPosEffectData$var
  res_pe_one_var <- test_condition_classification(posEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)
  res_pe <- test_condition_classification(posEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$null_dist, nNullSamples)
  testthat::expect_lt(res_pe_one_var$statistic, res_pe$statistic)
  testthat::expect_lt(res_pe$p, alpha)
})
