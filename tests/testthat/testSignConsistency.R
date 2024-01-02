####################################################
################## GENERAL TESTS ###################
####################################################

test_that("TestSignConsistency.GetSignConsistency - Positive Effect", {
  # test get sign consistency with a true positive effect
  posEffectData <- get_test_data(pe_ds_lbl)
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id$score, nSubj)
  testthat::expect_lt( .5, res_pe$statistic)
})

test_that("TestSignConsistency.TestSignConsistency - Weak Null", {
  # test for significant results when we expect a weak null
  signconData <- get_test_data(wn_ds_lbl)
  res_wn <- test_sign_consistency(signconData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type( res_wn$p, "double")
  testthat::expect_length(res_wn$null_dist, nNullSamples)
  testthat::expect_lt( res_wn$p,alpha)
})

test_that("TestSignConsistency.TestSignConsistency - Strong Null", {
  # test for false positives high noise
  strongNullData <- get_test_data(sn_ds_lbl)
  res_sn <- test_sign_consistency(strongNullData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_sn$p, "double")
  testthat::expect_length(res_sn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_sn$p)
})

test_that("TestSignConsistency.TestSignConsistency - Positive Effect - Small IDs", {
  # test for positive effect with small individual differences
  posEffectData_smallIds <- get_test_data(pe_ds_lbl)
  res_pe_smallIds <- test_sign_consistency(posEffectData_smallIds, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_pe_smallIds$p, "double")
  testthat::expect_length(res_pe_smallIds$null_dist, nNullSamples)
  testthat::expect_lt(res_pe_smallIds$p, alpha)
})

test_that("TestSignConsistency.TestSignConsistency - Positive Effect - Small IDs", {
  # test for positive effect with high individual differences
  posEffectData_highIds <- create_sample_data(1,1.5, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  res_pe_highIds <- test_sign_consistency(posEffectData_highIds, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)

  testthat::expect_type(res_pe_highIds$p, "double")
  testthat::expect_length(res_pe_highIds$null_dist, nNullSamples)
  testthat::expect_lt(res_pe_highIds$p, alpha)
})


####################################################
################ MULTIVARIATE TESTS ################
####################################################

test_that("TestSignConsistency.GetSignConsistency - Multivariate, Strong Null", {
  # test get sign consistency with a strong null effect, and 3 dependent variables
  median_func_mv <- function(mat) {stats::median(c(mat[,'var'], mat[,'var2'], mat[,'var3']))}
  snEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  snEffectData$var2 <- otherSnEffectData$var
  snEffectData$var3 <- anotherSnEffectData$var
  res_sn <- get_sign_consistency(snEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function = median_func_mv)

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$consistency_per_id$score, nSubj)
})

test_that("TestSignConsistency.TestSignConsistency - Multivariate, Strong Null", {
  # test 'test sign consistency' with a strong null effect
  median_func_mv <- function(mat) {stats::median(c(mat[,'var'], mat[,'var2'], mat[,'var3']))}
  snEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherSnEffectData <- create_sample_data(0,0, wSEsd = 2, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  snEffectData$var2 <- otherSnEffectData$var
  snEffectData$var3 <- anotherSnEffectData$var
  res_sn <- test_sign_consistency(snEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function = median_func_mv, null_dist_samples =  nNullSamples)

  testthat::expect_type(res_sn$statistic, "double")
  testthat::expect_length(res_sn$null_dist, nNullSamples)
  testthat::expect_lt(alpha, res_sn$p)
})

test_that("TestSignConsistency.TestSignConsistency - Multivariate, Positive Effect", {
  # test 'test sign consistency' with a positive effect, and 3 informative variables
  posEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  posEffectData$var2 <- otherPosEffectData$var
  posEffectData$var3 <- anotherPosEffectData$var
  res_pe_one_var <- test_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition', null_dist_samples = nNullSamples)
  sf <- function(data) { mean(as.matrix(data)) }
  res_pe <- test_sign_consistency(posEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition',
                                  null_dist_samples = nNullSamples, summary_function = sf)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$null_dist, nNullSamples)
  testthat::expect_lt(res_pe$p, alpha)
  testthat::expect_lt(res_pe_one_var$statistic, res_pe$statistic)
})

test_that("TestSignConsistency.GetSignConsistency - Multivariate, Positive Effect, 2nd null", {
  # test get sign consistency with a positive effect for one dependent variable, and a 2nd uninformative dependent variable.
  posEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  posEffectData$var2 <- rep(c(1,2), nrow(posEffectData)/2)
  res_pe_one_var <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  sf <- function(data) { mean(as.matrix(data)) }
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = c('var','var2'), iv = 'condition', summary_function = sf)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id$score, nSubj)
  diff.t <- t.test(res_pe$consistency_per_id$score, res_pe_one_var$consistency_per_id$score)
  testthat::expect_lt(alpha, diff.t$p.value)
})


test_that("TestSignConsistency.GetSignConsistency - Multivariate, Positive Effect", {
  median_func_mv <- function(mat) {stats::median(c(mat[,'var'], mat[,'var2'], mat[,'var3']))}
  # test get sign consistency with a true positive effect
  posEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  posEffectData$var2 <- otherPosEffectData$var
  posEffectData$var3 <- anotherPosEffectData$var
  res_pe_one_var <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function =  median_func_mv)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id$score, nSubj)
  testthat::expect_lt(res_pe_one_var$statistic, res_pe$statistic)
  diff.t <- t.test(res_pe$consistency_per_id$score, res_pe_one_var$consistency_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
})


test_that("TestSignConsistency.GetSignConsistency - Multivariate, Positive Effect, Sum", {
  summ_func_mv <- function(mat) {base::sum(c(mat[,'var'], mat[,'var2'], mat[,'var3']))}
  # test get sign consistency with a anther summary function (not mean, but sum)
  posEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj , trials_per_cnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherPosEffectData <- create_sample_data(1,.1, wSEsd = 2.5, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  posEffectData$var2 <- otherPosEffectData$var
  posEffectData$var3 <- anotherPosEffectData$var
  res_pe_one_var <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function =  summ_func_mv)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id$score, nSubj)
  testthat::expect_lt(res_pe_one_var$statistic, res_pe$statistic)
  diff.t <- t.test(res_pe$consistency_per_id$score, res_pe_one_var$consistency_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
})

test_that("TestSignConsistency.GetSignConsistency - Multivariate, Positive Effect, random summary function", {
  rnd_func_mv <- function(mat) {rnorm(1)}
  # test get sign consistency with a a random summary function, expecting null results
  posEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj , trials_per_cnd = nTrials, seed = seed_test)
  otherPosEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 1)
  anotherPosEffectData <- create_sample_data(1,.1, wSEsd = 1, N = nSubj, trials_per_cnd = nTrials, seed = seed_test + 2)
  posEffectData$var2 <- otherPosEffectData$var
  posEffectData$var3 <- anotherPosEffectData$var
  res_pe_one_var <- get_sign_consistency(posEffectData, idv = "id", dv = 'var', iv = 'condition')
  res_pe <- get_sign_consistency(posEffectData, idv = "id", dv = c('var','var2', 'var3'), iv = 'condition', summary_function =  rnd_func_mv)

  testthat::expect_type(res_pe$statistic, "double")
  testthat::expect_length(res_pe$consistency_per_id$score, nSubj)
  testthat::expect_lt(res_pe$statistic, res_pe_one_var$statistic)
  diff.t <- t.test(res_pe$consistency_per_id$score, res_pe_one_var$consistency_per_id$score)
  testthat::expect_lt(diff.t$p.value, alpha)
  testthat::expect_lt(diff.t$statistic, 0)
})

####################################################
################### ERROR TESTS ####################
####################################################

test_that("TestSignConsistency.GetSignConsistency - Max Resampling", {
  smallData <- create_sample_data(1,.1, wSEsd = 2, N = 2, trials_per_cnd = 2, seed = seed_test)
  NAFunc <- function(mat) {ifelse(runif(1) > .5, NA, runif(1))}
  testthat::expect_warning(get_sign_consistency(smallData, idv = "id", dv = 'var', iv = 'condition', summary_function = NAFunc, max_invalid_reps = 1))

  missingData <- smallData[-c(1,2),]
  testthat::expect_warning(get_sign_consistency(missingData, idv = "id", dv = 'var', iv = 'condition', summary_function = NAFunc, max_invalid_reps = 1))
})

test_that("TestSignConsistency.TestSignConsistency - Max Invalids", {
  missingData <- create_sample_data(1,.1, wSEsd = 2, N = 2, trials_per_cnd = 100, seed = seed_test)
  NAFunc <- function(mat) {ifelse(runif(1) < .75, NA, runif(1))}
  testthat::expect_warning(test_sign_consistency(missingData, idv = "id", dv = 'var', iv = 'condition',
                        summary_function = NAFunc, perm_repetitions = 5, null_dist_samples = 10,
                        max_invalid_reps = 1), "the null distribution includes invalid")
})


####################################################
#################### TIES TESTS ####################
####################################################

test_that("TestSignConsistency.GetSignConsistency - Ties (accuracy constnat value)", {
  accNullEffectData <- create_sample_data(p_mean = 0,0, wSEsd = 0, N = 5,
                                          trials_per_cnd = 10, seed = seed_test)
  accNullEffectData$var <- accNullEffectData$var + 1
  testthat::expect_error(get_sign_consistency(accNullEffectData, idv = "id", dv = 'var', iv = 'condition',
                                              max_invalid_reps = 5))
})

test_that("TestSignConsistency.GetSignConsistency - Ties (accuracy 50% per subj)", {
  accNullEffectData <- create_sample_data(p_mean = 0,0, wSEsd = 0, N = 5,
                                          trials_per_cnd = 2, seed = seed_test)
  accNullEffectData$var[seq(1,nrow(accNullEffectData),2)] <- 1
  testthat::expect_warning(get_sign_consistency(accNullEffectData, idv = "id", dv = 'var', iv = 'condition',
                                                max_invalid_reps = 5),
                           "could not compute consistency scores")
})

test_that("TestSignConsistency.GetSignConsistency - Ties (1 subj)", {
  wnEffectData <- create_sample_data(p_mean = 0,10, wSEsd = 5, N = 5,
                                          trials_per_cnd = 50, seed = seed_test)

  wnEffectData[wnEffectData$id == 1,]$var <- 1
  testthat::expect_warning(get_sign_consistency(wnEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5),
                           'calculating group-level sign consistency for 4 participants')
})

test_that("TestSignConsistency.TestSignConsistency - Ties (1 subj)", {
  wnEffectData <- create_sample_data(p_mean = 0,10, wSEsd = 5, N = 5,
                                     trials_per_cnd = 50, seed = seed_test)

  wnEffectData[wnEffectData$id == 1,]$var <- 1
  testthat::expect_warning(res <- test_sign_consistency(wnEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5),
                           'calculating group-level sign consistency for 4 participants. Invalid participant identifiers: 1')
  testthat::expect_length(res$consistency_per_id$id, length(unique(wnEffectData$id)) - 1)
  testthat::expect_lt(res$p, alpha)
})


####################################################
############# SPLIT TYPE ERROR TESTS ###############
####################################################

test_that("TestSignConsistency.SplitType - CountData", {
  n_per_cnd <- 500
  N_p_sn <- 10
  N_p_pe <- 10
  sn_normData <- create_sample_data(p_mean = 0, p_sd = 0, wSEsd = .5, N = N_p_sn,
                                     trials_per_cnd = n_per_cnd, seed = seed_test)
  for(p_i in 1:N_p_sn) {
    sn_normData[sn_normData$id == p_i,]$var <- rep(c(0,1), n_per_cnd)
  }
  pe_normData <- create_sample_data(p_mean = 2, .5, wSEsd = 2, N = N_p_pe,
                                    trials_per_cnd = n_per_cnd, seed = seed_test)
  pe_normData$id <- pe_normData$id + max(sn_normData$id)
  all_data <- rbind(pe_normData, sn_normData)
  res_noverlap <- test_sign_consistency(all_data, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5)
  res_random <- test_sign_consistency(all_data, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "random")
  cnt_data_diff <- t.test(res_noverlap$consistency_per_id$score[1:N_p_sn],
         res_random$consistency_per_id$score[1:N_p_sn])$p.value
  testthat::expect_lt(alpha, res_noverlap$p)
  testthat::expect_lt(res_random$p, alpha)
})


test_that("TestSignConsistency.SplitType - Strong Null", {
  snEffectData <- get_test_data(sn_ds_lbl)
  set.seed(777)
  res_default <- test_sign_consistency(snEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "noverlap")
  set.seed(777)
  res_noverlap <- test_sign_consistency(snEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "noverlap")
  testthat::expect_identical(res_default$consistency_per_id$score,
                         res_noverlap$consistency_per_id$score)
  set.seed(777)
  res_random <- test_sign_consistency(snEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "random")
  testthat::expect_true(!identical(res_random$consistency_per_id$score,
                         res_noverlap$consistency_per_id$score))

  testthat::expect_lt(alpha, res_noverlap$p)
  testthat::expect_lt(alpha, res_random$p)
})

test_that("TestSignConsistency.SplitType - Weak Null", {
  wnEffectData <- create_sample_data(0,2, wSEsd = 4, N = nSubj,
                                     trials_per_cnd = nTrials, seed = seed_test)
  set.seed(777)
  res_default <- test_sign_consistency(wnEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "noverlap")
  set.seed(777)
  res_noverlap <- test_sign_consistency(wnEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "noverlap")
  testthat::expect_identical(res_default$consistency_per_id$score,
                             res_noverlap$consistency_per_id$score)
  set.seed(777)
  res_random <- test_sign_consistency(wnEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "random")
  testthat::expect_true(!identical(res_random$consistency_per_id$score,
                                   res_noverlap$consistency_per_id$score))

  testthat::expect_lt(res_noverlap$p, alpha)
  testthat::expect_lt(res_random$p, alpha)
})

test_that("TestSignConsistency.SplitType - Positive Effect", {
  peEffectData <- get_test_data(pe_ds_lbl)
  set.seed(777)
  res_default <- test_sign_consistency(peEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "noverlap")
  set.seed(777)
  res_noverlap <- test_sign_consistency(peEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "noverlap")
  testthat::expect_identical(res_default$consistency_per_id$score,
                             res_noverlap$consistency_per_id$score)
  set.seed(777)
  res_random <- test_sign_consistency(peEffectData, idv = "id", dv = 'var', iv = 'condition', max_invalid_reps = 5, split_type = "random")

  testthat::expect_lt(res_noverlap$p, alpha)
  testthat::expect_lt(res_random$p, alpha)
})

