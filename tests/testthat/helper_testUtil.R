# "Tests for the directional effect functions"
seed_test <- 1001
nNullSamples <- 1000
alpha <- .05
nSubj = 20
nTrials = 30

# a helper function to apply to results of different analyses
null_rejected <- function(res, alpha) {res$p < alpha}

# a helper function get the different types of datasets used to test the package
wn_ds_lbl <- 'wn'
sn_ds_lbl <- 'sn'
pe_ds_lbl <- 'pe'
wn_eff <- 2
wn_err <- 2
sn_err <- 2
pe_eff <- 2
pe_errw <- 1
pe_errb <- .1
# a helper function to get a dataset with a weak/strong null or positive effect.
# used by other testing function
get_test_data <- function(type) {
  if (type == wn_ds_lbl) {
    ds <- create_sample_data(0,wn_eff, wSEsd = wn_err, N = nSubj,
                             trials_per_cnd = nTrials, seed = seed_test)
  } else if (type == sn_ds_lbl) {
    ds <- create_sample_data(0,0, wSEsd = sn_err, N = nSubj,
                       trials_per_cnd = nTrials, seed = seed_test)
  } else if (type == pe_ds_lbl) {
    ds <- create_sample_data(pe_eff,pe_errb, wSEsd = pe_errw, N = nSubj,
                       trials_per_cnd = nTrials, seed = seed_test)
  } else { stop(paste0('There is no \'', type, '\' type of dataset')) }
  return(ds)
}


#' Create Sample Data
#' @description The function generated mock data for tests and examples according to the arguments

#' @param p_mean the effect's population mean
#' @param p_sd the standard deviation of the population's effect
#' @param seed - a seed to use when generating the resulting data frame
#' @param N - the number of simulated participants
#' @param trials_per_cnd - the number of simulated trials per condition
#' @param wSEsd - the standard deviation of the dependent measure (within subject error term)
#'
#' @return a data frame with three columns: id (participant id), 'condition' (condition label), and 'var' (the dependent variable)
create_sample_data <- function(p_mean, p_sd, seed = 1, N = 30, trials_per_cnd = 100, wSEsd = 2) {
  set.seed(seed)
  # 0 = faster/smaller condition (e.g., 'congruent'), 1 = slower/larger condition (e.g., 'incongruent'),
  conditionLabels <- c(0,1)
  # define the number of trials across all conditions
  trialsN <- trials_per_cnd * length(conditionLabels)

  # define the baseline dependent measure statistical features
  effect_baseline <- 0
  within_subj_effect_sd <- wSEsd

  # define the effect statistical features
  population_sd = p_sd
  population_mean = p_mean

  # create an id column for the samples data
  idv <- rep(1:N, each = trialsN)
  # create a independent variable column
  iv <- rep(rep(conditionLabels, each = trials_per_cnd), N)

  # sample effects for each subject
  subj_true_effect <- stats::rnorm(N,population_mean,population_sd)
  # sample effects for each subject and trial
  subj_true_effect_per_trial <- rep(subj_true_effect, each = trialsN)
  # set the dependent variable columns according to baseine, the true effect, and the indepdent variable
  dv <- stats::rnorm(length(idv), effect_baseline, within_subj_effect_sd) + iv * subj_true_effect_per_trial
  # create a dataframe based on the three columns generated above
  sampled_data <- data.frame(id = idv, condition = iv, var = dv)
  return (sampled_data)
}
