create_sample_data <- function(p_mean, p_sd, seed = 1) {
  set.seed(seed)
  library(tibble)
  # define the number of subjects
  N <- 30
  # 0 = faster condition (e.g., 'congruent'), 0 = slower condition (e.g., 'incongruent'),
  conditionLabels <- c(0,1)
  # define the number of trials per condition
  trialsPerCnd <- 100
  # define the number of trials across all conditions
  trialsN <- trialsPerCnd * length(conditionLabels)

  # define the baseline dependent measure statistical features
  effect_baseline <- 0
  within_subj_effect_sd <- 2

  # define the effect statistical features
  population_sd = p_sd
  population_mean = p_mean

  # create an id column for the samples data
  idv <- rep(1:N, each = trialsN)
  # create a independent variable column
  iv <- rep(rep(conditionLabels, each = trialsPerCnd), N)

  # sample effects for each subject
  subj_true_effect <- rnorm(N,population_mean,population_sd)
  # sample effects for each subject and trial
  subj_true_effect_per_trial <- rep(subj_true_effect, each = trialsN)
  # set the dependent variable columns according to baseine, the true effect, and the indepdent variable
  dv <- rnorm(length(idv), effect_baseline, within_subj_effect_sd) + iv * subj_true_effect_per_trial
  # create a dataframe based on the three columns generated above
  sampled_data <- data.frame(id = idv, condition = iv, var = dv)
  return (sampled_data)
}
