#' Visual Metacognition dataset.
#'
#' A dataset from an experiment where participants reported the orientation of visual gratings,
#' and then rated their confidence on a 1-6 scale.
#'
#' @format A data frame with 46 participants, 9088 trials (rows) and 6 variables:
#' \describe{
#'   \item{Subj_idx}{an identifier for each participant}
#'   \item{Stimulus}{a binary code for the stimulus presented on each trial (a grating tilted to the left / right)}
#'   \item{Response}{participant's orientation response on each trial}
#'   \item{Confidence}{participant's confidence response on each trial}
#'   \item{RT_dec}{participant's reaction time to the orientation response on each trial}
#'   \item{Accuracy}{participant's accuracy in descrimination the orientation of the grating on each trial}
#' }
"visual_metacognition"
