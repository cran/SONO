# ROC AUC function
#' ROC AUC function
#'
#' @description Function computing the ROC AUC given a vector with scores of outlyingness. The computation for this is based on
#' \insertCite{hanley_meaning_1982;textual}{SONO}.
#'
#'
#'
#' @param scores Scores of (nominal) outlyingness. A higher score here implies an observation is more likely to be an outlier.
#' @param outs Vector of outlier indices.
#' @param grid Grid of Top K values over which the ROC AUC is computed. Must be between 0 and 1.
#'
#' @returns ROC AUC  at the points of the provided grid.
#' @export
#'
#' @references{
#'   \insertRef{hanley_meaning_1982}{SONO}
#' }
#'
#' @examples
#' dt <- as.data.frame(sample(c(1:2), 100, replace = TRUE, prob = c(0.5, 0.5)))
#' dt <- cbind(dt, sample(c(1:3), 100, replace = TRUE, prob = c(0.5, 0.3, 0.2)))
#' dt[, 1] <- as.factor(dt[, 1])
#' dt[, 2] <- as.factor(dt[, 2])
#' colnames(dt) <- c('V1', 'V2')
#' sono_out <- sono(data = dt,
#' probs = list(c(0.5, 0.5), c(1/3, 1/3, 1/3)),
#' alpha = 0.01,
#' r = 2,
#' MAXLEN = 0,
#' frequent = FALSE)
#' # Suppose observations 1 up to 5 are outliers
#' roc_auc(scores = sono_out[[2]][, 2], outs = c(1:5),
#' grid = c(1, 2.5, seq(5, 50, by = 5))/100)
#'
roc_auc <- function(scores, outs, grid){
  ### INPUT CHECKS ###
  if (max(outs) > length(scores)){
    stop("Invalid outlier indices.")
  }
  if (any(grid > 1) | any(grid < 0)){
    stop("Grid can only include values from 0 up to 1.")
  }
  ### END OF CHECKS ###
  labels <- numeric(length(scores))
  labels[outs] <- 1

  ordered_indices <- order(scores, decreasing = TRUE)
  scores_ordered <- scores[ordered_indices]
  labels_ordered <- labels[ordered_indices]

  n <- length(scores)
  aucs <- c()

  for (k in grid){
    # Use only top k scores
    subset_scores <- scores_ordered[1:ceiling(k*length(scores))]
    subset_labels <- labels_ordered[1:ceiling(k*length(scores))]
    # Compute AUC for the subset
    aucs <- c(aucs, compute_roc_auc(subset_scores, subset_labels))
  }
  return(aucs)
}
