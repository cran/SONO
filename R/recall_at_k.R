# Recall@K
#' Recall@K
#'
#' @description Function computing the recall based on the top K% scores of outlyingness.
#'
#'
#' @param scores Scores of (nominal) outlyingness. A higher score here implies an observation is more likely to be an outlier.
#' @param outs Vector of outlier indices.
#' @param grid Grid of K values over which the recall is computed. Must be between 0 and 1.
#'
#' @returns Recall values at the points of the provided grid.
#' @export
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
#' recall_at_k(scores = sono_out[[2]][, 2],
#' outs = c(1:5),
#' grid = c(1, 2.5, seq(5, 50, by = 5))/100)
#'
recall_at_k <- function(scores, outs, grid){
  ### INPUT CHECKS ###
  if (max(outs) > length(scores)){
    stop("Invalid outlier indices.")
  }
  if (any(grid > 1) | any(grid < 0)){
    stop("Grid can only include values from 0 up to 1.")
  }
  ### END OF CHECKS ###
  recall <- c()
  for (i in grid){
    top_k_outs_num <- round(i*length(scores))
    top_k_outs <- order(scores, decreasing = TRUE)[1:top_k_outs_num]
    recall <- c(recall, sum(top_k_outs %in% outs)/length(outs))
  }
  return(recall)
}
