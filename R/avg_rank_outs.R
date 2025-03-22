# Average rank of outliers
#' Average Rank Of Outlier
#'
#' @description Function computing the average rank of the outliers given a vector with scores of outlyingness.
#'
#'
#' @param scores Scores of (nominal) outlyingness. A higher score here implies an observation is more likely to be an outlier.
#' @param outs Vector of outlier indices.
#' @param ties A character string specifying how ties in scores are treated; can be one of "average", "first", "last",
#' "random", "max" or "min".
#'
#' @returns Average rank of outliers.
#' @export
#'
#' @examples
#' dt <- as.data.frame(sample(c(1:2), 100, replace = TRUE, prob = c(0.5, 0.5)))
#' dt <- cbind(dt, sample(c(1:3), 100, replace = TRUE, prob = c(0.5, 0.3, 0.2)))
#' dt[, 1] <- as.factor(dt[, 1])
#' dt[, 2] <- as.factor(dt[, 2])
#' colnames(dt) <- c('V1', 'V2')
#' sono_out <- sono(data = dt, probs = list(c(0.5, 0.5), c(1/3, 1/3, 1/3)),
#' alpha = 0.01, r = 2, MAXLEN = 0, frequent = FALSE)
#' # Suppose observations 1 up to 5 are outliers
#' avg_rank_outs(scores = sono_out[[2]][, 2], outs = c(1:5), ties = "min")
#'
avg_rank_outs <- function(scores, outs, ties = "min"){
  ### INPUT CHECKS ###
  if (max(outs) > length(scores)){
    stop("Invalid outlier indices.")
  }
  if (!ties %in% c("average", "first", "last", "random", "max", "min")){
    stop("Ties method has to be one of 'average', 'first', 'last', 'random', 'max' or 'min'.")
  }
  ### END OF CHECKS ###
  ranks <- rank(-scores, ties.method = ties)
  outs_ranks <- ranks[outs]
  return(mean(outs_ranks))
}

