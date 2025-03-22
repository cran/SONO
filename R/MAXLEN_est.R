# Estimate MAXLEN
#' Estimate MAXLEN
#'
#' @description Function estimating the value of MAXLEN (stopping criterion) prior to running the SONO algorithm. The estimation is done using
#' the ideas described in \insertCite{costa_novel_2025;textual}{SONO}, using simultaneous confidence intervals for Multinomial proportions, as done by
#' \insertCite{sison_simultaneous_1995;textual}{SONO}.
#'
#'
#' @param data Dataset; needs to be of class data.frame and consist of factor variables only.
#' @param probs List of probability vectors for each variable. Each element of the list must
#' include as many probabilities as the number of levels associated with it in the dataset.
#' @param alpha Significance level for the simultaneous Multinomial confidence intervals constructed, determining what the
#' frequency thresholds should be for itemsets of different length, used for outlier detection for discrete features. Must be a positive real, at most equal to 0.50. A
#' greater value leads to a much more conservative algorithm. Default value is 0.01.
#' @param frequent Logical determining whether highly frequent or highly infrequent itemsets are considered as outliers. Defaults
#' to FALSE, treating highly infrequent itemsets as outlying.
#'
#' @returns Estimated MAXLEN value.
#' @export
#'
#' @references{
#'   \insertRef{costa_novel_2025}{SONO}
#'
#'   \insertRef{sison_simultaneous_1995}{SONO}
#' }
#'
#' @importFrom Rdpack reprompt
#'
#' @examples
#' dt <- as.data.frame(sample(c(1:2), 100, replace = TRUE, prob = c(0.5, 0.5)))
#' dt <- cbind(dt, sample(c(1:3), 100, replace = TRUE, prob = c(0.5, 0.3, 0.2)))
#' dt[, 1] <- as.factor(dt[, 1])
#' dt[, 2] <- as.factor(dt[, 2])
#' colnames(dt) <- c('V1', 'V2')
#' MAXLEN_est(data = dt, probs = list(c(0.5, 0.5), c(1/3, 1/3, 1/3)), alpha = 0.01, frequent = FALSE)
#'
MAXLEN_est <- function(data, probs, alpha = 0.01, frequent = FALSE){
  disc_cols <- c(1:ncol(data))
  ### INPUT CHECKS ###
  if (!is.data.frame(data)){
    stop("Data set should be of class 'data.frame'.")
  }
  stopifnot("alpha should be of class 'numeric'." = is.numeric(alpha))
  if (length(alpha) > 1){
    stop("alpha should be of unit length.")
  }
  if (alpha <= 0 | alpha > 0.50){
    stop("alpha should be positive and at most equal to 0.50.")
  }
  if (length(probs) != length(disc_cols)){
    stop("Incorrect number of probability vectors.")
  }
  if (any(sapply(probs, sum) != 1)){
    stop("Probability vectors should sum to 1.")
  }
  if (any(sapply(probs, min) <= 0) | any(sapply(probs, max) >= 1)){
    stop("Probabilities can only be between zero and one.")
  }
  if (any(sapply(data, FUN = function(x) length(unique(x))) != sapply(probs, length))){
    stop("Number of elements in probability vectors need to match the number of levels of each variable.")
  }
  ### END OF CHECKS ###
  # Get all power sets up to length MAXLEN
  # For MAXLEN we need to make sure that the threshold value s >= 2
  # In order to do that, we take combinations of variables and calculate s
  # Until we achieve s^u < 2
  # The above of course only applies as long as MAXLEN is set equal to 0
  if (length(disc_cols)==1){
    MAXLEN <- 1
  } else {
    if (frequent){
      # Max expected probabilities
      max_probs <- unlist(lapply(probs, max))
      ordered_max_probs <- order(max_probs, decreasing = TRUE)
      for (i in 1:length(disc_cols)){
        probs_vec <- Reduce(kronecker, probs[c(ordered_max_probs[1:i])])
        s <- max(floor(as.numeric(nrow(data) * DescTools::MultinomCI(probs_vec*nrow(data), conf.level=(1-2*alpha))[, 3])))
        if (s == nrow(data)){
          MAXLEN <- i-1
          break
        } else {
          MAXLEN <- i
        }
      }
    }
    else {
      # Max expected probabilities
      max_probs <- unlist(lapply(probs, max))
      ordered_max_probs <- order(max_probs, decreasing = TRUE)
      for (i in 1:length(disc_cols)){
        probs_vec <- Reduce(kronecker, probs[c(ordered_max_probs[1:i])])
        s <- max(floor(as.numeric(nrow(data) * DescTools::MultinomCI(probs_vec*nrow(data), conf.level=(1-2*alpha))[, 2])))
        if (s < 2){
          MAXLEN <- i-1
          break
        } else {
          MAXLEN <- i
        }
      }
    }
  }
  return(MAXLEN)
}
