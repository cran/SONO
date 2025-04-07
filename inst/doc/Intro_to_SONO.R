## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----logo, echo=FALSE, out.width="20%", fig.align="right"---------------------
knitr::include_graphics("../man/figures/logo.png")

## ----setup--------------------------------------------------------------------
library(SONO)
# Generate data
set.seed(1)
X <- sample(c(1:3), 500, replace = TRUE, prob = c(0.2, 0.3, 0.5))
X <- cbind(X, sample(c(1:2), 500, replace = TRUE, prob = c(0.1, 0.9)))
X <- cbind(X, sample(c(1:5), 500, replace = TRUE, prob = rep(0.2, 5)))
X <- data.frame(X)
# Ensure every column is a factor
for (i in 1:ncol(X)){
  X[, i] <- factor(X[, i])
}

# Run SONO with probability vectors matching data generating process
prob_vecs <- list(c(0.2, 0.3, 0.5),
                  c(0.1, 0.9),
                  rep(0.2, 5))
# Run SONO with true probabilities and r = 2
sono_res1 <- sono(data = X,
                  probs = prob_vecs,
                  alpha = 0.01,
                  r = 2,
                  MAXLEN = 0,
                  frequent = FALSE,
                  verbose = TRUE)
# See summary of scores
summary(sono_res1[[2]][, 2])

## ----msspecification----------------------------------------------------------
# Run SONO with misspecified probability vectors
prob_vecs_mis <- list(c(0.4, 0.4, 0.2),
                      c(0.9, 0.1),
                      rep(0.2, 5))
# Run SONO with true probabilities and r = 2
sono_res2 <- sono(data = X,
                  probs = prob_vecs_mis,
                  alpha = 0.01,
                  r = 2,
                  MAXLEN = 0,
                  frequent = FALSE,
                  verbose = TRUE)
# See summary of scores
summary(sono_res2[[2]][, 2])

## ----summaries----------------------------------------------------------------
# See summary of scores for each case
summary(sono_res2[[2]][which(X[, 1] == 1), 2])
summary(sono_res2[[2]][which(X[, 1] == 2), 2])
summary(sono_res2[[2]][which(X[, 2] == 1), 2])

## ----contributions_plot-------------------------------------------------------
# Plot matrix of contributions
vis_contribs(contribs_mat = sono_res2[[3]],
             subset = which(sono_res2[[2]][, 2] > 0),
             scale = "max")

## ----eval_funs----------------------------------------------------------------
outliers <- which(X[, 2] == 1)
# Compute average rank of outliers
avg_rank <- avg_rank_outs(scores = sono_res2[[2]][, 2],
                          outs = outliers,
                          ties = "min")
cat('Average rank of outliers:', avg_rank, '\n')
grid_vals <- c(1, 2.5, seq(5, 100, by = 5))/100
recall <- recall_at_k(scores = sono_res2[[2]][, 2],
                      outs = outliers,
                      grid = grid_vals)
for (i in 1:length(grid_vals)){
  cat('Recall at', grid_vals[i], ':', recall[i], '\n')
}
roc_auc_vals <- roc_auc(scores = sono_res2[[2]][, 2],
                        outs = outliers,
                        grid = grid_vals)
for (i in 1:length(grid_vals)){
  cat('ROC AUC at', grid_vals[i], ':', roc_auc_vals[i], '\n')
}

