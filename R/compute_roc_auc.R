compute_roc_auc <- function(scores, labels){
  outlier_indices <- which(labels == 1)
  inlier_indices <- which(labels == 0)
  num_outliers <- length(outlier_indices)
  num_inliers <- length(inlier_indices)

  total_score <- 0
  if (length(outlier_indices)==0){
    return(0)
  } else if (length(inlier_indices)==0){
    return(1)
  } else {
    for (o in outlier_indices) {
      for (i in inlier_indices) {
        if (scores[o] > scores[i]) {
          total_score <- total_score + 1
        } else if (scores[o] == scores[i]) {
          total_score <- total_score + 0.5
        }
      }
    }
  }
  roc_auc <- total_score / (num_outliers * num_inliers)
  return(roc_auc)
}
