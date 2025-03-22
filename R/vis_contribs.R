#' @name vis_contribs
#' @title Visualise Contribution Matrix
#'
#' @description Function producing a visualisation of the matrix of variable contributions. The user can choose to plot
#' just a subset of the data (for instance the outliers), as well as scale the scores if needed.
#'
#' @param contribs_mat Matrix of variable contributions. Must be of class `data.frame` with as many columns as the number of variables
#' and rows representing the observations.
#' @param subset Subset of observations for which the variable contribution matrix will be plotted.
#' @param scale Optional scaling parameter; defaults to "none" for no scaling. Possible options are "row", with each row being
#' divided by its sum or "max", where each element of the matrix is divided by the maximum element.
#'
#' @returns Plot
#' @export
#'
#' @examples
#' \donttest{
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
#' vis_contribs(contribs_mat = sono_out[[3]], subset = c(1:50), scale = "row")
#' }

utils::globalVariables(c("Column", "Row", "Value"))
vis_contribs <- function(contribs_mat, subset = NULL, scale = "none"){
  ### INPUT CHECKS ###
  if (!is.data.frame(contribs_mat)){
    stop("Matrix of contributions should be of class 'data.frame'.")
  }
  if (any(contribs_mat < 0)){
    stop("Matrix of contributions can only have non-negative values.")
  }
  if (max(subset) > nrow(contribs_mat)){
    stop("Invalid subset.")
  }
  if (!scale %in% c("row", "max", "none")){
    stop("Scaling can only be one of 'row', 'max' or 'none'.")
  }
  ### END OF CHECKS ###
  if (!is.null(subset)){
    contribs_mat <- contribs_mat[subset, ]
  }
  if (scale == "row"){
    excl_rows <- which(rowSums(contribs_mat) == 0)
    if (length(excl_rows) > 0){
      contribs_mat[-excl_rows, ] <- contribs_mat[-excl_rows, ]/(rowSums(contribs_mat)[-excl_rows])
    }
  } else if (scale == "max"){
    contribs_mat <- contribs_mat/max(contribs_mat)
  }
  contribs_mat_f <- data.table::as.data.table(contribs_mat, keep.rownames = "Row")

  contribs_mat_f <- data.table::melt(contribs_mat_f,
                                     id.vars = "Row",
                                     variable.name = "Column",
                                     value.name = "Value")
  plt_contribmat <- ggplot2::ggplot(contribs_mat_f, ggplot2::aes(
    x = Column,
    y = as.factor(Row),
    fill = Value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = "magma") +
    ggplot2::labs(title = "Contribution Matrix Plot",
                  x = "Variable",
                  y = "Observation",
                  fill = "Contribution") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
  return(plt_contribmat)
}
