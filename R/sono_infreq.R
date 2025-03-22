sono_infreq <- function(data, probs, alpha = 0.01, r = 2, MAXLEN = 0, verbose = TRUE){
  ### INPUT CHECKS ###
  disc_cols <- c(1:ncol(data))
  if (!is.data.frame(data)){
    stop("Data set should be of class 'data.frame'.")
  }
  for (i in disc_cols){
    stopifnot("Discrete variables should be of class 'factor'." = (is.factor(data[, i])))
  }
  if (length(disc_cols)==1){
    data[, disc_cols] <- as.data.frame(data[, disc_cols])
  }
  stopifnot("alpha should be of class 'numeric'." = is.numeric(alpha))
  if (length(alpha) > 1){
    stop("alpha should be of unit length.")
  }
  if (alpha <= 0 | alpha > 0.50){
    stop("alpha should be positive and at most equal to 0.50.")
  }
  if (length(MAXLEN) > 1){
    stop("MAXLEN should be an integer at most equal to the number of discrete variables.")
  }
  if (MAXLEN %% 1 !=0){
    stop("MAXLEN should be an integer at most equal to the number of discrete variables.")
  }
  if (MAXLEN < 0 | MAXLEN > length(disc_cols)){
    stop("MAXLEN should be an integer at most equal to the number of discrete variables.")
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
  MAXLEN_User <- FALSE
  if (MAXLEN == 0){
    MAXLEN_User <- TRUE
    if (length(disc_cols)==1){
      MAXLEN <- 1
    } else {
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
  } else {
    MAXLEN_estimate <- MAXLEN_est(data, probs, alpha, frequent = FALSE)
    if (MAXLEN_estimate < MAXLEN){
      warning('MAXLEN value larger than estimated; MAXLEN set to ', MAXLEN_estimate)
    }
  }
  if (verbose){
    message('MAXLEN: ', MAXLEN)
  }
  # Get power set
  powerset_test <- rje::powerSet(disc_cols, MAXLEN)
  if (verbose){
    message('Power set object created.')
  }

  # Empty list to store probability vectors
  probs_list <- list()

  # Empty list to store outlier scores data frames
  outlierdfs_list <- list()

  # create corresponding list of data frames with sequences for each data point
  for (i in disc_cols){
    data[,i] <- as.numeric(data[,i])
  }

  if (verbose){
    message('Pre-processing done.')
  }

  dfs <- list()
  # List with infrequent items and powersets
  infreq_list <- list()
  for (i in 1:MAXLEN){
    inxs <- which(sapply(X=powerset_test, FUN=length)==i)
    aux_vec <- c()
    for (j in inxs){
      nam <- "df"
      # Setting lower threshold value s
      for (k in 1:length(powerset_test[[j]])){
        nam <- paste(nam, powerset_test[[j]][k], sep="_")
      }
      probs_vec <- Reduce(kronecker, probs[powerset_test[[j]]])
      s_obj <- DescTools::MultinomCI(probs_vec*nrow(data), conf.level=(1-2*alpha))
      s <- as.numeric(nrow(data) * s_obj[, 2])
      s_probs <- s_obj[, 1]
      df <- data.frame('Sequence'=character(),
                       'Count'=integer(),
                       'Frequent'=logical(),
                       'Threshold' =numeric(),
                       stringsAsFactors = FALSE)
      if (i==1){
        dt <- data[, powerset_test[[j]]]
        tab <- table(dt)
        for (k in 1:length(tab)){
          df <- rbind(df, data.frame('Sequence'=names(tab)[k],
                                     'Count'=as.numeric(tab)[k],
                                     'Frequent'=as.numeric(tab)[k]>=s[k],
                                     'Threshold'=s[k]))
          if (as.numeric(tab)[k]<s[k]){
            infreq_list[[length(infreq_list)+1]] <- list("Variables"=powerset_test[[j]],
                                                         "Sequence"=names(tab)[k])
          }
        }
      } else {
        rows <- c()
        if (length(infreq_list)>0){
          for (k in 1:length(infreq_list)){
            if (rje::is.subset(infreq_list[[k]]$Variables, powerset_test[[j]])){
              ifelse(length(infreq_list[[k]]$Variables)>1, {
                rows <- c(rows,
                          which(sapply(1:nrow(data),
                                       FUN = function(i) check_vecs_equal(data[i, infreq_list[[k]]$Variables],
                                                                          vec2=as.numeric(strsplit(infreq_list[[k]]$Sequence, split="_")[[1]])))==length(infreq_list[[k]]$Variables)))
              }, {
                rows <- c(rows,
                          which(data[, infreq_list[[k]]$Variables]==infreq_list[[k]]$Sequence))
              })
            }
          }
        }
        ifelse(length(rows)>0, dt <- data[-rows,powerset_test[[j]]], dt <- data[,powerset_test[[j]]])
        if (dim(dt)[1] == 0){
          newrow <- data.frame('Sequence' = 'X',
                               'Count' = Inf,
                               'Frequent' = FALSE,
                               'Threshold' = 0)
          df <- rbind(df, newrow)
          dfs[[nam]] <- assign(nam, df)
          next
        }
        for (k in 1:nrow(dt)){
          row <- dt[k,]
          rownam <- character()
          for (l in 1:length(row)){
            ifelse(l==1, rownam <- paste0(rownam, row[l]), rownam <- paste(rownam, row[l], sep="_"))
          }
          # Save sequences based on power set, with their support
          ifelse(rownam %in% df$Sequence,
                 {row_inx <- which(df$Sequence==rownam, arr.ind=TRUE)
                 df[row_inx, 2] <- df[row_inx, 2]+1},
                 {prod <- 1
                 probs_sublist <- probs[powerset_test[[j]]]
                 for (row_num in c(1:length(row))){
                   prod <- prod*probs_sublist[[row_num]][as.numeric(row[row_num])]
                 }
                 newrow <- data.frame('Sequence'=rownam,
                                      'Count'=1,
                                      'Frequent'=FALSE,
                                      'Threshold'=s[match_numeric(prod, probs_vec)])
                 df <- rbind(df, newrow)})
        }
        # Sort by increasing sequence name to be able to do comparison with thresholds
        df <- df[order(df$Sequence, decreasing = FALSE),]
        for (k in 1:nrow(df)){
          ifelse(df[k,2]>=df[k,4], df[k,3] <- TRUE, infreq_list[[length(infreq_list)+1]] <- list("Variables"=powerset_test[[j]],
                                                                                                 "Sequence"=df[k,1]))
        }
      }
      if (all(df[,4] < 2) | all(!df[,3])){
        aux_vec <- c(aux_vec, TRUE)
      } else {
        aux_vec <- c(aux_vec, FALSE)
      }
      dfs[[nam]] <- assign(nam, df)
    }
    if (all(aux_vec) & MAXLEN_User){
      MAXLEN <- i-1
      # Remove redundant dfs
      redundant_dfs <- as.numeric(which(sapply(dfs, function(df) sapply(df[1,1], count_digits))==i))
      dfs <- dfs[-redundant_dfs]
      break
    }
  }

  # Preallocate memory for outputs
  outscoredf <- data.frame('Observation' = 1:nrow(data), 'Score' = rep(0, nrow(data)))
  outscoredfcells <- matrix(0, nrow = nrow(data), ncol = length(disc_cols))
  colnames(outscoredfcells) <- colnames(data[, disc_cols, drop = FALSE])

  # Initialize vectors for nod_vec and nod_counts
  nod_vec <- numeric(nrow(data))
  nod_counts <- numeric(nrow(data))

  # Iterate over subset lengths
  for (j in 1:MAXLEN){
    inxs <- which(sapply(powerset_test, length) == j)

    for (k in inxs){
      power_subset <- powerset_test[[k]]
      df_name <- paste0("df_", paste(power_subset, collapse = "_"))

      # Precompute concatenated sequences for all rows
      seq_vals <- apply(data[, power_subset, drop = FALSE], 1, function(row) {
        paste(row, collapse = "_")
      })

      df <- dfs[[match(df_name, names(dfs))]]
      # Skip processing if dfs entry is a placeholder
      if (nrow(df) == 1 && df[1, 1] == "X") next

      # Match sequences in the current data frame
      matched_rows <- match(seq_vals, df$Sequence, nomatch = 0)
      valid <- matched_rows > 0

      if (any(valid)) {
        rows <- which(valid)
        matches <- matched_rows[valid]

        # Compute scores for valid matches
        scores <- (!df[matches, 3]) *
          df[matches, 4] /
          (df[matches, 2] * j^r)

        # Update nod_vec, nod_counts, and outscoredf
        nod_vec[rows] <- nod_vec[rows] + (!df[matches, 3]) * j
        nod_counts[rows] <- nod_counts[rows] + (!df[matches, 3])
        outscoredf$Score[rows] <- outscoredf$Score[rows] + scores

        # Update cell-wise scores
        if (length(disc_cols) > 1) {
          cell_scores <- scores / length(power_subset)
          for (col in power_subset) {
            col_index <- match(col, disc_cols)
            outscoredfcells[rows, col_index] <- outscoredfcells[rows, col_index] + cell_scores
          }
        }
      }
    }
  }

  # Convert outscoredfcells to a data frame for consistency
  outscoredfcells <- as.data.frame(outscoredfcells)

  if (length(disc_cols) == 1){
    outscoredfcells <- as.matrix(outscoredf[, 2], ncol = 1)
    colnames(outscoredfcells) <- colnames(data[, disc_cols])
    outscoredfcells <- as.data.frame(outscoredfcells)
  }
  # Compute average depth
  nod_vec[which(nod_vec > 0)] <- nod_vec[which(nod_vec > 0)]/nod_counts[which(nod_vec > 0)]

  if (verbose){
    message('Outlyingness scores for discrete variables calculated.')
  }
  return(list('MAXLEN'=MAXLEN, 'Discrete Scores'=outscoredf, 'Contributions'=outscoredfcells, 'Depth'=nod_vec))
}
