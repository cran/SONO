# SONO (Scores Of Nominal Outlyingness)
The `SONO` (Scores Of Nominal Outlyingness) `R` package includes a function that can be used for detecting outliers in data sets consisting of nominal data. Some of the capabilities of the package include:

- Calculating scores of outlyingness for data sets consisting of nominal variables.
- Estimating the maximum length of nominal sequences (MAXLEN) for doing frequent pattern mining.
- Computing maximum/minimum itemset support threshold values.
- Visualising the matrix of variable contributions to the score of nominal outlyingness computed.
- Computing evaluation metrics to compare performance of outlier detection algorithms (ROC AUC, Recall@K, Average Outlier Rank).

A detailed description of the methods included in the package can be found in [Costa, E., & Papatsouma, I. (2025). A novel framework for quantifying nominal outlyingness.](https://arxiv.org/abs/2408.07463)

# Installation
The package can be installed using [devtools](https://devtools.r-lib.org/).
```R
# install.packages("devtools")
devtools::install_github('EfthymiosCosta/SONO')
```

