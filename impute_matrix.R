#' Imputes missing values in numeric matrix
#'
#' @param infile CSV file path containing numeric matrix with missing values
#' @param outfile CSV file path of output matrix
#'
#' @return List with "n_missing" (number of missing elements in the original matrix) and
#'    "missing_percentage" (percentage of missing elements in the original matrix)
#'
#' @export
imputeColumns <- function(infile, outfile = "matrix_imputed.csv") {
  ...
}

library(Matrix)

imputeColumns <- function (input_file, output_file) {
  #1a read numeric matrix
  mat <- as.matrix(read.csv(input_file, header = TRUE, check.names = FALSE))

  #1b compute number and percentages of missing elements in original matrix
  n_missing <- sum(is.na(mat))
  missing_percentage <- 100* n_missing / length(mat)

  #1c replace missing values with median of observed values in the same column
  for (j in 1:ncol(mat)) {
    mat[is.na(mat[, j]), j] <- median(mat[, j], na.rm = TRUE)
  }
  #1d save completed matrix
  write.csv(mat, output_file, row.names = FALSE)

  #1e list of number and percentages of missing elements in original matrix
  list(n_missing = n_missing, missing_percentage = missing_percentage)
}

result <- imputeColumns("data/matrix_missing.csv", "data/matrix_imputed.csv")
result
