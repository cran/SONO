check_vecs_equal <- function(vec1, vec2){
  sum(vec1==vec2)
}
match_numeric <- function(x, table){
  are.equal <- function(x, y) isTRUE(all.equal(x, y))
  match.one <- function(x, table)
    match(TRUE, vapply(table, are.equal, logical(1L), x = x))
  vapply(x, match.one, integer(1L), table)
}

count_digits <- function(string) {
  digits_only <- gsub("[^0-9]", "", string)
  num_digits <- nchar(digits_only)
  return(num_digits)
}
