
.n_count_up <- function(n_x, n_y, r){
  .n_x <- n_x
  .n_y <- n_y

  if ( (n_y/n_x) < r) {
    .n_y <- n_y + 1
  }
  if ( (n_y/n_x) == r) {
    if (r < 1)  {.n_y <- n_y + 1}
    if (r == 1) {
      .n_x <- n_x + 1
      .n_y <- n_y + 1
    }
    if (r > 1) {.n_x <- n_x + 1}
  }
  if ( (n_y/n_x) > r) {
    .n_x <- n_x + 1
  }

  result <- list(n_x = .n_x, n_y = .n_y)
  return(result)
}


.n_count_down <- function(n_x, n_y, r){
  .n_x <- n_x
  .n_y <- n_y

  if ( (n_y/n_x) < r) {
    .n_x <- n_x - 1
  }
  if ( (n_y/n_x) == r) {
    if (r < 1)  {.n_x <- n_x - 1}
    if (r == 1) {
      .n_x <- n_x - 1
      .n_y <- n_y - 1
    }
    if (r > 1) {.n_y <- n_y - 1}
  }
  if ( (n_y/n_x) > r) {
    .n_y <- n_y - 1
  }

  result <- list(n_x = .n_x, n_y = .n_y)
  return(result)
}
