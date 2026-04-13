## -----------------------------------------------------------------------------
#| label: setup
library(causalDisco)


## -----------------------------------------------------------------------------
#| label: my_custom_test
my_test <- function(x, y, conditioning_set, suff_stat) {
  C <- suff_stat$C
  n <- suff_stat$n

  vars <- c(x, y, conditioning_set)
  C_sub <- C[vars, vars, drop = FALSE]
  K <- solve(C_sub)
  r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
  z <- 0.5 * log((1 + r) / (1 - r))

  stat <- sqrt(n - length(conditioning_set) - 3) * abs(z)

  pval <- 2 * (1 - pnorm(stat))

  pval
}

my_suff_stat <- function(data) {
  list(
    C = cor(data),
    n = nrow(data)
  )
}


## -----------------------------------------------------------------------------
#| label: using_my_custom_test_causalDisco
data(num_data)

my_tpc <- tpc(
  engine = "causalDisco",
  test = my_test,
  alpha = 0.05,
  suff_stat_fun = my_suff_stat
)
result <- disco(data = num_data, method = my_tpc)
plot(result)


## -----------------------------------------------------------------------------
#| label: using_my_custom_test_pcalg
my_pc <- pc(
  engine = "pcalg",
  test = my_test,
  alpha = 0.05,
  suff_stat_fun = my_suff_stat
)
result <- disco(data = num_data, method = my_pc)
plot(result)


## -----------------------------------------------------------------------------
#| label: my_custom_test_bnlearn
my_test_bnlearn <- function(x, y, conditioning_set, suff_stat, args = list()) {
  not_used <- args$not_used
  C <- cor(suff_stat)
  n <- nrow(suff_stat)

  vars <- c(x, y, conditioning_set)
  C_sub <- C[vars, vars, drop = FALSE]
  K <- solve(C_sub)
  r <- -K[1, 2] / sqrt(K[1, 1] * K[2, 2])
  z <- 0.5 * log((1 + r) / (1 - r))

  stat <- sqrt(n - length(conditioning_set) - 3) * abs(z)

  pval <- 2 * (1 - pnorm(stat))

  pval
}

my_pc <- pc(
  engine = "bnlearn",
  test = my_test_bnlearn,
  alpha = 0.05,
  args = list(not_used = "Example of passing additional arguments")
)
result <- disco(data = num_data, method = my_pc)
plot(result)

