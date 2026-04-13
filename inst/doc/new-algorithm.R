## -----------------------------------------------------------------------------
#| label: setup
library(causalDisco)


## -----------------------------------------------------------------------------
#| label: hpc function
hpc <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "hpc",
    engine = engine,
    engine_fns = list(
      bnlearn = function(...) make_runner(engine = "bnlearn", alg = "hpc", ...)
    ),
    test = test,
    alpha = alpha,
    graph_class = "PDAG",
    ...
  )
}


## -----------------------------------------------------------------------------
#| label: hpc example
data(tpc_example)
hpc_bnlearn <- hpc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
hpc_bnlearn_result <- disco(tpc_example, hpc_bnlearn)
plot(hpc_bnlearn_result)


## -----------------------------------------------------------------------------
#| label: register algorithm
register_tetrad_algorithm(
  "my_boss_variant",
  function(
    self,
    num_starts = 1,
    use_bes = TRUE,
    use_data_order = TRUE,
    output_cpdag = TRUE
  ) {
    self$set_params(
      USE_BES = use_bes,
      NUM_STARTS = num_starts,
      USE_DATA_ORDER = use_data_order,
      OUTPUT_CPDAG = output_cpdag
    )

    self$alg <- rJava::.jnew(
      "edu/cmu/tetrad/algcomparison/algorithm/oracle/cpdag/Boss",
      self$score
    )
    self$alg$setKnowledge(self$knowledge)
  }
)


## -----------------------------------------------------------------------------
#| label: my_boss_variant function
my_boss_variant <- function(
  engine = "tetrad",
  score,
  ...
) {
  engine <- match.arg(engine)

  make_method(
    method_name = "my_boss_variant",
    engine = engine,
    engine_fns = list(
      tetrad = function(...) {
        make_runner(engine = "tetrad", alg = "my_boss_variant", ...)
      }
    ),
    score = score,
    graph_class = "PDAG",
    ...
  )
}


## -----------------------------------------------------------------------------
#| label: my_boss_variant example
#| eval: false
# # Ensure Tetrad is installed and Java is working before running the algorithm
# if (verify_tetrad()$installed && verify_tetrad()$java_ok) {
#   my_boss_variant_tetrad <- my_boss_variant(
#     engine = "tetrad",
#     score = "sem_bic"
#   )
#   my_boss_variant_tetrad_result <- disco(tpc_example, my_boss_variant_tetrad)
#   plot(my_boss_variant_tetrad_result)
# }

## -----------------------------------------------------------------------------
#| label: my_boss_variant_tetrad algorithm plot code
#| echo: false
#| eval: false
# # Code to generate the plot image on pkgdown and CRAN.
# 
# # For pkgdown (comment out to avoid CRAN NOTE about unstated dependency used) use the function fig_settings
# # ragg::agg_png(
# #   filename = "custom-boss-variant-pkgdown.png",
# #   width  = fig_settings$fig.width,
# #   height = fig_settings$fig.height,
# #   units  = "in",
# #   res    = fig_settings$dpi * fig_settings$fig.retina
# # )
# my_boss_variant_tetrad <- my_boss_variant(engine = "tetrad", score = "sem_bic")
# my_boss_variant_tetrad_result <- disco(tpc_example, my_boss_variant_tetrad)
# plot(my_boss_variant_tetrad_result)
# dev.off()
# 
# # For CRAN
# # Hardcoded since knitr::opts_current$get() is different when running in console.
# ragg::agg_png(
#   filename = "custom-boss-variant-cran.png",
#   width = 3,
#   height = 3,
#   units = "in",
#   res = 92 * 1
# )
# my_boss_variant_tetrad <- my_boss_variant(engine = "tetrad", score = "sem_bic")
# my_boss_variant_tetrad_result <- disco(tpc_example, my_boss_variant_tetrad)
# plot(my_boss_variant_tetrad_result)
# dev.off()

## -----------------------------------------------------------------------------
#| label: ges-ebic-tetrad-include
#| echo: false
img_file <- if (identical(Sys.getenv("IN_PKGDOWN"), "true")) {
  "custom-boss-variant-pkgdown.png"
} else {
  "custom-boss-variant-cran.png"
}

knitr::include_graphics(img_file)


## -----------------------------------------------------------------------------
#| label: cleanup
reset_tetrad_alg_registry()

