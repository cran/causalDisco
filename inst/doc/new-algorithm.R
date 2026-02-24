## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(causalDisco)

## ----hpc function-------------------------------------------------------------
hpc <- function(
  engine = c("bnlearn"),
  test,
  alpha = 0.05,
  ...
) {
  engine <- match.arg(engine)
  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      bnlearn = rlang::exec(
        hpc_bnlearn_runner,
        test = test,
        alpha = alpha,
        !!!args
      )
    )
    runner
  }
  method <- new_disco_method(
    builder = builder,
    name = "hpc",
    engine = engine,
    graph_class = "PDAG"
  )
  method
}

## ----hpc runner---------------------------------------------------------------
hpc_bnlearn_runner <- function(test, alpha, ...) {
  args <- list(...)
  search <- BnlearnSearch$new()
  args_to_pass <- distribute_engine_args(
    search = search,
    args = args,
    engine = "bnlearn",
    alg = "hpc"
  )

  search$set_test(test, alpha)
  search$set_alg("hpc", args_to_pass)

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}

## ----hpc example--------------------------------------------------------------
data(tpc_example)
hpc_bnlearn <- hpc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
hpc_bnlearn_result <- disco(tpc_example, hpc_bnlearn)
plot(hpc_bnlearn_result)

## -----------------------------------------------------------------------------
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
my_boss_variant <- function(
  engine = "tetrad",
  score,
  ...
) {
  engine <- match.arg(engine)
  args <- rlang::list2(...)

  builder <- function(knowledge = NULL) {
    runner <- switch(
      engine,
      tetrad = rlang::exec(my_boss_variant_tetrad_runner, score, !!!args)
    )
    runner
  }

  method <- new_disco_method(
    builder = builder,
    name = "my_boss_variant",
    engine = engine,
    graph_class = "PDAG"
  )
  method
}

## ----my_boss_variant runner---------------------------------------------------
my_boss_variant_tetrad_runner <- function(score, ...) {
  search <- TetradSearch$new()
  args <- list(...)
  args_to_pass <- distribute_engine_args(
    search = search,
    args = args,
    engine = "tetrad",
    alg = "my_boss_variant"
  )

  if (length(args_to_pass$score_args) > 0) {
    rlang::exec(search$set_score, score, !!!args_to_pass$score_args)
  } else {
    search$set_score(score)
  }

  if (length(args_to_pass$alg_args) > 0) {
    rlang::exec(search$set_alg, "my_boss_variant", !!!args_to_pass$alg_args)
  } else {
    search$set_alg("my_boss_variant")
  }

  runner <- list(
    set_knowledge = function(knowledge) {
      search$set_knowledge(knowledge)
    },
    run = function(data) {
      search$run_search(data)
    }
  )
  runner
}

## ----my_boss_variant example, eval = FALSE------------------------------------
# # Ensure Tetrad is installed and Java is working before running the algorithm
# if (verify_tetrad()$installed && verify_tetrad()$java_ok) {
#   my_boss_variant_tetrad <- my_boss_variant(engine = "tetrad", score = "sem_bic")
#   my_boss_variant_tetrad_result <- disco(tpc_example, my_boss_variant_tetrad)
#   plot(my_boss_variant_tetrad_result)
# }

## ----my_boss_variant_tetrad algorithm plot code, echo=FALSE, eval = FALSE-----
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
#   width  = 3,
#   height = 3,
#   units  = "in",
#   res    = 92 * 1
# )
# my_boss_variant_tetrad <- my_boss_variant(engine = "tetrad", score = "sem_bic")
# my_boss_variant_tetrad_result <- disco(tpc_example, my_boss_variant_tetrad)
# plot(my_boss_variant_tetrad_result)
# dev.off()

## ----ges-ebic-tetrad-include, echo=FALSE--------------------------------------
img_file <- if (identical(Sys.getenv("IN_PKGDOWN"), "true")) {
  "ges-ebic-tetrad-pkgdown.png"
} else {
  "ges-ebic-tetrad-cran.png"
}

knitr::include_graphics(img_file)

## ----cleanup------------------------------------------------------------------
reset_tetrad_alg_registry()

