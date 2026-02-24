## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(causalDisco)

## ----load data----------------------------------------------------------------
data(num_data)
head(num_data)

## -----------------------------------------------------------------------------
plot_layout <- data.frame(
  name = c("Z", "X3", "X1", "X2", "Y"),
  x = c(0.00, 0.50, 0.00, 0.50, 0.25),
  y = c(0.0, 0.0, 0.5, 0.5, 1.0)
)

## ----pc algorithm fisher z bnlearn--------------------------------------------
pc_bnlearn <- pc(
  engine = "bnlearn", # Use the bnlearn implementation
  test = "fisher_z", # Use Fisher's Z test for conditional independence
  alpha = 0.05 # Significance level for the test
)
pc_result_bnlearn <- disco(data = num_data, method = pc_bnlearn)

## -----------------------------------------------------------------------------
plot(pc_result_bnlearn, layout = plot_layout, main = "PC Fisher Z (bnlearn)")

## ----pc algorithm fisher z pcalg----------------------------------------------
pc_pcalg <- pc(
  engine = "pcalg", # Use the pcalg implementation
  test = "fisher_z", # Use Fisher's Z test for conditional independence
  alpha = 0.05 # Significance level for the test
)
pc_result_pcalg <- disco(data = num_data, method = pc_pcalg)
plot(pc_result_pcalg, layout = plot_layout, main = "PC Fisher Z (pcalg)")

## ----install java, eval=FALSE-------------------------------------------------
# # Use the development version of rJavaEnv from GitHub
# # pak::pak("e-kotov/rJavaEnv")
# rJavaEnv::java_quick_install(version = 25, distribution = "Temurin")

## ----install tetrad, eval=FALSE-----------------------------------------------
# install_tetrad()

## ----check tetrad install-----------------------------------------------------
verify_tetrad()

## ----ges algorithm ebic tetrad, eval = FALSE----------------------------------
# if (verify_tetrad()$installed && verify_tetrad()$java_ok) {
#   ges_tetrad <- ges(
#     engine = "tetrad", # Use the Tetrad implementation
#     score = "ebic" # Use the EBIC score
#   )
#   ges_result_tetrad <- disco(data = num_data, method = ges_tetrad)
#   plot(ges_result_tetrad, layout = plot_layout, main = "GES EBIC (Tetrad)")
# }

## ----ges algorithm ebic tetrad plot code, echo=FALSE, eval = FALSE------------
# # Code to generate the plot image on pkgdown and CRAN.
# 
# # For pkgdown (comment out to avoid CRAN NOTE about unstated dependency used) use the function fig_settings
# # ragg::agg_png(
#   # filename = "ges-ebic-tetrad-pkgdown.png",
# #   width  = fig_settings$fig.width,
# #   height = fig_settings$fig.height,
# #   units  = "in",
# #   res    = fig_settings$dpi * fig_settings$fig.retina
# # )
# # ges_result <- disco(data = num_data, method = ges(engine = "tetrad", score = "ebic"))
# # plot(ges_result, layout = plot_layout, main = "GES EBIC (Tetrad)")
# # dev.off()
# 
# # For CRAN
# # Hardcoded since knitr::opts_current$get() is different when running in console.
# ragg::agg_png(
#   filename = "ges-ebic-tetrad-cran.png",
#   width  = 3,
#   height = 3,
#   units  = "in",
#   res    = 92 * 1
# )
# ges_result <- disco(data = num_data, method = ges(engine = "tetrad", score = "ebic"))
# plot(ges_result, layout = plot_layout, main = "GES EBIC (Tetrad)")
# dev.off()

## ----ges-ebic-tetrad-include, echo=FALSE--------------------------------------
img_file <- if (identical(Sys.getenv("IN_PKGDOWN"), "true")) {
  "ges-ebic-tetrad-pkgdown.png"
} else {
  "ges-ebic-tetrad-cran.png"
}

knitr::include_graphics(img_file)

## ----custom plot--------------------------------------------------------------
plot(
  pc_result_bnlearn,
  layout = plot_layout,
  main = "Customized plot",
  node_style = list(
    fill = "lightblue", # Fill color
    col = "darkblue", # Border color
    lwd = 2, # Border width
    padding = 4, # Text padding (mm)
    size = 1.2 # Size multiplier
  )
)

## ----view results-------------------------------------------------------------
print(pc_result_bnlearn)
summary(pc_result_bnlearn)

## -----------------------------------------------------------------------------
data(tpc_example)
head(tpc_example)

## ----prior knowledge----------------------------------------------------------
kn <- knowledge(
  tpc_example,
  tier(
    child ~ c("child_x1", "child_x2"),
    youth ~ starts_with("youth"), # tidyselect helper; equivalent to c("youth_x3", "youth_x4")
    oldage ~ starts_with("oldage")
  )
)

## ----view knowledge-----------------------------------------------------------
print(kn)
summary(kn)
plot(kn, main = "Temporal Knowledge")

## ----tpc algorithm with knowledge---------------------------------------------
tpc_method <- tpc(
  engine = "causalDisco", # Use the causalDisco implementation
  test = "reg" # Use the regression-based information loss test
)
tpc_result <- disco(data = tpc_example, method = tpc_method, knowledge = kn)

## ----view tpc results---------------------------------------------------------
print(tpc_result)
summary(tpc_result)
plot(tpc_result, main = "TPC reg_test with Temporal Knowledge (causalDisco)")

