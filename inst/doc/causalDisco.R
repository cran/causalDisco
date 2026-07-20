## -----------------------------------------------------------------------------
#| label: setup

library(causalDisco)


## -----------------------------------------------------------------------------
#| label: load-data

data(num_data)
head(num_data)


## -----------------------------------------------------------------------------
#| label: plot-layout

plot_layout <- data.frame(
  name = c("Z", "X3", "X1", "X2", "Y"),
  x = c(0.00, 0.50, 0.00, 0.50, 0.25),
  y = c(0.0, 0.0, 0.5, 0.5, 1.0)
)


## -----------------------------------------------------------------------------
#| label: pc-algorithm-fisher-z-bnlearn

pc_bnlearn <- pc(
  engine = "bnlearn", # Use the bnlearn implementation
  test = "fisher_z", # Use Fisher's Z test for conditional independence
  alpha = 0.05 # Significance level for the test
)
pc_result_bnlearn <- disco(data = num_data, method = pc_bnlearn)


## -----------------------------------------------------------------------------
#| label: pc-algorithm-fisher-z-bnlearn-plot

plot(pc_result_bnlearn, layout = plot_layout, main = "PC Fisher Z (bnlearn)")


## -----------------------------------------------------------------------------
#| label: pc-algorithm-fisher-z-pcalg

pc_pcalg <- pc(
  engine = "pcalg", # Use the pcalg implementation
  test = "fisher_z", # Use Fisher's Z test for conditional independence
  alpha = 0.05 # Significance level for the test
)
pc_result_pcalg <- disco(data = num_data, method = pc_pcalg)
plot(pc_result_pcalg, layout = plot_layout, main = "PC Fisher Z (pcalg)")


## -----------------------------------------------------------------------------
#| label: install-rJavaEnv
#| eval: false
# # Use the development version of rJavaEnv from GitHub
# # pak::pak("e-kotov/rJavaEnv")
# rJavaEnv::java_quick_install(version = 25, distribution = "Temurin")


## -----------------------------------------------------------------------------
#| label: install-tetrad
#| eval: false
# install_tetrad()


## -----------------------------------------------------------------------------
#| label: check-tetrad-install
verify_tetrad()


## -----------------------------------------------------------------------------
#| label: ges-algorithm-ebic-tetrad

if (verify_tetrad()$installed && verify_tetrad()$java_ok) {
  ges_tetrad <- ges(
    engine = "tetrad", # Use the Tetrad implementation
    score = "ebic" # Use the EBIC score
  )
  ges_result_tetrad <- disco(data = num_data, method = ges_tetrad)
  plot(ges_result_tetrad, layout = plot_layout, main = "GES EBIC (Tetrad)")
}


## -----------------------------------------------------------------------------
#| label: custom-plot

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


## -----------------------------------------------------------------------------
#| label: view-results

print(pc_result_bnlearn)


## -----------------------------------------------------------------------------
#| label: load-tpc-example-data

data(tpc_example)
head(tpc_example)


## -----------------------------------------------------------------------------
#| label: prior-knowledge

kn <- knowledge(
  tpc_example,
  tier(
    child ~ c("child_x1", "child_x2"),
    youth ~ starts_with("youth"), # tidyselect helper; equivalent to c("youth_x3", "youth_x4")
    oldage ~ starts_with("oldage")
  )
)


## -----------------------------------------------------------------------------
#| label: view-knowledge

print(kn)
summary(kn)
plot(kn, main = "Temporal Knowledge")


## -----------------------------------------------------------------------------
#| label: tpc-algorithm-with-knowledge

tpc_method <- tpc(
  engine = "causalDisco", # Use the causalDisco implementation
  test = "reg" # Use the regression-based information loss test
)
tpc_result <- disco(data = tpc_example, method = tpc_method, knowledge = kn)


## -----------------------------------------------------------------------------
#| label: view-tpc-results

print(tpc_result)
summary(tpc_result)
plot(tpc_result, main = "TPC reg_test with Temporal Knowledge (causalDisco)")

