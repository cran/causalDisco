## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(causalDisco)

## ----dag----------------------------------------------------------------------
cg <- caugi::caugi(
    Z %-->% X1,
    X3 %-->% X2,
    X1 %-->% Y,
    X2 %-->% Y
)

## ----plot dag-----------------------------------------------------------------
layout <- caugi::caugi_layout_sugiyama(cg)
plot(cg, layout = layout, main = "True DAG")

## ----simple causal discovery--------------------------------------------------
data_linear <- generate_dag_data(
  cg,
  n = 10000,
  seed = 1405,
  coef_range = c(0.1, 0.9),
  error_sd = c(0.3, 2)
)
head(data_linear)

## ----generating model---------------------------------------------------------
attr(data_linear, "generating_model")

## ----pc algorithm simple------------------------------------------------------
pc_pcalg <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
pc_result_pcalg <- disco(data = data_linear, method = pc_pcalg)

## ----plot pc results simple---------------------------------------------------
plot(pc_result_pcalg, layout = layout, main = "PC (pcalg)")

## ----pc algorithm reversed----------------------------------------------------
cg_reverse <- caugi::caugi(
  Z %-->% X1,
  X2 %-->% X3,
  X1 %-->% Y,
  X2 %-->% Y
)

data_linear_reverse <- generate_dag_data(
  cg_reverse,
  n = 10000,
  seed = 1405,
  coef_range = c(0.1, 0.9),
  error_sd = c(0.3, 2)
)

pc_result_reverse <- disco(data = data_linear_reverse, method = pc_pcalg)
plot(pc_result_reverse, layout = layout, main = "PC (pcalg) reversed")

## ----dag unobserved confounder------------------------------------------------
cg_unobserved <- caugi::caugi(
  Z %-->% X1,
  X3 %-->% X2,
  X1 %-->% Y,
  X2 %-->% Y,
  U %-->% X1 + X2
)

## ----plot dag unobserved confounder-------------------------------------------
plot(
  cg_unobserved,
  edge_style = list(
    by_edge = list(
      U = list(col = "red", fill = "red", lty = "dashed")
    )
  ),
  node_style = list(
    by_node = list(
      U = list(col = "red", fill = "red")
    )
  ),
  main = "True DAG"
)

## ----data unobserved confounder-----------------------------------------------
data_unobserved <- generate_dag_data(
  cg_unobserved,
  n = 10000,
  seed = 1405,
  coef_range = c(0.1, 0.9),
  error_sd = c(0.3, 2)
)
data_unobserved <- data_unobserved[, names(data_unobserved) != "U"]
head(data_unobserved)

## ----pc algorithm unobserved confounder---------------------------------------
pc_pcalg_unobserved <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
pc_result_unobserved <- disco(data = data_unobserved, method = pc_pcalg_unobserved)
plot(pc_result_unobserved, layout = layout, main = "PC (pcalg)")

