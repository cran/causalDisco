## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(causalDisco)

## ----knowledge plot-----------------------------------------------------------
data(num_data)
kn <- knowledge(
  num_data,
  X1 %-->% c(X2, X3), # Require edge from X1 to X2, and X1 to X3
  X2 %!-->% c(X3, Y), # Forbid edge from X2 to X3, and X2 to Y
  Y %!-->% Z  # Forbid edge from Y to Z
)

plot(kn)

## ----knowledge plot different colors------------------------------------------
plot(kn, required_col = "skyblue", forbidden_col = "orange")

## ----knowledge plot custom styles---------------------------------------------
plot(
  kn,
  layout = "fruchterman-reingold",
  node_style = list(
    fill = "lightblue", # Fill color
    col = "darkblue", # Border color
    lwd = 2, # Border width
    padding = 4, # Text padding (mm)
    size = 1.2 # Size multiplier
  ),
  edge_style = list(
    lwd = 1.5, # Edge width
    arrow_size = 4 # Arrow size (mm)
  ),
  required_col = "blue", # Color for required edges
  forbidden_col = "red" # Color for forbidden edges
)

## ----knowledge plot edge by edge----------------------------------------------
plot(
  kn,
  layout = "fruchterman-reingold",
  node_style = list(
    fill = "lightblue", # Fill color
    col = "darkblue", # Border color
    lwd = 2, # Border width
    padding = 4, # Text padding (mm)
    size = 1.2 # Size multiplier
  ),
  edge_style = list(
    lwd = 1.5, # Edge width
    arrow_size = 4, # Arrow size (mm)
    # Per-edge overrides
    by_edge = list(
      X1 = list(
        X2 = list(col = "orange", fill = "orange", lwd = 3)
      ),
      X2 = list(
        Y = list(col = "yellow", fill = "yellow", lwd = 3)
      )
    )
  ),
  required_col = "blue", # Color for required edges
  forbidden_col = "red" # Color for forbidden edges
)

## -----------------------------------------------------------------------------
my_layout <- data.frame(
  name = c("X1", "X2", "X3", "Y", "Z"),
  x = c(1, 2, 3, 4, 5),
  y = c(1, 2, 1, 2, 1)
)
plot(kn, layout = my_layout)

## ----tiered knowledge plot----------------------------------------------------
data(tpc_example)
kn_tiered <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"), # tidyselect helper; equivalent to c("child_x1", "child_x2")
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  ),
  child_x1 %-->% child_x2,
  child_x2 %!-->% youth_x3
)
plot(kn_tiered)

## ----knowledgeable caugi plot-------------------------------------------------
data(num_data)
kn <- knowledge(
  num_data,
  X1 %-->% X2,
  X2 %!-->% c(X3, Y),
  Y %!-->% Z
)

pc_bnlearn <- pc(engine = "bnlearn", test = "fisher_z")
pc_result <- disco(num_data, method = pc_bnlearn, knowledge = kn)
plot(pc_result)

## ----knowledgeable caugi tiered plot------------------------------------------
data(tpc_example)
kn_tiered <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)
cd_tges <- tges(engine = "causalDisco", score = "tbic")
disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn_tiered)
plot(disco_cd_tges)

## ----tikz export knowledge----------------------------------------------------
data(num_data)
kn <- knowledge(
  num_data,
  X1 %-->% X2,
  X2 %!-->% c(X3, Y),
  Y %!-->% Z
)

# Full standalone document
tikz_knowledge_code <- make_tikz(kn, scale = 10, full_doc = TRUE)
cat(tikz_knowledge_code)

# Only the tikzpicture environment
tikz_knowledge_snippet <- make_tikz(kn, scale = 10, full_doc = FALSE)
cat(tikz_knowledge_snippet)

## ----tikz bend edges----------------------------------------------------------
data(tpc_example)
kn_tiered <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  ),
  child_x1 %-->% oldage_x5
)

plot(kn_tiered)

tikz_bent_tiered <- make_tikz(
  kn_tiered,
  scale = 10,
  full_doc = FALSE,
  bend_edges = TRUE,
  bend_angle = 20
)
cat(tikz_bent_tiered)

## ----tikz tier plot, echo=FALSE-----------------------------------------------
knitr::include_graphics("tikz-tier-plot.png")

## ----tikz export knowledgeable caugi------------------------------------------
data(tpc_example)
kn_tiered <- knowledge(
  tpc_example,
  tier(
    child ~ starts_with("child"),
    youth ~ starts_with("youth"),
    old ~ starts_with("old")
  )
)

tiers <- list(
  child = c("child_x1", "child_x2"),
  youth = c("youth_x3", "youth_x4"),
  old = c("oldage_x5", "oldage_x6")
)

cd_tges <- tges(engine = "causalDisco", score = "tbic")
disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn_tiered)

disco_plot <- plot(disco_cd_tges)
tikz_snippet <- make_tikz(disco_plot, tier_node_map = tiers, scale = 10, full_doc = FALSE)
cat(tikz_snippet)

## ----tikz export caugi--------------------------------------------------------
cg <- caugi::caugi(
  A %-->% B + C
)
plot_obj <- caugi::plot(cg, node_style = list(fill = "red"))
tikz_caugi_snippet <- make_tikz(plot_obj, scale = 10, full_doc = FALSE)
cat(tikz_caugi_snippet)

