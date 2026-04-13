## -----------------------------------------------------------------------------
#| label: setup
library(causalDisco)


## -----------------------------------------------------------------------------
#| label: required and forbidden knowledge
kn_1 <- knowledge(
  A %-->% c(B, C), # Require edges from A to B and A to C
  B %!-->% C # Forbid edge from B to C
)


## -----------------------------------------------------------------------------
#| label: plot required and forbidden knowledge
plot(kn_1)


## -----------------------------------------------------------------------------
#| label: remove required edge
kn_1_removed <- remove_edge(kn_1, from = A, to = B)
plot(kn_1_removed)


## -----------------------------------------------------------------------------
#| label: dataset required and forbidden knowledge
data(tpc_example)
head(tpc_example)


## -----------------------------------------------------------------------------
#| label: required and forbidden knowledge with data
kn_2 <- knowledge(
  tpc_example,
  child_x1 %-->% youth_x3, # Require edge from child_x1 to youth_x3
  child_x2 %!-->% oldage_x5 # Forbid edge from child_x2 to oldage_x5
)


## -----------------------------------------------------------------------------
#| label: plot required and forbidden knowledge with data
cg <- knowledge_to_caugi(kn_2)$caugi
layout <- caugi::caugi_layout_sugiyama(cg)
layout[6, 2] <- layout[4, 2]

plot(kn_2, layout = layout)


## -----------------------------------------------------------------------------
#| label: required and forbidden knowledge with tidyselect
kn_3 <- knowledge(
  tpc_example,
  starts_with("child") %-->% starts_with("youth"),
  starts_with("oldage") %!-->% starts_with("youth")
)


## -----------------------------------------------------------------------------
#| label: plot required and forbidden knowledge with tidyselect
plot(kn_3)


## -----------------------------------------------------------------------------
#| label: tier knowledge
kn <- knowledge(
  tier(
    1 ~ c(A1, A2),
    2 ~ c(B1, B2),
    3 ~ c(C1, C2)
  )
)

# Same object, since tiers are ordered numerically
kn_same <- knowledge(
  tier(
    1 ~ c(A1, A2),
    3 ~ c(C1, C2),
    2 ~ c(B1, B2)
  )
)

# Functionally equivalent, though not identical
kn_almost <- knowledge(
  tier(
    10 ~ c(A1, A2),
    30 ~ c(C1, C2),
    20 ~ c(B1, B2)
  )
)

# Again functionally equivalent
kn_also_almost <- knowledge(
  tier(
    A ~ c(A1, A2),
    B ~ c(B1, B2),
    C ~ c(C1, C2)
  )
)

# Has a letter, so tiers are ordered by appearance, thus functionally equivalent
kn_mixed <- knowledge(
  tier(
    3 ~ c(A1, A2),
    B ~ c(B1, B2),
    1 ~ c(C1, C2)
  )
)


## -----------------------------------------------------------------------------
#| label: plot tier knowledge
plot(kn)


## -----------------------------------------------------------------------------
#| label: convert tiers to forbidden
kn_converted <- convert_tiers_to_forbidden(kn)
print(kn_converted)
plot(kn_converted)


## -----------------------------------------------------------------------------
#| label: tier knowledge with tidyselect
kn_tier_tidyselect <- knowledge(
  tpc_example,
  tier(
    young ~ starts_with("child") + ends_with(c("3", "4")),
    old ~ starts_with("old")
  )
)
plot(kn_tier_tidyselect)


## -----------------------------------------------------------------------------
#| label: exogenous knowledge
kn_exo_1 <- knowledge(
  tpc_example,
  exogenous("child_x1")
)


## -----------------------------------------------------------------------------
#| label: plot exogenous knowledge
plot(kn_exo_1)


## -----------------------------------------------------------------------------
#| label: exogenous knowledge with tidyselect
kn_exo_2 <- knowledge(
  tpc_example,
  exo(starts_with("child"))
)
plot(kn_exo_2, layout = "bipartite", orientation = "columns")


## -----------------------------------------------------------------------------
#| label: combined knowledge
kn_combined <- knowledge(
  tpc_example,
  tier(
    1 ~ starts_with("child"),
    2 ~ starts_with("youth"),
    3 ~ starts_with("oldage")
  ),
  child_x1 %-->% youth_x3,
  child_x1 %!-->% child_x2
)

plot(kn_combined)


## -----------------------------------------------------------------------------
#| label: causal discovery with tier knowledge
kn <- knowledge(
  tpc_example,
  tier(
    1 ~ starts_with("child"),
    2 ~ starts_with("youth"),
    3 ~ starts_with("oldage")
  )
)

cd_tges <- tges(engine = "causalDisco", score = "tbic")
disco_cd_tges <- disco(data = tpc_example, method = cd_tges, knowledge = kn)


## -----------------------------------------------------------------------------
#| label: plot causal discovery with tier knowledge
plot(disco_cd_tges)


## -----------------------------------------------------------------------------
#| label: bnlearn
data(tpc_example)

kn <- knowledge(
  tpc_example,
  child_x1 %-->% youth_x3
)

bnlearn_pc <- pc(engine = "bnlearn", test = "fisher_z", alpha = 0.05)
output <- disco(data = tpc_example, method = bnlearn_pc, knowledge = kn)


## -----------------------------------------------------------------------------
#| label: plot bnlearn
plot(output)


## -----------------------------------------------------------------------------
#| label: pcalg
data(tpc_example)
kn <- knowledge(
  tpc_example,
  child_x1 %!-->% youth_x3,
  youth_x3 %!-->% child_x1
)
pc_pcalg <- pc(engine = "pcalg", test = "fisher_z", alpha = 0.05)
output <- disco(data = tpc_example, method = pc_pcalg, knowledge = kn)

