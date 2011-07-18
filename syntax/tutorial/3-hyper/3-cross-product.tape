# TODO: Make this into a proper regression test and tutorial example

# Defining work plans to get more than just one-off experiments (this is intended primarily for config files)
# ducttape can be called with "ducttape -P Basics" to run cross products of branches by adding lines
# like the following to config files:
++plan Basics
  # 4 experiments/realizations: all combinations of small and medium
  # The * operator indicates a cross-product
  (whichTM: small, medium) * (whichLM: small, medium)
  # 1 experiment/realization: just the large model
  (whichLM: large) * (whichLM: large)
