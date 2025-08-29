library(magrittr)
library(socmod)


#---------UNBOUNDED-FORMAL-----------
discrete_unbounded_contagion <- function(tvec, alpha, delta) {}

continuous_unbounded_contagion <- function(tvec, alpha, delta) {}

#---test alternative contagion formulation----


#---------UNBOUNDED-SIMULATION-----------
# Approximate for small t and very large population, i.e., t << N
make_unbounded_contagion_abm <- function(N = 200, initial_prevalence = 0.01, 
                               alpha = 0.2, delta = 0.1) {
  print(delta)
  # Initialize ABM...
  unbounded_abm <- 
    make_abm(n_agents = N, learning_strategy = contagion_learning_strategy,
             initial_prevalence = initial_prevalence, 
             adoption_rate = alpha, drop_rate = delta) %>%
    # Initialize given prevalence with Adaptive behavior
    initialize_agents(initial_prevalence = initial_prevalence) 

  print(unbounded_abm$get_parameters()$as_list())

  return (unbounded_abm)
}


test_one_unbounded <- function (N = 20, initial_prevalence = 0.5, alpha = 0.5, delta = 0.0) {
  print(delta)
  abm <- make_unbounded_contagion_abm(N, initial_prevalence, alpha, delta)
  trial <- run_trial(abm, stop = fixated)
  p <- plot_prevalence(trial, tracked_behaviors = c("Adaptive"))

  print(p)
}
