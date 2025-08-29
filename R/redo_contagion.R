library(socmod)

# Load ggplot2 to avoid writing it during plotting below.
library(ggplot2)
# Model generation function used in both computational experiments below.
gen <- function(model_parameter_row) {
  
  # Extract adaptive_fitness to create agents.
  adaptive_fitness <- model_parameter_row$adaptive_fitness
  
  agent_1 <- socmod::Agent$new(1, behavior = "Legacy", 
                               fitness = 1.0, name = "a1")
  agent_2 <- socmod::Agent$new(2, behavior = "Adaptive", 
                               fitness = adaptive_fitness, name = "a2")
  agent_3 <- socmod::Agent$new(3, behavior = "Legacy", 
                               fitness = 1.0, name = "a3")
  agent_4 <- socmod::Agent$new(4, behavior = "Legacy", 
                               fitness = 1.0, name = "a4")
  
  agents <- list(agent_1, agent_2, agent_3, agent_4)
  graph <- igraph::make_graph(~ 1-2, 1-3, 1-4, 3-2)
  
  # Extract other necessary model parameters.
  learning_strategy <- model_parameter_row$learning_strategy
  drop_rate <- model_parameter_row$drop_rate
  adoption_rate <- model_parameter_row$adoption_rate
  
  # Make ModelParameters to encapsulate this model's parameters.
  model_parameters <- socmod::make_model_parameters(
    learning_strategy, graph, adaptive_fitness = adaptive_fitness, 
    adoption_rate = adoption_rate, drop_rate = drop_rate
  )
  
  return (
    socmod::make_abm(
      model_parameters,
      agents = agents
    )
  )
}

if (!("trials_adoption" %in% ls(all.names = TRUE))) {
  trials_adoption <- socmod::run_trials(
    gen,
    n_trials_per_param = 10,
    stop = socmod::fixated, 
    syncfile = "trials-adoption-rate.RData",
    overwrite = TRUE,
    learning_strategy = c(socmod::success_bias_learning_strategy,
                          socmod::frequency_bias_learning_strategy,
                          socmod::contagion_learning_strategy),
    adaptive_fitness = 1.4,
    adoption_rate = c(0.05, 0.2, 0.4, 0.6, 0.8, 1.0),
    drop_rate = 0.2
  )
}
trials_summary <- socmod::summarise_by_parameters(
  trials_adoption, c("learning_strategy", "adoption_rate")
)

trials_success_rate <- dplyr::filter(
  trials_summary, Measure == "success_rate"
)