library(igraph)
library(socmod)
library(purrr)

# Model generator for run_trials
cooperation_abm_gen <- function(params) {
  return (
    make_cooperation_model(
      params$grid_height, params$grid_width, params$coop_benefit, 
      params$coop_cost, params$disaster_debit, params$migration_rate
    )
  )
}

# Model step used to specify the LearningStrategy 
coop_model_step <- function(abm) {
  
  # Each agent plays game, receiving a total fitness from playing with each neighbor
  for (agent in abm$agents) {
    play_game_with_neighbors(agent, abm)
  }
  
  # After each agent plays, iterate through again to do success-biased learning,
  # but rescaling to deal with potentially negative fitness values
  purrr::walk(
    abm$agents,
    \(agent) {
      # Get focal agent's neighbors
      neighbors <- agent$get_neighbors()
      
      # Calculate unscaled weights
      weights <- purrr::map_vec(
        unname(neighbors$agents), \(n) n$fitness_current
      )
      
      max_idx <- which.max(weights)
      
      teacher <- neighbors$agents[[max_idx]]
      # Only use teacher's behavior if they have a higher fitness
      if (teacher$fitness_current > agent$fitness_current) {
        agent$set_next_behavior(teacher$behavior_current)
      }
      
      # Agent-level fitness resets after each round (i.e. time step in this case)
      agent$set_next_fitness(0.0)
    }
  )
  
  # Use socmod-provided model step function for learning given next_behavior/fitness
  iterate_learning_model(abm)
  # After interaction and learning, agents migrate
  mu <- abm$get_parameter("migration_rate")
  if (!is.null(mu) && (mu > 0.0)) {
    migration(abm)
  }
}


# For this normal game in this format we don't have individual partner selection 
# and interaction steps. Instead all is handled in the model_step, 
# i.e., coop_model_step defined below
coop_game_strategy <- make_learning_strategy(
  partner_selection = \(f, m) NULL,
  interaction = \(f, p, m) NULL,
  model_step = coop_model_step,
  label = "Normal game strategy"
)

make_cooperation_model <- function(grid_height = 11, grid_width = 11, 
                                   coop_benefit = 1.0, coop_cost = 0.2,
                                   disaster_debit = 0.0, migration_rate = 0.0) {
  # Set up spatial grid
  g <- make_lattice(
    dimvector = c(grid_height, grid_width), periodic = FALSE
  )
  # Define payoff matrix: row 1 is for focal cooperator, row 2 focal defector
  payoff_matrix <- matrix(
    c(coop_benefit - coop_cost,  -1.0 * coop_cost, 
      coop_benefit,              -1.0 * disaster_debit),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      "Focal" = c("Cooperate", "Defect"),
      "Partner" = c("Cooperate", "Defect")
    )
  )
  # Initialized like Smaldino _MSB_ Ch. 6
  abm <- 
    make_abm(
      graph = g, 
      coop_benefit = coop_benefit, 
      coop_cost = coop_cost,
      payoff_matrix = payoff_matrix,
      migration_rate = migration_rate,
      disaster_debit = disaster_debit,
      learning_strategy = coop_game_strategy
    ) %>% 
    initialize_agents(initial_prevalence = 0.5,  # initial_prevalence_adaptive 
                      adaptive_behavior = "Cooperate",
                      legacy_behavior = "Defect")
  
  # There's a bug in initialize_agents: fitness_next and behavior_next are not set,
  # which causes a problem in the model step in this model that would end up setting
  # some agents to do the "Legacy" behavior instead of 
  purrr::walk(abm$agents, \(a) a$set_next_behavior(a$behavior_current))
  
  return (abm)
}


# Two individuals play game with one another; b = coop_benefit, c = coop_cost
play_game <- function(focal_agent, partner_agent, model) {

  return (
    model$get_parameter("payoff_matrix")[
      focal_agent$behavior_current,
      partner_agent$behavior_current
    ]
  )
}


# Have the focal_agent play the coordination game with all neighbors
play_game_with_neighbors <- function(focal_agent, model) {
  # Focal agent plays game with neighbors, accumulating payoffs
  total_payoff <- 
    sum(purrr::map_vec(
      focal_agent$get_neighbors()$agents,
      # Define an *anonymous function* to apply to each neighbor
      \(neighbor) play_game(focal_agent, neighbor, model)
      )
    )
  # Focal agent's fitness gets set to the total_payoff
  focal_agent$fitness_current <- total_payoff
}



# Assumes that there are an even number of agents migrating
# and that they don't care where they migrate to as long 
# as they migrate.
migration <- function(abm) {
  # First get half of migrators in group called migrators_1
  # First see how many half of the total migrators size is
  N <- abm$get_parameter("n_agents") 
  mu <- abm$get_parameter("migration_rate")
  half_migrators_size <- round(0.5 * N * mu)

  # Then randomly select that number of agents to migrate
  all_ids <- 1:N
  migrators_ids_1 <- sample(all_ids, half_migrators_size)
  not_migrated <- setdiff(all_ids, migrators_ids_1)
  migrators_ids_2 <- sample(not_migrated, half_migrators_size)
  
  walk2(
    migrators_ids_1, migrators_ids_2, 
    \(mid1, mid2) {
      # print(
      #   paste("::: Exchanging agents with ids", mid1, "and", mid2, ":::")
      # )
      a1 <- abm$agents[[mid1]]
      a2 <- abm$agents[[mid2]]
      # Only have to exchange behaviors since the fitness is zero for all agents
      # following the model step...
      # ...first copy a1's behaviors before overwriting
      a1_behavior_current_sync <- a1$behavior_current
      a1_behavior_next_sync <- a1$get_next_behavior()
      # ...then overwrite a1's behavior with a2's
      a1$behavior_current <- a2$behavior_current
      a1$set_next_behavior(a2$get_next_behavior())
      # ...and assign a2's behaviors to be those stored from a1.
      a2$behavior_current <- a1_behavior_current_sync
      a2$set_next_behavior(a1_behavior_next_sync)
    }
  )
}

# Check migration is working OK
abm <- make_cooperation_model(migration_rate = 0.1)
migration(abm)
