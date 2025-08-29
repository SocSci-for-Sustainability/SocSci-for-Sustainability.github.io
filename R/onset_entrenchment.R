

#' Calculate derivative of adoption of A over time for different a
adot_success <- function(a, s) {
  
  num <- s * a * (1 - a)
  denom <- 1 + (a * s)
  
  ret <- num / denom
  
  return (ret)
}

print(adot_success(a = 0.01, s = 0.5))

plot_adot_success <- function(amin = 0.0, amax = 1.0, s = 0.05, a_res = 0.01) {
  # s <- 0.2
  # a_res <- 0.05
  avec <- seq(amin, amax, a_res)
  
  adotvec <- adot_success(avec, s)
  
  plot(avec, adotvec)
}
plot_adot_success(amax = 0.5, s = 0.1)


plot_adot_conformity <- function(amin = 0.0, amax = 0.1, gamma = -0.1, a_res = 0.001) {

  avec <- seq(amin, amax, a_res)
  adotvec <- adot_conformity(avec, gamma)
  
  plot(avec, adotvec)
}
plot_adot_conformity()


#' Calculate derivative of adoption of A over time
adot_conformity <- function(a, gamma) {
  
  return (PA_conformity(a, gamma) - a)
}

#' Probability of adopting the A behavior due to conformity on a given time step
PA_conformity <- function(a, gamma) {
  
  conf_exponent <- 1 + gamma
  
  num <- a^conf_exponent
  denom <- num + ((1 - a)^conf_exponent)
  
  ret  <- num / denom
  
  return (ret)
}
