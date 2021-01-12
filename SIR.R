library(shinySIR)


# 1. Sistema de ODEs para o modelo SI

mySI <- function(t, y, parms) {
  
  with(as.list(c(y, parms)),{
    
    # Change in Susceptibles
    dS <- - beta * S * I
    
    # Change in Infecteds
    dI <- beta * S * I
    
    return(list(c(dS, dI)))
  })
}

run_shiny(model = "SI", 
          neweqns = mySI,
          ics = c(S = 9999, I = 1),
          parm0 = c(beta = 5e-5),
          parm_names = c("Transmission rate"),
          parm_min = c(beta = 5e-6),
          parm_max = c(beta = 5e-4))

# 2. SIR with transmission rate and Infectious period

mySIR <- function(t, y, parms) {
  
  with(as.list(c(y, parms)),{
    
    # Change in Susceptibles
    dS <- - beta * S * I
    
    # Change in Infecteds
    dI <- beta * S * I - (1/gamma) * I
    
    dR <- (1/gamma) * I
    
    return(list(c(dS, dI,dR)))
  })
}

run_shiny(model="SIR", 
          neweqns = mySIR,
          ics = c(S = 9999, I = 1, R = 0),
          parm0 = c(beta = 0.00001, gamma = 18),
          parm_names=c("transmission rate","Infectious period"),
          parm_min = c(beta = 0.00001, gamma = 0.1),
          parm_max = c(beta = 0.00030, gamma = 20),showtable = F)


# 3. SIR with Ro and Infectious period

run_shiny(model="SIR", 
          ics = c(S = 9999, I = 1, R = 0),
          parm0 = c(R0 = 2.5, Ip = 5),
          parm_names=c("Ro","Infectious period"),
          parm_min = c(R0 = 0.1, Ip = 1),
          parm_max = c(R0 = 10, Ip = 20))


# 3.1 SIR with Ro<1 and 1000 infected individuals at time the start


run_shiny(model="SIR", 
          ics = c(S = 9000, I = 1000, R = 0),
          parm0 = c(R0 = 0.9, Ip = 5),
          parm_names=c("Ro","Infectious period"),
          parm_min = c(R0 = 0.1, Ip = 1),
          parm_max = c(R0 = 10, Ip = 20))


# 4. Obtaining the Ro from the final Attack Rate

final_attack_rate = 0.5

R0::est.R0.AR(AR=final_attack_rate)

