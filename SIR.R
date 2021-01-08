library(shinySIR)


# Sistema de ODEs para o modelo SI

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

# SIR

run_shiny(model="SIR", 
          ics = c(S = 9999, I = 1, R = 0),
          parm0 = c(R0 = 2.5, Ip = 5),
          parm_names=c("Ro","Infectious period"),
          parm_min = c(R0 = 0.1, Ip = 1),
          parm_max = c(R0 = 10, Ip = 20))


# SIR with Ro<1 and 1000 infected individuals at time the start


run_shiny(model="SIR", 
          ics = c(S = 9000, I = 1000, R = 0),
          parm0 = c(R0 = 0.9, Ip = 5),
          parm_names=c("Ro","Infectious period"),
          parm_min = c(R0 = 0.1, Ip = 1),
          parm_max = c(R0 = 10, Ip = 20))


