

##########################################################################
#model of SEIR-HQD -population movement
#codes for study ""
##########################################################################

#calculation transmissibility 
#The transmissibility codes are adapted from https://github.com/kieshaprem/covid19-agestructureSEIR-wuhan-social-distancing
calculate_transimissibility = function(R0t,recov.rate,pop_age_structure,calculate_transmission_probability=1,contact.matrix)
{

  n = length(pop_age_structure) #length(age groups)
  constraints_base = list(home = diag(1,n),
                          work = diag(1,n), 
                          school = diag(1,n), 
                          others = diag(1,n)) # constraints under a DO-NOTHING scenario
  
  contact.matrix.pop.adjust <- lapply(contact.matrix, function(x, pop_age_structure) (x + t(x)*((pop_age_structure)%*%t(1/pop_age_structure)))/2, pop_age_structure) # make sure contacts are reciprocal
  
  C = constraints_base[[1]]%*%contact.matrix.pop.adjust[[1]]+
    constraints_base[[2]]%*%contact.matrix.pop.adjust[[2]]+
    constraints_base[[3]]%*%contact.matrix.pop.adjust[[3]]+
    constraints_base[[4]]%*%contact.matrix.pop.adjust[[4]]
  
  
  if (calculate_transmission_probability==1){
    M = C
    for(i in 1:n)
    {
      for(j in 1:n){
        M[i,j] = C[i,j]*pop_age_structure[i]/pop_age_structure[j]
      }
    }
    eig = eigen(M)
    transmissibility = R0t*recov.rate/max(Re(eig$values))  # reverse engineer transmissibility from the R0 and gamma 
    
    transmissibility = transmissibility
  }else{
    transmissibility = 0.025#0.05
  }
  results = list(transmissibility)
  names(results) =c('transmissibility')
  return(results)
}



#==============================without intervention model==========================================
SEIHDQR_9age_simulation_no_intervention_model <- function(pars = NULL,constraint = NULL, init = NULL, time = NULL, ...) {
  if (is.null(pars)) {
    stop("undefined 'pars'")
  }
  if (is.null(init)) {
    stop("undefined 'inits'")
  }
  if (is.null(time)) {
    stop("undefined 'time'")
  }
  
  function1 <- function(pars = NULL,constraint = NULL, init = NULL, time = NULL, outbreak_start_date = NULL, inter_start_date = NULL, inter_end_date = NULL) {
    function2 <- function(time, init, pars) {
      with(as.list(c(init, pars)), {
        
        dif.eq <- vector(length = 72)
        Ii <- c(eval(parse(text = 'I.1')), eval(parse(text = 'I.2')), eval(parse(text = 'I.3')), eval(parse(text = 'I.4')),eval(parse(text = 'I.5')), eval(parse(text = 'I.6')), eval(parse(text = 'I.7')), eval(parse(text = 'I.8')), eval(parse(text = 'I.9')))
        
        for (i in 1:9){
          agegroups<-9
          S <- eval(parse(text = paste('S.', i, sep='')))
          E <- eval(parse(text = paste('E.', i, sep='')))
          I <- eval(parse(text = paste('I.', i, sep='')))
          H <- eval(parse(text = paste('H.', i, sep='')))
          D <- eval(parse(text = paste('D.', i, sep='')))
          Q <- eval(parse(text = paste('Q.', i, sep='')))
          R <- eval(parse(text = paste('R.', i, sep='')))
          Ic <- eval(parse(text = paste('Ic.', i, sep='')))#cumulative infection for further data analysis
          #transmission.rate=transm.rate; 
          infection <- transm.rate[i,] %*% t(t(Ii))/n[i] * S #infection
          dif.eq[0 * agegroups + i] <- birth.rate[i] * sum(n)/agegroups - infection - death.rate[i] * S - quar.rate[i] * E #dS/dt
          dif.eq[1 * agegroups + i] <- infection - incub.rate[i] * E - death.rate[i] * E - quar.rate[i] * E #dE/dt
          dif.eq[2 * agegroups + i] <- incub.rate[i] * E - recov.rate[i] * I - death.rate[i] * I - fatal.rate[i] * I #dI/dt
          dif.eq[3 * agegroups + i] <- hosp.rate[i]* I - recov.rate[i] * H - fatal.rate[i] * H #dH/dt
          dif.eq[4 * agegroups + i] <- fatal.rate[i] * I #dD/dt  
          dif.eq[5 * agegroups + i] <-  I + quar.rate[i] * E - 1/14 * Q - death.rate[i] * Q#dQ/dt
          dif.eq[6 * agegroups + i] <- recov.rate[i] * I - death.rate[i] * R #dR/dt
          dif.eq[7 * agegroups + i] <- incub.rate[i] * E #dIc/dt 
        }
        list(dif.eq)
      })
    }
    
    output <- ode(times = time, 
                  func = function2, 
                  y = init, parms = pars, ...) 
    return(output)
    
  }
  
  
  
  output_without_intervention <- cbind.data.frame(time = 0,
                                                  data.frame(t(vector(72, mode='numeric'))));
  
  names(output_without_intervention)[2:73] <- names(init);
  
  T0 <- min(time)
  T0_max <- max(time)
  
  while (T0 < max(time)) {
    
    output_without_intervention <- rbind.data.frame(output_without_intervention,
                                                    function1(pars = pars, init = init,
                                                              time = seq(T0, T0 + T0_max,
                                                                         time[2] - time[1])))
    output_without_intervention$period <- "without_intervention"
    
    T0 <- T0 + T0_max + time[2] - time[1]
    
  };
  
  
  return(output_without_intervention[-1,])
}



#==============================intervention model==========================================

SEIHDQR_9age_simulation_6stage_intervention_model <- function(pars = NULL,constraint = NULL, init = NULL, time = NULL, outbreak_start_date = NULL, inter_start_date1 = NULL, inter_start_date2 = NULL, inter_start_date3 = NULL, inter_start_date4 = NULL,inter_end_date4 = NULL, constraint1 = NULL,constraint2 = NULL,constraint3 = NULL,constraint4 = NULL,constraint5 = NULL,constraint6 = NULL, ...) {
  if (is.null(pars)) {
    stop("undefined 'pars'")
  }
  if (is.null(init)) {
    stop("undefined 'inits'")
  }
  if (is.null(time)) {
    stop("undefined 'time'")
  }
  
  function1 <- function(pars = NULL,constraint = NULL, init = NULL, time = NULL) {
    function2 <- function(time, init, pars) {
      with(as.list(c(init, pars)), {
        
        dif.eq <- vector(length = 72)
        Ii <- c(eval(parse(text = 'I.1')), eval(parse(text = 'I.2')), eval(parse(text = 'I.3')), eval(parse(text = 'I.4')),eval(parse(text = 'I.5')), eval(parse(text = 'I.6')), eval(parse(text = 'I.7')), eval(parse(text = 'I.8')), eval(parse(text = 'I.9')))
        Hi <- c(eval(parse(text = 'H.1')), eval(parse(text = 'H.2')), eval(parse(text = 'H.3')), eval(parse(text = 'H.4')),eval(parse(text = 'H.5')), eval(parse(text = 'H.6')), eval(parse(text = 'H.7')), eval(parse(text = 'H.8')), eval(parse(text = 'H.9')))
        Qi <- c(eval(parse(text = 'Q.1')), eval(parse(text = 'Q.2')), eval(parse(text = 'Q.3')), eval(parse(text = 'Q.4')),eval(parse(text = 'Q.5')), eval(parse(text = 'Q.6')), eval(parse(text = 'Q.7')), eval(parse(text = 'Q.8')), eval(parse(text = 'Q.9')))
        for (i in 1:9){
          agegroups<-9
          S <- eval(parse(text = paste('S.', i, sep='')))
          E <- eval(parse(text = paste('E.', i, sep='')))
          I <- eval(parse(text = paste('I.', i, sep='')))
          H <- eval(parse(text = paste('H.', i, sep='')))
          D <- eval(parse(text = paste('D.', i, sep='')))
          Q <- eval(parse(text = paste('Q.', i, sep='')))
          R <- eval(parse(text = paste('R.', i, sep='')))
          Ic <- eval(parse(text = paste('Ic.', i, sep='')))#cumulative infection
          #transmission.rate=transm.rate; 
          infection <- transm.rate[i,] %*% t(t(Ii-Hi-Qi))/n[i] * S
          
          dif.eq[0 * agegroups + i] <- birth.rate[i] * sum(n)/agegroups - infection - death.rate[i] * S  #dS/dt
          dif.eq[1 * agegroups + i] <- infection - incub.rate[i] * E - death.rate[i] * E  #dE/dt
          dif.eq[2 * agegroups + i] <- incub.rate[i] * E   - recov.rate[i] * I - death.rate[i] * I - fatal.rate[i] * I #dI/dt
          dif.eq[3 * agegroups + i] <- hosp.rate[i] * (I - H) - fatal.rate[i] * H - recov.rate[i] * H - death.rate[i] * H #dH/dt
          dif.eq[4 * agegroups + i] <- fatal.rate[i] * I  #dD/dt  
          dif.eq[5 * agegroups + i] <- quar.rate[i] * (I - Q - H) -hosp.rate[i] *Q - recov.rate[i] * Q - death.rate[i]*Q - fatal.rate[i] * Q #dQ/dt        
          dif.eq[6 * agegroups + i] <- recov.rate[i] * I - death.rate[i] * R #dR/dt
          dif.eq[7 * agegroups + i] <- incub.rate[i] * E #dIc/dt  
          
        }
        list(dif.eq)
      })
    }
    
    output <- ode(times = time, 
                  func = function2, 
                  y = init, parms = pars, ...) 
    return(output)
    
  }

  time_before_intervention1 <- c(0:as.vector(inter_start_date1 - outbreak_start_date-1))
  time_during_intervention1 <- c(as.vector(inter_start_date1-outbreak_start_date): as.vector(inter_start_date2-outbreak_start_date-1))#we assume one day time lag of intervention came into effect
  time_during_intervention2 <- c(as.vector(inter_start_date2-outbreak_start_date): as.vector(inter_start_date3-outbreak_start_date-1))
  time_during_intervention3 <- c(as.vector(inter_start_date3-outbreak_start_date): as.vector(inter_start_date4-outbreak_start_date-1))
  time_during_intervention4 <- c(as.vector(inter_start_date4-outbreak_start_date): as.vector(inter_end_date4-outbreak_start_date))  
  time_after_intervention4 <- c(c(as.vector(inter_end_date4-outbreak_start_date+1)):max(time))  
  
  
  output_before_intervention1 <- cbind.data.frame(time = 0,
                                                  data.frame(t(vector(72, mode='numeric'))));
  output_during_intervention1 <- cbind.data.frame(time = 0, 
                                                  data.frame(t(vector(72, mode='numeric'))));
  output_during_intervention2 <- cbind.data.frame(time = 0,
                                                  data.frame(t(vector(72, mode='numeric'))));
  output_during_intervention3 <- cbind.data.frame(time = 0,
                                                  data.frame(t(vector(72, mode='numeric'))));
  output_during_intervention4 <- cbind.data.frame(time = 0,
                                                  data.frame(t(vector(72, mode='numeric'))));
  
  output_after_intervention4 <- cbind.data.frame(time = 0,
                                                 data.frame(t(vector(72, mode='numeric'))));
  
  names(output_before_intervention1)[2:73] <- names(init);#stage 1
  names(output_during_intervention1)[2:73] <- names(init);#stage 2
  names(output_during_intervention2)[2:73] <- names(init);#stage 3
  names(output_during_intervention3)[2:73] <- names(init);#stage 4
  names(output_during_intervention4)[2:73] <- names(init);#stage 5
  names(output_after_intervention4)[2:73] <- names(init);#stage 6
  
  
  T1 <- min(time_before_intervention1)
  T1_max <-max(time_before_intervention1)
  
  
  output_before_intervention1 <- rbind.data.frame(output_before_intervention1,
                                                  function1(pars = Map("*",pars, constraint1), init = init,#add constraint 1
                                                            time = seq(T1, T1_max,
                                                                       time[2] - time[1])))
  output_before_intervention1$period <- "before_intervention1";
  
  
  T2 <- min(time_during_intervention1)
  T2_max <- max(time_during_intervention1)
  
  
  output_during_intervention1 <- rbind.data.frame(output_during_intervention1,
                                                  function1(pars = Map("*",pars, constraint2), init = unlist(tail(output_before_intervention1,1)[2:73]), #add constraint 2
                                                            time = seq(T2-1, T2_max,
                                                                       time[2] - time[1]))) #move one day forward since the simulation take the initial from last period as the first estimate in this period
  output_during_intervention1$period <- "during_intervention1";
  
  
  T3 <- min(time_during_intervention2)
  T3_max <-max(time_during_intervention2)
  
  output_during_intervention2 <- rbind.data.frame(output_during_intervention2,
                                                  function1(pars = Map("*",pars, constraint3), init = unlist(tail(output_during_intervention1,1)[2:73]),#add constraint 3
                                                            time = seq(T3-1, T3_max,
                                                                       time[2] - time[1])))
  output_during_intervention2$period <- "during_intervention2"
  
  
  
  T4 <- min(time_during_intervention3)
  T4_max <-max(time_during_intervention3)
  
  output_during_intervention3 <- rbind.data.frame(output_during_intervention3,
                                                  function1(pars = Map("*", pars, constraint4), init =unlist(tail(output_during_intervention2,1)[2:73]),time = seq(T4-1, T4_max, time[2] - time[1])))#add constraint 4
  output_during_intervention3$period <- "during_intervention3"
  
  
  
  
  T5 <- min(time_during_intervention4)
  T5_max <-max(time_during_intervention4)
  
  output_during_intervention4 <- rbind.data.frame(output_during_intervention4,
                                                  function1(pars = Map("*", pars, constraint5), init =unlist(tail(output_during_intervention3,1)[2:73]), time = seq(T5-1, T5_max, time[2] - time[1])))#add constraint 5
  output_during_intervention4$period <- "during_intervention4"
  
  

  
  T6 <- min(time_after_intervention4)
  T6_max <-max(time_after_intervention4)
  
  output_after_intervention4 <- rbind.data.frame(output_after_intervention4,
                                                 function1(pars = Map("*", pars, constraint6), init =unlist(tail(output_during_intervention4,1)[2:73]), time = seq(T6-1, T6_max,
                                                                                                                                                                   time[2] - time[1])))
  output_after_intervention4$period <- "after_intervention4"
  
  return(rbind(output_before_intervention1[-1,],output_during_intervention1[-c(1:2),],output_during_intervention2[-c(1:2),],output_during_intervention3[-c(1:2),],output_during_intervention4[-c(1:2),],output_after_intervention4[-c(1:2),]))
}