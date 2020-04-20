

##########################################################################
#model of scenario simulations using SEIR-HQD 
#codes for study "Impacts of Early Interventions on the Age-Specific Incidence of COVID-19 in New York, Los Angeles, Daegu and Nairobi"
##########################################################################
#=============================================model of scenario simulations using SEIR-HQD=============================================
#=============================================parameters preparation=============================================
#para 1
death.rate_Nairobi<-readRDS("death_rate_Nairobi.rds")
death.rate_Nairobi<-death.rate_Nairobi/365
death.rate_daegu<-readRDS("death_rate_daegu.rds")
death.rate_daegu<-death.rate_daegu/365
death.rate_nyc<-readRDS("death_rate_nyc.rds")
death.rate_nyc<-death.rate_nyc/365
death.rate_los_angeles<-readRDS("death_rate_los_angeles.rds")
death.rate_los_angeles<-death.rate_los_angeles/365
#para 2
birth.rate_Nairobi<-readRDS("birth_rate_Nairobi.rds")
birth.rate_daegu<-readRDS("birth_rate_daegu.rds")
birth.rate_nyc<-readRDS("birth_rate_nyc.rds")
birth.rate_los_angeles<-readRDS("birth_rate_los_angeles.rds")
#para 3
pop_age_structure<-fread("D:/Dropbox (Princeton)/Hao_project/Coronavirus-2019/pop_age_prop_by_city.csv") #population proportion by age'
pop_age_structure_nyc<-readRDS("pop_age_structure_nyc.rds")
pop_age_structure_los_angeles<-readRDS("pop_age_structure_los_angeles.rds")
pop_age_structure_Nairobi<-readRDS("pop_age_structure_Nairobi.rds")
pop_age_structure_daegu<-readRDS("pop_age_structure_daegu.rds")

#para 4
pop=1e+06 #population 

#para 5
incub.rate = c(rep(1-exp(-1/6.4),9))#incubation days are 6.4 The effect of control strategies to reduce social mixing on outcomes of the COVID-19 epidemic in Wuhan, China:a modelling study

#para 6
recov.day = c(7, 7, 7, 9, 9, 9, 11, 11, 11) #reference "Epidemiology and Transmission of COVID-19 in Shenzhen China: Analysis of 391 cases and 1,286 of their close contacts" and "Report of the WHO-China Joint Mission on Coronavirus Disease 2019 (COVID-19)" and "Incorporating Human Movement Data to Improve Epidemiological Estimates for 2019-nCoV"

recov.rate <- 1-exp(-1/recov.day)#The effect of control strategies to reduce social mixing on outcomes of the COVID-19 epidemic in Wuhan
saveRDS(recov.rate,"recov.rate.rds")

#para 7
R0t = c(rep(2.92,9)) # Liu et al.


#calculate transmissibility
contact_matrix_9age_kenya <- readRDS("contact_matrix_9age_kenya.rds")
#contact matrix without intervention
contact_matrix_9age_without_intervention_Nairobi <- list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work,contact_matrix_9age_kenya$other)
#contact matrix policy package 1: 100%*home+100%*school+60%*workplace+80%*others+5%*quarantine
contact.matrix_package1_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school*0,contact_matrix_9age_kenya$work,contact_matrix_9age_kenya$other)
#contact matrix policy package 2: 100%*home+100%*school+60%*workplace+40%*others+10%*quarantine
contact.matrix_package2_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work*0.5,contact_matrix_9age_kenya$other)
#contact matrix policy package 3: 100%*home+0%*school+30%*workplace+40%*others+10%quarantine
contact.matrix_package3_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work,contact_matrix_9age_kenya$other*0.5)
#contact matrix policy package 4: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine
contact.matrix_package4_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work,contact_matrix_9age_kenya$other)
#contact matrix policy package 5: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine
contact.matrix_package5_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school*0,contact_matrix_9age_kenya$work*0.5,contact_matrix_9age_kenya$other*0.5)
#contact matrix policy package 6: 80% working from home
contact.matrix_package6_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work*0.2,contact_matrix_9age_kenya$other)
#contact matrix policy package 7: 80% other
contact.matrix_package7_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work,contact_matrix_9age_kenya$other*0.2)

#contact matrix for the US
#contact matrix package

contact_matrix_9age<-readRDS("contact_matrix_9age.rds")
#contact matrix without intervention nyc
contact_matrix_9age_without_intervention_nyc <- list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work,contact_matrix_9age$other)
#contact matrix policy package 1: 100%*home+100%*school+100%*workplace+90%*others+0%quarantine
contact.matrix_package1_nyc<-list(contact_matrix_9age$home,contact_matrix_9age$school*0,contact_matrix_9age$work,contact_matrix_9age$other)
#contact matrix policy package 2: 100%*home+0%*school+95%*workplace+80%*others+5%quarantine
contact.matrix_package2_nyc<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work*0.5,contact_matrix_9age$other)
#contact matrix policy package 3: 100%*home+0%*school+20%*workplace+50%*others+30%quarantine
contact.matrix_package3_nyc<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work,contact_matrix_9age$other*0.5)
#contact matrix policy package 4: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine+95%transimissibility
contact.matrix_package4_nyc<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work,contact_matrix_9age$other)
#contact matrix policy package 5: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine+95%transimissibility
contact.matrix_package5_nyc<-list(contact_matrix_9age$home,contact_matrix_9age$school*0,contact_matrix_9age$work*0.5,contact_matrix_9age$other*0.5)
#contact matrix policy package 6:
contact.matrix_package6_nyc<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work*0.2,contact_matrix_9age$other)
#contact matrix policy package 7:
contact.matrix_package7_nyc<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work,contact_matrix_9age$other*0.2)


#contact matrix without intervention los angeles
contact_matrix_9age_without_intervention_los_angeles <-  list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work,contact_matrix_9age$other)
#contact matrix policy package 1: school closure
contact.matrix_package1_los_angeles<-list(contact_matrix_9age$home,contact_matrix_9age$school*0,contact_matrix_9age$work,contact_matrix_9age$other)
#contact matrix policy package 2: 50% working from home
contact.matrix_package2_los_angeles<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work*0.5,contact_matrix_9age$other)
#contact matrix policy package 3: 50% other
contact.matrix_package3_los_angeles<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work,contact_matrix_9age$other*0.5)
#contact matrix policy package 4: 10% increase of quarantine rate of infection
contact.matrix_package4_los_angeles<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work,contact_matrix_9age$other)
#contact matrix policy package 5: all policy together
contact.matrix_package5_los_angeles<-list(contact_matrix_9age$home,contact_matrix_9age$school*0,contact_matrix_9age$work*0.5,contact_matrix_9age$other*0.5)
#contact matrix policy package 6: 80% working from home
contact.matrix_package6_los_angeles<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work*0.2,contact_matrix_9age$other)
#contact matrix policy package 7: 80% other
contact.matrix_package7_los_angeles<-list(contact_matrix_9age$home,contact_matrix_9age$school,contact_matrix_9age$work,contact_matrix_9age$other*0.2)
#contact matrix policy package 8: strict social distancing
contact.matrix_package8_los_angeles<-list(contact_matrix_9age$home,contact_matrix_9age$school*0,contact_matrix_9age$work*0.2,contact_matrix_9age$other*0.2)

#contact matrix package korea

contact_matrix_9age_korea <- readRDS("contact_matrix_9age_korea.rds")
#contact matrix without intervention
contact_matrix_9age_without_intervention_daegu <- list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school,contact_matrix_9age_korea$work,contact_matrix_9age_korea$other)
#contact matrix policy package 1: 100%*home+100%*school+60%*workplace+80%*others+5%*quarantine
contact.matrix_package1_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school*0,contact_matrix_9age_korea$work,contact_matrix_9age_korea$other)
#contact matrix policy package 2: 100%*home+100%*school+60%*workplace+40%*others+10%*quarantine
contact.matrix_package2_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school,contact_matrix_9age_korea$work*0.5,contact_matrix_9age_korea$other)
#contact matrix policy package 3: 100%*home+0%*school+30%*workplace+40%*others+10%quarantine
contact.matrix_package3_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school,contact_matrix_9age_korea$work,contact_matrix_9age_korea$other*0.5)
#contact matrix policy package 4: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine
contact.matrix_package4_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school,contact_matrix_9age_korea$work,contact_matrix_9age_korea$other)
#contact matrix policy package 5: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine
contact.matrix_package5_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school*0,contact_matrix_9age_korea$work*0.5,contact_matrix_9age_korea$other*0.5)
#contact matrix policy package 6: 80% working from home
contact.matrix_package6_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school,contact_matrix_9age_korea$work*0.2,contact_matrix_9age_korea$other)
#contact matrix policy package 7: 80% reduction in other
contact.matrix_package7_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school,contact_matrix_9age_korea$work,contact_matrix_9age_korea$other*0.2)
#contact matrix policy package 8: 80% reduction in other
contact.matrix_package8_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school*0,contact_matrix_9age_korea$work*0.2,contact_matrix_9age_korea$other*0.2)


#transmissibility
transmissibility_9age_Nairobi <- calculate_transimissibility(R0t = R0t, recov.rate = recov.rate, pop_age_structure = pop_age_structure_Nairobi, calculate_transmission_probability = 1, contact.matrix = contact_matrix_9age_without_intervention_Nairobi)

transmissibility_9age_daegu <- calculate_transimissibility(R0t = R0t, recov.rate = recov.rate, pop_age_structure = pop_age_structure_daegu, calculate_transmission_probability = 1, contact.matrix = contact_matrix_9age_without_intervention_daegu)

transmissibility_9age_nyc <- calculate_transimissibility(R0t = R0t, recov.rate = recov.rate, pop_age_structure = pop_age_structure_nyc, calculate_transmission_probability = 1, contact.matrix = contact_matrix_9age_without_intervention_nyc)

transmissibility_9age_los_angeles <- calculate_transimissibility(R0t = R0t, recov.rate = recov.rate, pop_age_structure = pop_age_structure_los_angeles, calculate_transmission_probability = 1, contact.matrix = contact_matrix_9age_without_intervention_los_angeles)
##
transm.rate_without_intervention_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact_matrix_9age_without_intervention_Nairobi)
saveRDS(transm.rate_without_intervention_Nairobi,"transm.rate_without_intervention_Nairobi_age_effect.rds")
transm.rate_package1_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package1_Nairobi)
saveRDS(transm.rate_package1_Nairobi,"transm.rate_package1_Nairobi_age_effect.rds")
transm.rate_package2_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package2_Nairobi)
saveRDS(transm.rate_package2_Nairobi,"transm.rate_package2_Nairobi_age_effect.rds")
transm.rate_package3_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package3_Nairobi)
saveRDS(transm.rate_package3_Nairobi,"transm.rate_package3_Nairobi_age_effect.rds")
transm.rate_package4_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package4_Nairobi)
saveRDS(transm.rate_package4_Nairobi,"transm.rate_package4_Nairobi_age_effect.rds")
transm.rate_package5_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package5_Nairobi)
saveRDS(transm.rate_package5_Nairobi,"transm.rate_package5_Nairobi_age_effect.rds")
transm.rate_package6_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package6_Nairobi)
saveRDS(transm.rate_package6_Nairobi,"transm.rate_package6_Nairobi_age_effect.rds")
transm.rate_package7_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package7_Nairobi)
saveRDS(transm.rate_package7_Nairobi,"transm.rate_package7_Nairobi_age_effect.rds")
#daegu
transm.rate_without_intervention_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact_matrix_9age_without_intervention_daegu)
saveRDS(transm.rate_without_intervention_daegu,"transm.rate_without_intervention_daegu_age_effect.rds")
transm.rate_package1_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package1_daegu)
saveRDS(transm.rate_package1_daegu,"transm.rate_package1_daegu_age_effect.rds")
transm.rate_package2_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package2_daegu)
saveRDS(transm.rate_package2_daegu,"transm.rate_package2_daegu_age_effect.rds")
transm.rate_package3_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package3_daegu)
saveRDS(transm.rate_package3_daegu,"transm.rate_package3_daegu_age_effect.rds")
transm.rate_package4_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package4_daegu)
saveRDS(transm.rate_package4_daegu,"transm.rate_package4_daegu_age_effect.rds")
transm.rate_package5_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package5_daegu)
saveRDS(transm.rate_package5_daegu,"transm.rate_package5_daegu_age_effect.rds")
transm.rate_package6_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package6_daegu)
saveRDS(transm.rate_package6_daegu,"transm.rate_package6_daegu_age_effect.rds")
transm.rate_package7_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package7_daegu)
saveRDS(transm.rate_package7_daegu,"transm.rate_package7_daegu_age_effect.rds")
#nyc
transm.rate_without_intervention_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact_matrix_9age_without_intervention_nyc)
saveRDS(transm.rate_without_intervention_nyc,"transm.rate_without_intervention_nyc_age_effect.rds")
transm.rate_package1_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package1_nyc)
saveRDS(transm.rate_package1_nyc,"transm.rate_package1_nyc_age_effect.rds")
transm.rate_package2_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package2_nyc)
saveRDS(transm.rate_package2_nyc,"transm.rate_package2_nyc_age_effect.rds")
transm.rate_package3_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package3_nyc)
saveRDS(transm.rate_package3_nyc,"transm.rate_package3_nyc_age_effect.rds")
transm.rate_package4_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package4_nyc)
saveRDS(transm.rate_package4_nyc,"transm.rate_package4_nyc_age_effect.rds")
transm.rate_package5_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package5_nyc)
saveRDS(transm.rate_package5_nyc,"transm.rate_package5_nyc_age_effect.rds")
transm.rate_package6_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package6_nyc)
saveRDS(transm.rate_package6_nyc,"transm.rate_package6_nyc_age_effect.rds")
transm.rate_package7_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package7_nyc)
saveRDS(transm.rate_package7_nyc,"transm.rate_package7_nyc_age_effect.rds")
#los_angeles
transm.rate_without_intervention_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact_matrix_9age_without_intervention_los_angeles)
saveRDS(transm.rate_without_intervention_los_angeles,"transm.rate_without_intervention_los_angeles_age_effect.rds")
transm.rate_package1_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package1_los_angeles)
saveRDS(transm.rate_package1_los_angeles,"transm.rate_package1_los_angeles_age_effect.rds")
transm.rate_package2_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package2_los_angeles)
saveRDS(transm.rate_package2_los_angeles,"transm.rate_package2_los_angeles_age_effect.rds")
transm.rate_package3_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package3_los_angeles)
saveRDS(transm.rate_package3_los_angeles,"transm.rate_package3_los_angeles_age_effect.rds")
transm.rate_package4_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package4_los_angeles)
saveRDS(transm.rate_package4_los_angeles,"transm.rate_package4_los_angeles_age_effect.rds")
transm.rate_package5_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package5_los_angeles)
saveRDS(transm.rate_package5_los_angeles,"transm.rate_package5_los_angeles_age_effect.rds")
transm.rate_package6_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package6_los_angeles)
saveRDS(transm.rate_package6_los_angeles,"transm.rate_package6_los_angeles_age_effect.rds")
transm.rate_package7_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package7_los_angeles)
saveRDS(transm.rate_package7_los_angeles,"transm.rate_package7_los_angeles_age_effect.rds")
transm.rate_package8_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package8_los_angeles)
saveRDS(transm.rate_package8_los_angeles,"transm.rate_package8_los_angeles_age_effect.rds")

#parameters for SEIR-HQD model
parameters_Nairobi_age_effect <- list(transm.rate = transm.rate_without_intervention_Nairobi,
                                      incub.rate = incub.rate,
                                      recov.rate = recov.rate,
                                      hosp.rate = hosp.rate_Nairobi,
                                      fatal.rate = fatal.rate_Nairobi,
                                      quar.rate = quar.rate_Nairobi,
                                      death.rate = death.rate_Nairobi,
                                      birth.rate = birth.rate_Nairobi,
                                      n = pop_age_structure_Nairobi*pop)
saveRDS(parameters_Nairobi_age_effect,"parameters_Nairobi_age_effect.rds")

#without intervention
parameters_daegu_age_effect <- list(transm.rate = transm.rate_without_intervention_daegu,
                                    incub.rate = incub.rate,
                                    recov.rate = recov.rate,
                                    hosp.rate = hosp.rate_daegu,
                                    fatal.rate = fatal.rate_daegu,
                                    quar.rate = quar.rate_daegu,
                                    death.rate = death.rate_daegu,
                                    birth.rate = birth.rate_daegu,
                                    n = pop_age_structure_daegu*pop)
saveRDS(parameters_daegu_age_effect,"parameters_daegu_age_effect.rds")

#without intervention
parameters_nyc_age_effect <- list(transm.rate = transm.rate_without_intervention_nyc,
                                  incub.rate = incub.rate,
                                  recov.rate = recov.rate,
                                  hosp.rate = hosp.rate_nyc,
                                  fatal.rate = fatal.rate_nyc,
                                  quar.rate = quar.rate_nyc,
                                  death.rate = death.rate_nyc,
                                  birth.rate = birth.rate_nyc,
                                  n = pop_age_structure_nyc*pop)
saveRDS(parameters_nyc_age_effect,"parameters_nyc_age_effect.rds")


#without intervention
parameters_los_angeles_age_effect <- list(transm.rate = transm.rate_without_intervention_los_angeles,
                                          incub.rate = incub.rate,
                                          recov.rate = recov.rate,
                                          hosp.rate = hosp.rate_los_angeles,
                                          fatal.rate = fatal.rate_los_angeles,
                                          quar.rate = quar.rate_los_angeles,
                                          death.rate = death.rate_los_angeles,
                                          birth.rate = birth.rate_los_angeles,
                                          n = pop_age_structure_los_angeles*pop)
saveRDS(parameters_los_angeles_age_effect,"parameters_los_angeles_age_effect.rds")

#No intervention
constraint_no_intervention_Nairobi <- list(transm.rate = matrix(1,9,9),#transmission rate
                                           incub.rate = c(rep(1,9)),#incubation
                                           recov.rate = c(rep(1,9)),#recovery rate
                                           hosp.rate = c(rep(1,9)),#hospitalization rate
                                           fatal.rate = c(rep(1,9)),#fatality rate
                                           quar.rate = c(rep(1,9)),#quarantine rate
                                           death.rate = c(rep(1,9)),  #death.rate
                                           birth.rate = c(rep(1,9)),#birth rate
                                           n = c(rep(1,9)))
saveRDS(constraint_no_intervention_Nairobi,"constraint_no_intervention_Nairobi.rds")


constraint_no_intervention_daegu <- list(transm.rate = matrix(1,9,9),
                                         incub.rate = c(rep(1,9)),
                                         recov.rate = c(rep(1,9)),
                                         hosp.rate = c(rep(1,9)),
                                         fatal.rate = c(rep(1,9)),
                                         quar.rate = c(rep(1,9)),
                                         death.rate = c(rep(1,9)),
                                         birth.rate = c(rep(1,9)),
                                         n = c(rep(1,9)))
saveRDS(constraint_no_intervention_daegu,"constraint_no_intervention_daegu.rds")

constraint_no_intervention_nyc <- list(transm.rate = matrix(1,9,9),
                                       incub.rate = c(rep(1,9)),
                                       recov.rate = c(rep(1,9)),
                                       hosp.rate = c(rep(1,9)),
                                       fatal.rate = c(rep(1,9)),
                                       quar.rate = c(rep(1,9)),
                                       death.rate = c(rep(1,9)), 
                                       birth.rate = c(rep(1,9)),
                                       n = c(rep(1,9)))
saveRDS(constraint_no_intervention_nyc,"constraint_no_intervention_nyc.rds")

constraint_no_intervention_los_angeles <- list(transm.rate = matrix(1,9,9),
                                               incub.rate = c(rep(1,9)),
                                               recov.rate = c(rep(1,9)),
                                               hosp.rate = c(rep(1,9)),
                                               fatal.rate = c(rep(1,9)),
                                               quar.rate = c(rep(1,9)),
                                               death.rate = c(rep(1,9)),
                                               birth.rate = c(rep(1,9)),
                                               n = c(rep(1,9)))
saveRDS(constraint_no_intervention_los_angeles,"constraint_no_intervention_los_angeles.rds")



#intervention constraint package1
constraint_package1_Nairobi <- list(transm.rate = transm.rate_package1_Nairobi/transm.rate_without_intervention_Nairobi,
                                    incub.rate = c(rep(1,9)),
                                    recov.rate = c(rep(1,9)),
                                    hosp.rate = c(rep(1,9)),
                                    fatal.rate = c(rep(1,9)),
                                    quar.rate = c(rep(1,9)),
                                    death.rate = c(rep(1,9)), 
                                    birth.rate = c(rep(1,9)),
                                    n = c(rep(1,9)))
saveRDS(constraint_package1_Nairobi,"constraint_package1_Nairobi.rds")


constraint_package1_daegu <- list(transm.rate = transm.rate_package1_daegu/transm.rate_without_intervention_daegu,
                                  incub.rate = c(rep(1,9)),
                                  recov.rate = c(rep(1,9)),
                                  hosp.rate = c(rep(1,9)),
                                  fatal.rate = c(rep(1,9)),
                                  quar.rate = c(rep(1,9)),
                                  death.rate = c(rep(1,9)),
                                  birth.rate = c(rep(1,9)),
                                  n = c(rep(1,9)))
saveRDS(constraint_package1_daegu,"constraint_package1_daegu.rds")

constraint_package1_nyc <- list(transm.rate = transm.rate_package1_nyc/transm.rate_without_intervention_nyc,
                                incub.rate = c(rep(1,9)),#
                                recov.rate = c(rep(1,9)),
                                hosp.rate = c(rep(1,9)),
                                fatal.rate = c(rep(1,9)),
                                quar.rate = c(rep(1,9)),
                                death.rate = c(rep(1,9)), 
                                birth.rate = c(rep(1,9)),
                                n = c(rep(1,9)))
saveRDS(constraint_package1_nyc,"constraint_package1_nyc.rds")

constraint_package1_los_angeles <- list(transm.rate = transm.rate_package1_los_angeles/transm.rate_without_intervention_los_angeles,
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(1,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_package1_los_angeles,"constraint_package1_los_angeles.rds")


#intervention constraint package2
constraint_package2_Nairobi <- list(transm.rate = transm.rate_package2_Nairobi/transm.rate_without_intervention_Nairobi,
                                    incub.rate = c(rep(1,9)),#
                                    recov.rate = c(rep(1,9)),
                                    hosp.rate = c(rep(1,9)),
                                    fatal.rate = c(rep(1,9)),
                                    quar.rate = c(rep(1,9)),
                                    death.rate = c(rep(1,9)), 
                                    birth.rate = c(rep(1,9)),
                                    n = c(rep(1,9)))
saveRDS(constraint_package2_Nairobi,"constraint_package2_Nairobi.rds")


constraint_package2_daegu <- list(transm.rate = transm.rate_package2_daegu/transm.rate_without_intervention_daegu,
                                  incub.rate = c(rep(1,9)),#
                                  recov.rate = c(rep(1,9)),
                                  hosp.rate = c(rep(1,9)),
                                  fatal.rate = c(rep(1,9)),
                                  quar.rate = c(rep(1,9)),
                                  death.rate = c(rep(1,9)), 
                                  birth.rate = c(rep(1,9)),
                                  n = c(rep(1,9)))
saveRDS(constraint_package2_daegu,"constraint_package2_daegu.rds")

constraint_package2_nyc <- list(transm.rate = transm.rate_package2_nyc/transm.rate_without_intervention_nyc,
                                incub.rate = c(rep(1,9)),#
                                recov.rate = c(rep(1,9)),
                                hosp.rate = c(rep(1,9)),
                                fatal.rate = c(rep(1,9)),
                                quar.rate = c(rep(1,9)),
                                death.rate = c(rep(1,9)), 
                                birth.rate = c(rep(1,9)),
                                n = c(rep(1,9)))
saveRDS(constraint_package2_nyc,"constraint_package2_nyc.rds")

constraint_package2_los_angeles <- list(transm.rate = transm.rate_package2_los_angeles/transm.rate_without_intervention_los_angeles,
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(1,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_package2_los_angeles,"constraint_package2_los_angeles.rds")


#intervention constraint package3
constraint_package3_Nairobi <- list(transm.rate = transm.rate_package3_Nairobi/transm.rate_without_intervention_Nairobi,
                                    incub.rate = c(rep(1,9)),#
                                    recov.rate = c(rep(1,9)),
                                    hosp.rate = c(rep(1,9)),
                                    fatal.rate = c(rep(1,9)),
                                    quar.rate = c(rep(1,9)),
                                    death.rate = c(rep(1,9)), 
                                    birth.rate = c(rep(1,9)),
                                    n = c(rep(1,9)))
saveRDS(constraint_package3_Nairobi,"constraint_package3_Nairobi.rds")


constraint_package3_daegu <- list(transm.rate = transm.rate_package3_daegu/transm.rate_without_intervention_daegu,
                                  incub.rate = c(rep(1,9)),#
                                  recov.rate = c(rep(1,9)),
                                  hosp.rate = c(rep(1,9)),
                                  fatal.rate = c(rep(1,9)),
                                  quar.rate = c(rep(1,9)),
                                  death.rate = c(rep(1,9)), 
                                  birth.rate = c(rep(1,9)),
                                  n = c(rep(1,9)))
saveRDS(constraint_package3_daegu,"constraint_package3_daegu.rds")

constraint_package3_nyc <- list(transm.rate = transm.rate_package3_nyc/transm.rate_without_intervention_nyc,
                                incub.rate = c(rep(1,9)),#
                                recov.rate = c(rep(1,9)),
                                hosp.rate = c(rep(1,9)),
                                fatal.rate = c(rep(1,9)),
                                quar.rate = c(rep(1,9)),
                                death.rate = c(rep(1,9)), 
                                birth.rate = c(rep(1,9)),
                                n = c(rep(1,9)))
saveRDS(constraint_package3_nyc,"constraint_package3_nyc.rds")

constraint_package3_los_angeles <- list(transm.rate = transm.rate_package3_los_angeles/transm.rate_without_intervention_los_angeles,
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(1,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_package3_los_angeles,"constraint_package3_los_angeles.rds")


#intervention constraint package4
constraint_package4_Nairobi <- list(transm.rate = transm.rate_package4_Nairobi/transm.rate_without_intervention_Nairobi,
                                    incub.rate = c(rep(1,9)),#
                                    recov.rate = c(rep(1,9)),
                                    hosp.rate = c(rep(1,9)),
                                    fatal.rate = c(rep(1,9)),
                                    quar.rate = c(rep(10000,9)),
                                    death.rate = c(rep(1,9)), 
                                    birth.rate = c(rep(1,9)),
                                    n = c(rep(1,9)))
saveRDS(constraint_package4_Nairobi,"constraint_package4_Nairobi.rds")


constraint_package4_daegu <- list(transm.rate = transm.rate_package4_daegu/transm.rate_without_intervention_daegu,
                                  incub.rate = c(rep(1,9)),#
                                  recov.rate = c(rep(1,9)),
                                  hosp.rate = c(rep(1,9)),
                                  fatal.rate = c(rep(1,9)),
                                  quar.rate = c(rep(10000,9)),
                                  death.rate = c(rep(1,9)), 
                                  birth.rate = c(rep(1,9)),
                                  n = c(rep(1,9)))
saveRDS(constraint_package4_daegu,"constraint_package4_daegu.rds")

constraint_package4_nyc <- list(transm.rate = transm.rate_package4_nyc/transm.rate_without_intervention_nyc,
                                incub.rate = c(rep(1,9)),#
                                recov.rate = c(rep(1,9)),
                                hosp.rate = c(rep(1,9)),
                                fatal.rate = c(rep(1,9)),
                                quar.rate = c(rep(10000,9)),
                                death.rate = c(rep(1,9)), 
                                birth.rate = c(rep(1,9)),
                                n = c(rep(1,9)))
saveRDS(constraint_package4_nyc,"constraint_package4_nyc.rds")

constraint_package4_los_angeles <- list(transm.rate = transm.rate_package4_los_angeles/transm.rate_without_intervention_los_angeles,
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(10000,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_package4_los_angeles,"constraint_package4_los_angeles.rds")


#intervention constraint package5
constraint_package5_Nairobi <- list(transm.rate = transm.rate_package5_Nairobi/transm.rate_without_intervention_Nairobi,
                                    incub.rate = c(rep(1,9)),#
                                    recov.rate = c(rep(1,9)),
                                    hosp.rate = c(rep(1,9)),
                                    fatal.rate = c(rep(1,9)),
                                    quar.rate = c(rep(10000,9)),
                                    death.rate = c(rep(1,9)), 
                                    birth.rate = c(rep(1,9)),
                                    n = c(rep(1,9)))
saveRDS(constraint_package5_Nairobi,"constraint_package5_Nairobi.rds")


constraint_package5_daegu <- list(transm.rate = transm.rate_package5_daegu/transm.rate_without_intervention_daegu,
                                  incub.rate = c(rep(1,9)),#
                                  recov.rate = c(rep(1,9)),
                                  hosp.rate = c(rep(1,9)),
                                  fatal.rate = c(rep(1,9)),
                                  quar.rate = c(rep(10000,9)),
                                  death.rate = c(rep(1,9)), 
                                  birth.rate = c(rep(1,9)),
                                  n = c(rep(1,9)))
saveRDS(constraint_package5_daegu,"constraint_package5_daegu.rds")

constraint_package5_nyc <- list(transm.rate = transm.rate_package5_nyc/transm.rate_without_intervention_nyc,
                                incub.rate = c(rep(1,9)),#
                                recov.rate = c(rep(1,9)),
                                hosp.rate = c(rep(1,9)),
                                fatal.rate = c(rep(1,9)),
                                quar.rate = c(rep(10000,9)),
                                death.rate = c(rep(1,9)), 
                                birth.rate = c(rep(1,9)),
                                n = c(rep(1,9)))
saveRDS(constraint_package5_nyc,"constraint_package5_nyc.rds")

constraint_package5_los_angeles <- list(transm.rate = transm.rate_package5_los_angeles/transm.rate_without_intervention_los_angeles,
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(10000,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_package5_los_angeles,"constraint_package5_los_angeles.rds")


#intervention constraint package6
constraint_package6_Nairobi <- list(transm.rate = transm.rate_package6_Nairobi/transm.rate_without_intervention_Nairobi,
                                    incub.rate = c(rep(1,9)),#
                                    recov.rate = c(rep(1,9)),
                                    hosp.rate = c(rep(1,9)),
                                    fatal.rate = c(rep(1,9)),
                                    quar.rate = c(rep(1,9)),
                                    death.rate = c(rep(1,9)), 
                                    birth.rate = c(rep(1,9)),
                                    n = c(rep(1,9)))
saveRDS(constraint_package6_Nairobi,"constraint_package6_Nairobi.rds")


constraint_package6_daegu <- list(transm.rate = transm.rate_package6_daegu/transm.rate_without_intervention_daegu,
                                  incub.rate = c(rep(1,9)),#
                                  recov.rate = c(rep(1,9)),
                                  hosp.rate = c(rep(1,9)),
                                  fatal.rate = c(rep(1,9)),
                                  quar.rate = c(rep(1,9)),
                                  death.rate = c(rep(1,9)), 
                                  birth.rate = c(rep(1,9)),
                                  n = c(rep(1,9)))
saveRDS(constraint_package6_daegu,"constraint_package6_daegu.rds")

constraint_package6_nyc <- list(transm.rate = transm.rate_package6_nyc/transm.rate_without_intervention_nyc,
                                incub.rate = c(rep(1,9)),#
                                recov.rate = c(rep(1,9)),
                                hosp.rate = c(rep(1,9)),
                                fatal.rate = c(rep(1,9)),
                                quar.rate = c(rep(1,9)),
                                death.rate = c(rep(1,9)), 
                                birth.rate = c(rep(1,9)),
                                n = c(rep(1,9)))
saveRDS(constraint_package6_nyc,"constraint_package6_nyc.rds")

constraint_package6_los_angeles <- list(transm.rate = transm.rate_package6_los_angeles/transm.rate_without_intervention_los_angeles,
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(1,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_package6_los_angeles,"constraint_package6_los_angeles.rds")


#intervention constraint package7
constraint_package7_Nairobi <- list(transm.rate = transm.rate_package7_Nairobi/transm.rate_without_intervention_Nairobi,
                                    incub.rate = c(rep(1,9)),#
                                    recov.rate = c(rep(1,9)),
                                    hosp.rate = c(rep(1,9)),
                                    fatal.rate = c(rep(1,9)),
                                    quar.rate = c(rep(1,9)),
                                    death.rate = c(rep(1,9)), 
                                    birth.rate = c(rep(1,9)),
                                    n = c(rep(1,9)))
saveRDS(constraint_package7_Nairobi,"constraint_package7_Nairobi.rds")


constraint_package7_daegu <- list(transm.rate = transm.rate_package7_daegu/transm.rate_without_intervention_daegu,
                                  incub.rate = c(rep(1,9)),#
                                  recov.rate = c(rep(1,9)),
                                  hosp.rate = c(rep(1,9)),
                                  fatal.rate = c(rep(1,9)),
                                  quar.rate = c(rep(1,9)),
                                  death.rate = c(rep(1,9)), 
                                  birth.rate = c(rep(1,9)),
                                  n = c(rep(1,9)))
saveRDS(constraint_package7_daegu,"constraint_package7_daegu.rds")

constraint_package7_nyc <- list(transm.rate = transm.rate_package7_nyc/transm.rate_without_intervention_nyc,
                                incub.rate = c(rep(1,9)),#
                                recov.rate = c(rep(1,9)),
                                hosp.rate = c(rep(1,9)),
                                fatal.rate = c(rep(1,9)),
                                quar.rate = c(rep(1,9)),
                                death.rate = c(rep(1,9)), 
                                birth.rate = c(rep(1,9)),
                                n = c(rep(1,9)))
saveRDS(constraint_package7_nyc,"constraint_package7_nyc.rds")

constraint_package7_los_angeles <- list(transm.rate = transm.rate_package7_los_angeles/transm.rate_without_intervention_los_angeles,
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(1,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_package7_los_angeles,"constraint_package7_los_angeles.rds")

constraint_package8_los_angeles <- list(transm.rate = transm.rate_package8_los_angeles/transm.rate_without_intervention_los_angeles,
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(1,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_package8_los_angeles,"constraint_package8_los_angeles.rds")

initials_Nairobi <- c(S. = c(pop_age_structure_Nairobi*pop-(pop_age_structure_Nairobi*pop*0.0002)/incub.rate - pop_age_structure_Nairobi*pop*0.0002), 
                      E. = c(pop_age_structure_Nairobi*pop*0.0002/incub.rate),
                      I. = c(pop_age_structure_Nairobi*pop*0.0002),
                      H. = c(rep(0,9)),
                      D. = c(rep(0,9)),
                      Q. = c(rep(0,9)),
                      R. = c(rep(0,9)),
                      Ic. = c(pop_age_structure_Nairobi*pop*0.0002))
saveRDS(initials_Nairobi,"initials_Nairobi.rds")


initials_daegu <- c(S. = c(pop_age_structure_daegu*pop-(pop_age_structure_daegu*pop*0.0002)/incub.rate - pop_age_structure_daegu*pop*0.0002), 
                    E. = c(pop_age_structure_daegu*pop*0.0002/incub.rate),
                    I. = c(pop_age_structure_daegu*pop*0.0002),
                    H. = c(rep(0,9)),
                    D. = c(rep(0,9)),
                    Q. = c(rep(0,9)),
                    R. = c(rep(0,9)),
                    Ic. = c(pop_age_structure_daegu*pop*0.0002))
saveRDS(initials_daegu,"initials_daegu.rds")

initials_nyc <- c(S. = c(pop_age_structure_nyc*pop-(pop_age_structure_nyc*pop*0.0002)/incub.rate - pop_age_structure_nyc*pop*0.0002), 
                  E. = c(pop_age_structure_nyc*pop*0.0002/incub.rate),
                  I. = c(pop_age_structure_nyc*pop*0.0002),
                  H. = c(rep(0,9)),
                  D. = c(rep(0,9)),
                  Q. = c(rep(0,9)),
                  R. = c(rep(0,9)),
                  Ic. = c(pop_age_structure_nyc*pop*0.0002))
saveRDS(initials_nyc,"initials_nyc.rds")


initials_los_angeles <- c(S. = c(pop_age_structure_los_angeles*pop-(pop_age_structure_los_angeles*pop*0.0002)/incub.rate - pop_age_structure_los_angeles*pop*0.0002), 
                          E. = c(pop_age_structure_los_angeles*pop*0.0002/incub.rate),
                          I. = c(pop_age_structure_los_angeles*pop*0.0002),
                          H. = c(rep(0,9)),
                          D. = c(rep(0,9)),
                          Q. = c(rep(0,9)),
                          R. = c(rep(0,9)),
                          Ic. = c(pop_age_structure_los_angeles*pop*0.0002))
saveRDS(initials_los_angeles,"initials_los_angeles.rds")


#no intervention
#View(SEIHDQR_9age_simulation_Nairobi_no_inter_age_effect_estimate)
#simulate the cases
SEIHDQR_9age_simulation_Nairobi_no_inter_age_effect_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi_age_effect, 
                                                                                                                  init = initials_Nairobi, time = 0:365, 
                                                                                                                  outbreak_start_date = as.Date("2020-03-13"), 
                                                                                                                  inter_start_date1 = as.Date('2020-03-20'), 
                                                                                                                  inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                                  inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                                  inter_start_date4 = as.Date("2020-04-09"),
                                                                                                                  inter_end_date4 = as.Date("2020-11-20"), 
                                                                                                                  constraint1 = constraint_no_intervention_Nairobi, 
                                                                                                                  constraint2 = constraint_no_intervention_Nairobi, 
                                                                                                                  constraint3 = constraint_no_intervention_Nairobi, 
                                                                                                                  constraint4 = constraint_no_intervention_Nairobi, 
                                                                                                                  constraint5 = constraint_no_intervention_Nairobi, 
                                                                                                                  constraint6 =constraint_no_intervention_Nairobi)

SEIHDQR_9age_simulation_daegu_no_inter_age_effect_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu_age_effect, 
                                                                                                                init = initials_daegu, time = 0:365, 
                                                                                                                outbreak_start_date = as.Date("2020-01-20"), 
                                                                                                                inter_start_date1 = as.Date('2020-01-27'), 
                                                                                                                inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                                inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                                inter_start_date4 = as.Date("2020-04-20"),
                                                                                                                inter_end_date4 = as.Date("2020-09-27"), 
                                                                                                                constraint1 = constraint_no_intervention_daegu, 
                                                                                                                constraint2 = constraint_no_intervention_daegu, 
                                                                                                                constraint3 = constraint_no_intervention_daegu, 
                                                                                                                constraint4 = constraint_no_intervention_daegu, 
                                                                                                                constraint5 = constraint_no_intervention_daegu, 
                                                                                                                constraint6 =constraint_no_intervention_daegu)

SEIHDQR_9age_simulation_nyc_no_inter_age_effect_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc_age_effect, 
                                                                                                              init = initials_nyc, time = 0:365, 
                                                                                                              outbreak_start_date = as.Date("2020-03-02"), 
                                                                                                              inter_start_date1 = as.Date('2020-03-09'), 
                                                                                                              inter_start_date2 = as.Date("2020-03-16"), 
                                                                                                              inter_start_date3 = as.Date("2020-03-22"), 
                                                                                                              inter_start_date4 = as.Date("2020-04-02"),
                                                                                                              inter_end_date4 = as.Date("2020-11-09"), 
                                                                                                              constraint1 = constraint_no_intervention_nyc, 
                                                                                                              constraint2 = constraint_no_intervention_nyc, 
                                                                                                              constraint3 = constraint_no_intervention_nyc, 
                                                                                                              constraint4 = constraint_no_intervention_nyc, 
                                                                                                              constraint5 = constraint_no_intervention_nyc, 
                                                                                                              constraint6 =constraint_no_intervention_nyc)

SEIHDQR_9age_simulation_los_angeles_no_inter_age_effect_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles_age_effect, 
                                                                                                                      init = initials_los_angeles, time = 0:365, 
                                                                                                                      outbreak_start_date = as.Date("2020-01-26"), 
                                                                                                                      inter_start_date1 = as.Date('2020-02-02'), 
                                                                                                                      inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                                      inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                                      inter_start_date4 = as.Date("2020-04-20"),
                                                                                                                      inter_end_date4 = as.Date("2020-10-02"), 
                                                                                                                      constraint1 = constraint_no_intervention_los_angeles,
                                                                                                                      constraint2 = constraint_no_intervention_los_angeles, 
                                                                                                                      constraint3 = constraint_no_intervention_los_angeles, 
                                                                                                                      constraint4 = constraint_no_intervention_los_angeles, 
                                                                                                                      constraint5 = constraint_no_intervention_los_angeles, 
                                                                                                                      constraint6 =constraint_no_intervention_los_angeles)



#intervention school close
SEIHDQR_9age_simulation_Nairobi_inter_package1_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi_age_effect, 
                                                                                                             init = initials_Nairobi, time = 0:365, 
                                                                                                             outbreak_start_date = as.Date("2020-03-13"), 
                                                                                                             inter_start_date1 = as.Date('2020-03-20'), 
                                                                                                             inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                             inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                             inter_start_date4 = as.Date("2020-04-09"),
                                                                                                             inter_end_date4 = as.Date("2020-11-20"), 
                                                                                                             constraint1 = constraint_no_intervention_Nairobi, 
                                                                                                             constraint2 = constraint_package1_Nairobi, 
                                                                                                             constraint3 = constraint_package1_Nairobi, 
                                                                                                             constraint4 = constraint_package1_Nairobi, 
                                                                                                             constraint5 = constraint_package1_Nairobi, 
                                                                                                             constraint6 =constraint_no_intervention_Nairobi)

SEIHDQR_9age_simulation_daegu_inter_package1_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu_age_effect, 
                                                                                                           init = initials_daegu, time = 0:365,
                                                                                                           outbreak_start_date = as.Date("2020-01-20"), 
                                                                                                           inter_start_date1 = as.Date('2020-01-27'), 
                                                                                                           inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                           inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                           inter_start_date4 = as.Date("2020-04-20"),
                                                                                                           inter_end_date4 = as.Date("2020-09-27"), 
                                                                                                           constraint1 = constraint_no_intervention_daegu, 
                                                                                                           constraint2 = constraint_package1_daegu, 
                                                                                                           constraint3 = constraint_package1_daegu, 
                                                                                                           constraint4 = constraint_package1_daegu, 
                                                                                                           constraint5 = constraint_package1_daegu, 
                                                                                                           constraint6 =constraint_no_intervention_daegu)

SEIHDQR_9age_simulation_nyc_inter_package1_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc_age_effect, 
                                                                                                         init = initials_nyc, time = 0:365, 
                                                                                                         outbreak_start_date = as.Date("2020-03-02"), 
                                                                                                         inter_start_date1 = as.Date('2020-03-09'), 
                                                                                                         inter_start_date2 = as.Date("2020-03-16"), 
                                                                                                         inter_start_date3 = as.Date("2020-03-22"), 
                                                                                                         inter_start_date4 = as.Date("2020-04-02"),
                                                                                                         inter_end_date4 = as.Date("2020-11-09"),
                                                                                                         constraint1 = constraint_no_intervention_nyc, 
                                                                                                         constraint2 = constraint_package1_nyc, 
                                                                                                         constraint3 = constraint_package1_nyc, 
                                                                                                         constraint4 = constraint_package1_nyc, 
                                                                                                         constraint5 = constraint_package1_nyc, 
                                                                                                         constraint6 =constraint_no_intervention_nyc)

SEIHDQR_9age_simulation_los_angeles_inter_package1_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles_age_effect, 
                                                                                                                 init = initials_los_angeles, time = 0:365, 
                                                                                                                 outbreak_start_date = as.Date("2020-01-26"), 
                                                                                                                 inter_start_date1 = as.Date('2020-02-02'), 
                                                                                                                 inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                                 inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                                 inter_start_date4 = as.Date("2020-04-20"),
                                                                                                                 inter_end_date4 = as.Date("2020-10-02"), 
                                                                                                                 constraint1 = constraint_no_intervention_los_angeles,
                                                                                                                 constraint2 = constraint_package1_los_angeles, 
                                                                                                                 constraint3 = constraint_package1_los_angeles, 
                                                                                                                 constraint4 = constraint_package1_los_angeles, 
                                                                                                                 constraint5 = constraint_package1_los_angeles, 
                                                                                                                 constraint6 =constraint_no_intervention_los_angeles)



#intervention 50% work from home
SEIHDQR_9age_simulation_Nairobi_inter_package2_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi_age_effect, 
                                                                                                             init = initials_Nairobi, time = 0:365,
                                                                                                             outbreak_start_date = as.Date("2020-03-13"), 
                                                                                                             inter_start_date1 = as.Date('2020-03-20'), 
                                                                                                             inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                             inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                             inter_start_date4 = as.Date("2020-04-09"),
                                                                                                             inter_end_date4 = as.Date("2020-11-20"), 
                                                                                                             constraint1 = constraint_no_intervention_Nairobi, 
                                                                                                             constraint2 = constraint_package2_Nairobi, 
                                                                                                             constraint3 = constraint_package2_Nairobi, 
                                                                                                             constraint4 = constraint_package2_Nairobi, 
                                                                                                             constraint5 = constraint_package2_Nairobi, 
                                                                                                             constraint6 =constraint_no_intervention_Nairobi)

SEIHDQR_9age_simulation_daegu_inter_package2_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu_age_effect, 
                                                                                                           init = initials_daegu, time = 0:365, 
                                                                                                           outbreak_start_date = as.Date("2020-01-20"), 
                                                                                                           inter_start_date1 = as.Date('2020-01-27'), 
                                                                                                           inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                           inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                           inter_start_date4 = as.Date("2020-04-20"),
                                                                                                           inter_end_date4 = as.Date("2020-09-27"), 
                                                                                                           constraint1 = constraint_no_intervention_daegu, 
                                                                                                           constraint2 = constraint_package2_daegu, 
                                                                                                           constraint3 = constraint_package2_daegu, 
                                                                                                           constraint4 = constraint_package2_daegu, 
                                                                                                           constraint5 = constraint_package2_daegu, 
                                                                                                           constraint6 =constraint_no_intervention_daegu)

SEIHDQR_9age_simulation_nyc_inter_package2_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc_age_effect, 
                                                                                                         init = initials_nyc, time = 0:365, 
                                                                                                         outbreak_start_date = as.Date("2020-03-02"), 
                                                                                                         inter_start_date1 = as.Date('2020-03-09'), 
                                                                                                         inter_start_date2 = as.Date("2020-03-16"), 
                                                                                                         inter_start_date3 = as.Date("2020-03-22"), 
                                                                                                         inter_start_date4 = as.Date("2020-04-02"),
                                                                                                         inter_end_date4 = as.Date("2020-11-09"), 
                                                                                                         constraint1 = constraint_no_intervention_nyc, 
                                                                                                         constraint2 = constraint_package2_nyc, 
                                                                                                         constraint3 = constraint_package2_nyc, 
                                                                                                         constraint4 = constraint_package2_nyc, 
                                                                                                         constraint5 = constraint_package2_nyc, 
                                                                                                         constraint6 =constraint_no_intervention_nyc)

SEIHDQR_9age_simulation_los_angeles_inter_package2_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles_age_effect, 
                                                                                                                 init = initials_los_angeles, time = 0:365, 
                                                                                                                 outbreak_start_date = as.Date("2020-01-26"), 
                                                                                                                 inter_start_date1 = as.Date('2020-02-02'), 
                                                                                                                 inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                                 inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                                 inter_start_date4 = as.Date("2020-04-20"),
                                                                                                                 inter_end_date4 = as.Date("2020-10-02"), 
                                                                                                                 constraint1 = constraint_no_intervention_los_angeles, 
                                                                                                                 constraint2 = constraint_package2_los_angeles, 
                                                                                                                 constraint3 = constraint_package2_los_angeles, 
                                                                                                                 constraint4 = constraint_package2_los_angeles, 
                                                                                                                 constraint5 = constraint_package2_los_angeles, 
                                                                                                                 constraint6 =constraint_no_intervention_los_angeles)




#intervention 50% other
SEIHDQR_9age_simulation_Nairobi_inter_package3_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi_age_effect, 
                                                                                                             init = initials_Nairobi, time = 0:365, 
                                                                                                             outbreak_start_date = as.Date("2020-03-13"), 
                                                                                                             inter_start_date1 = as.Date('2020-03-20'), 
                                                                                                             inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                             inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                             inter_start_date4 = as.Date("2020-04-09"),
                                                                                                             inter_end_date4 = as.Date("2020-11-20"), 
                                                                                                             constraint1 = constraint_no_intervention_Nairobi, 
                                                                                                             constraint2 = constraint_package3_Nairobi, 
                                                                                                             constraint3 = constraint_package3_Nairobi, 
                                                                                                             constraint4 = constraint_package3_Nairobi, 
                                                                                                             constraint5 = constraint_package3_Nairobi, 
                                                                                                             constraint6 =constraint_no_intervention_Nairobi)

SEIHDQR_9age_simulation_daegu_inter_package3_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu_age_effect, 
                                                                                                           init = initials_daegu, time = 0:365, 
                                                                                                           outbreak_start_date = as.Date("2020-01-20"), 
                                                                                                           inter_start_date1 = as.Date('2020-01-27'), 
                                                                                                           inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                           inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                           inter_start_date4 = as.Date("2020-04-20"),
                                                                                                           inter_end_date4 = as.Date("2020-09-27"), 
                                                                                                           constraint1 = constraint_no_intervention_daegu, 
                                                                                                           constraint2 = constraint_package3_daegu, 
                                                                                                           constraint3 = constraint_package3_daegu, 
                                                                                                           constraint4 = constraint_package3_daegu, 
                                                                                                           constraint5 = constraint_package3_daegu, 
                                                                                                           constraint6 =constraint_no_intervention_daegu)

SEIHDQR_9age_simulation_nyc_inter_package3_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc_age_effect, 
                                                                                                         init = initials_nyc, time = 0:365,
                                                                                                         outbreak_start_date = as.Date("2020-03-02"), 
                                                                                                         inter_start_date1 = as.Date('2020-03-09'), 
                                                                                                         inter_start_date2 = as.Date("2020-03-16"), 
                                                                                                         inter_start_date3 = as.Date("2020-03-22"), 
                                                                                                         inter_start_date4 = as.Date("2020-04-02"),
                                                                                                         inter_end_date4 = as.Date("2020-11-09"), 
                                                                                                         constraint1 = constraint_no_intervention_nyc, 
                                                                                                         constraint2 = constraint_package3_nyc, 
                                                                                                         constraint3 = constraint_package3_nyc, 
                                                                                                         constraint4 = constraint_package3_nyc, 
                                                                                                         constraint5 = constraint_package3_nyc, 
                                                                                                         constraint6 =constraint_no_intervention_nyc)

SEIHDQR_9age_simulation_los_angeles_inter_package3_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles_age_effect, 
                                                                                                                 init = initials_los_angeles, time = 0:365,  
                                                                                                                 outbreak_start_date = as.Date("2020-01-26"), 
                                                                                                                 inter_start_date1 = as.Date('2020-02-02'), 
                                                                                                                 inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                                 inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                                 inter_start_date4 = as.Date("2020-04-20"),
                                                                                                                 inter_end_date4 = as.Date("2020-10-02"), 
                                                                                                                 constraint1 = constraint_no_intervention_los_angeles, 
                                                                                                                 constraint2 = constraint_package3_los_angeles, 
                                                                                                                 constraint3 = constraint_package3_los_angeles, 
                                                                                                                 constraint4 = constraint_package3_los_angeles, 
                                                                                                                 constraint5 = constraint_package3_los_angeles, 
                                                                                                                 constraint6 =constraint_no_intervention_los_angeles)






SEIHDQR_9age_simulation_Nairobi_inter_package4_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi_age_effect, 
                                                                                                             init = initials_Nairobi, time = 0:365, 
                                                                                                             outbreak_start_date = as.Date("2020-03-13"),
                                                                                                             inter_start_date1 = as.Date('2020-03-20'), 
                                                                                                             inter_start_date2 = as.Date("2020-04-01"), 
                                                                                                             inter_start_date3 = as.Date("2020-04-07"), 
                                                                                                             inter_start_date4 = as.Date("2020-04-09"),
                                                                                                             inter_end_date4 = as.Date("2020-11-20"), 
                                                                                                             constraint1 = constraint_no_intervention_Nairobi, 
                                                                                                             constraint2 = constraint_package4_Nairobi, 
                                                                                                             constraint3 = constraint_package4_Nairobi, 
                                                                                                             constraint4 = constraint_package4_Nairobi,
                                                                                                             constraint5 = constraint_package4_Nairobi,
                                                                                                             constraint6 =constraint_no_intervention_Nairobi)

SEIHDQR_9age_simulation_daegu_inter_package4_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu_age_effect, init = initials_daegu, time = 0:365, outbreak_start_date = as.Date("2020-01-20"), inter_start_date1 = as.Date('2020-01-27'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-09-27"), constraint1 = constraint_no_intervention_daegu, constraint2 = constraint_package4_daegu, constraint3 = constraint_package4_daegu, constraint4 = constraint_package4_daegu, constraint5 = constraint_package4_daegu, constraint6 =constraint_no_intervention_daegu)

SEIHDQR_9age_simulation_nyc_inter_package4_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc_age_effect, init = initials_nyc, time = 0:365, outbreak_start_date = as.Date("2020-03-02"), inter_start_date1 = as.Date('2020-03-09'), inter_start_date2 = as.Date("2020-03-16"), inter_start_date3 = as.Date("2020-03-22"), inter_start_date4 = as.Date("2020-04-02"),inter_end_date4 = as.Date("2020-11-09"), constraint1 = constraint_no_intervention_nyc, constraint2 = constraint_package4_nyc, constraint3 = constraint_package4_nyc, constraint4 = constraint_package4_nyc, constraint5 = constraint_package4_nyc, constraint6 =constraint_no_intervention_nyc)

SEIHDQR_9age_simulation_los_angeles_inter_package4_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles_age_effect, init = initials_los_angeles, time = 0:365, outbreak_start_date = as.Date("2020-01-26"), inter_start_date1 = as.Date('2020-02-02'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-10-02"), constraint1 = constraint_no_intervention_los_angeles, constraint2 = constraint_package4_los_angeles, constraint3 = constraint_package4_los_angeles, constraint4 = constraint_package4_los_angeles, constraint5 = constraint_package4_los_angeles, constraint6 =constraint_no_intervention_los_angeles)




#intervention school close, 50% work from home and 50% other 1% quarantine rate
SEIHDQR_9age_simulation_Nairobi_inter_package5_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi_age_effect, init = initials_Nairobi, time = 0:365, outbreak_start_date = as.Date("2020-03-13"), inter_start_date1 = as.Date('2020-03-20'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-09"),inter_end_date4 = as.Date("2020-11-20"), constraint1 = constraint_no_intervention_Nairobi, constraint2 = constraint_package5_Nairobi, constraint3 = constraint_package5_Nairobi, constraint4 = constraint_package5_Nairobi, constraint5 = constraint_package5_Nairobi, constraint6 =constraint_no_intervention_Nairobi)

SEIHDQR_9age_simulation_daegu_inter_package5_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu_age_effect, init = initials_daegu, time = 0:365,  outbreak_start_date = as.Date("2020-01-20"), inter_start_date1 = as.Date('2020-01-27'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-09-27"), constraint1 = constraint_no_intervention_daegu, constraint2 = constraint_package5_daegu, constraint3 = constraint_package5_daegu, constraint4 = constraint_package5_daegu, constraint5 = constraint_package5_daegu, constraint6 =constraint_no_intervention_daegu)

SEIHDQR_9age_simulation_nyc_inter_package5_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc_age_effect, init = initials_nyc, time = 0:365, outbreak_start_date = as.Date("2020-03-02"), inter_start_date1 = as.Date('2020-03-09'), inter_start_date2 = as.Date("2020-03-16"), inter_start_date3 = as.Date("2020-03-22"), inter_start_date4 = as.Date("2020-04-02"),inter_end_date4 = as.Date("2020-11-09"), constraint1 = constraint_no_intervention_nyc, constraint2 = constraint_package5_nyc, constraint3 = constraint_package5_nyc, constraint4 = constraint_package5_nyc, constraint5 = constraint_package5_nyc, constraint6 =constraint_no_intervention_nyc)

SEIHDQR_9age_simulation_los_angeles_inter_package5_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles_age_effect, init = initials_los_angeles, time = 0:365, outbreak_start_date = as.Date("2020-01-26"), inter_start_date1 = as.Date('2020-02-02'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-10-02"), constraint1 = constraint_no_intervention_los_angeles, constraint2 = constraint_package5_los_angeles, constraint3 = constraint_package5_los_angeles, constraint4 = constraint_package5_los_angeles, constraint5 = constraint_package5_los_angeles, constraint6 =constraint_no_intervention_los_angeles)





#intervention 20% work from home
SEIHDQR_9age_simulation_Nairobi_inter_package6_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi_age_effect, init = initials_Nairobi, time = 0:365,outbreak_start_date = as.Date("2020-03-13"), inter_start_date1 = as.Date('2020-03-20'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-09"),inter_end_date4 = as.Date("2020-11-20"), constraint1 = constraint_no_intervention_Nairobi, constraint2 = constraint_package6_Nairobi, constraint3 = constraint_package6_Nairobi, constraint4 = constraint_package6_Nairobi, constraint5 = constraint_package6_Nairobi, constraint6 =constraint_no_intervention_Nairobi)

SEIHDQR_9age_simulation_daegu_inter_package6_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu_age_effect, init = initials_daegu, time = 0:365, outbreak_start_date = as.Date("2020-01-20"), inter_start_date1 = as.Date('2020-01-27'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-09-27"), constraint1 = constraint_no_intervention_daegu, constraint2 = constraint_package6_daegu, constraint3 = constraint_package6_daegu, constraint4 = constraint_package6_daegu, constraint5 = constraint_package6_daegu, constraint6 =constraint_no_intervention_daegu)

SEIHDQR_9age_simulation_nyc_inter_package6_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc_age_effect, init = initials_nyc, time = 0:365, outbreak_start_date = as.Date("2020-03-02"), inter_start_date1 = as.Date('2020-03-09'), inter_start_date2 = as.Date("2020-03-16"), inter_start_date3 = as.Date("2020-03-22"), inter_start_date4 = as.Date("2020-04-02"),inter_end_date4 = as.Date("2020-11-09"), constraint1 = constraint_no_intervention_nyc, constraint2 = constraint_package6_nyc, constraint3 = constraint_package6_nyc, constraint4 = constraint_package6_nyc, constraint5 = constraint_package6_nyc, constraint6 =constraint_no_intervention_nyc)

SEIHDQR_9age_simulation_los_angeles_inter_package6_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles_age_effect, init = initials_los_angeles, time = 0:365, outbreak_start_date = as.Date("2020-01-26"), inter_start_date1 = as.Date('2020-02-02'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-10-02"), constraint1 = constraint_no_intervention_los_angeles, constraint2 = constraint_package6_los_angeles, constraint3 = constraint_package6_los_angeles, constraint4 = constraint_package6_los_angeles, constraint5 = constraint_package6_los_angeles, constraint6 =constraint_no_intervention_los_angeles)





#intervention 20% other
SEIHDQR_9age_simulation_Nairobi_inter_package7_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi_age_effect, init = initials_Nairobi, time = 0:365, outbreak_start_date = as.Date("2020-03-13"), inter_start_date1 = as.Date('2020-03-20'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-09"),inter_end_date4 = as.Date("2020-11-20"), constraint1 = constraint_no_intervention_Nairobi, constraint2 = constraint_package7_Nairobi, constraint3 = constraint_package7_Nairobi, constraint4 = constraint_package7_Nairobi, constraint5 = constraint_package7_Nairobi, constraint6 =constraint_no_intervention_Nairobi)

SEIHDQR_9age_simulation_daegu_inter_package7_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu_age_effect, init = initials_daegu, time = 0:365, outbreak_start_date = as.Date("2020-01-20"), inter_start_date1 = as.Date('2020-01-27'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-09-27"), constraint1 = constraint_no_intervention_daegu, constraint2 = constraint_package7_daegu, constraint3 = constraint_package7_daegu, constraint4 = constraint_package7_daegu, constraint5 = constraint_package7_daegu, constraint6 =constraint_no_intervention_daegu)

SEIHDQR_9age_simulation_nyc_inter_package7_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc_age_effect, init = initials_nyc, time = 0:365,outbreak_start_date = as.Date("2020-03-02"), inter_start_date1 = as.Date('2020-03-09'), inter_start_date2 = as.Date("2020-03-16"), inter_start_date3 = as.Date("2020-03-22"), inter_start_date4 = as.Date("2020-04-02"),inter_end_date4 = as.Date("2020-11-09"), constraint1 = constraint_no_intervention_nyc, constraint2 = constraint_package7_nyc, constraint3 = constraint_package7_nyc, constraint4 = constraint_package7_nyc, constraint5 = constraint_package7_nyc, constraint6 =constraint_no_intervention_nyc)

SEIHDQR_9age_simulation_los_angeles_inter_package7_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles_age_effect, init = initials_los_angeles, time = 0:365,  outbreak_start_date = as.Date("2020-01-26"), inter_start_date1 = as.Date('2020-02-02'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-10-02"), constraint1 = constraint_no_intervention_los_angeles, constraint2 = constraint_package7_los_angeles, constraint3 = constraint_package7_los_angeles, constraint4 = constraint_package7_los_angeles, constraint5 = constraint_package7_los_angeles, constraint6 =constraint_no_intervention_los_angeles)





#===========================Timing effects scenarios====================================================

contact_matrix_9age_without_intervention_nyc <- list(contact_matrix_9age_US$home,contact_matrix_9age_US$school,contact_matrix_9age_US$work,contact_matrix_9age_US$other)
#contact matrix without intervention
#contact matrix policy package 1: 100%*home+100%*school+100%*workplace+90%*others+0%quarantine
contact.matrix_package1_nyc<-list(contact_matrix_9age_US$home,contact_matrix_9age_US$school,contact_matrix_9age_US$work,contact_matrix_9age_US$other*0.9)
#contact matrix policy package 2: 100%*home+0%*school+95%*workplace+80%*others+5%quarantine
contact.matrix_package2_nyc<-list(contact_matrix_9age_US$home,contact_matrix_9age_US$school*0,contact_matrix_9age_US$work,contact_matrix_9age_US$other*0.8)
#contact matrix policy package 3: 100%*home+0%*school+20%*workplace+50%*others+30%quarantine
contact.matrix_package3_nyc<-list(contact_matrix_9age_US$home,contact_matrix_9age_US$school*0,contact_matrix_9age_US$work*0.2,contact_matrix_9age_US$other*0.5)
#contact matrix policy package 4: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine+95%transimissibility
contact.matrix_package4_nyc<-list(contact_matrix_9age_US$home,contact_matrix_9age_US$school*0,contact_matrix_9age_US$work*0.2,contact_matrix_9age_US$other*0.2)


#contact matrix without intervention
contact_matrix_9age_without_intervention_los_angeles <- list(contact_matrix_9age_US$home,contact_matrix_9age_US$school,contact_matrix_9age_US$work,contact_matrix_9age_US$other)
#contact matrix policy package 1: 100%*home+100%*school+90%*workplace+20%*others+0%*quarantine
contact.matrix_package1_los_angeles<-list(contact_matrix_9age_US$home,contact_matrix_9age_US$school,contact_matrix_9age_US$work*0.9,contact_matrix_9age_US$other*0.2)
#contact matrix policy package 2: 100%*home+100%*school+50%*workplace+20%*others+10%*quarantine
contact.matrix_package2_los_angeles<-list(contact_matrix_9age_US$home,contact_matrix_9age_US$school,contact_matrix_9age_US$work*0.5,contact_matrix_9age_US$other*0.2)
#contact matrix policy package 3: 100%*home+0%*school+50%*workplace+20%*others+50%quarantine
contact.matrix_package3_los_angeles<-list(contact_matrix_9age_US$home,contact_matrix_9age_US$school*0,contact_matrix_9age_US$work*0.5,contact_matrix_9age_US$other*0.2)
#contact matrix policy package 4: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine
contact.matrix_package4_los_angeles<-list(contact_matrix_9age_US$home,contact_matrix_9age_US$school*0,contact_matrix_9age_US$work*0.2,contact_matrix_9age_US$other*0.2)

contact_matrix_9age_korea <- readRDS("contact_matrix_9age_korea.rds")
#contact matrix without intervention
contact_matrix_9age_without_intervention_daegu <- list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school,contact_matrix_9age_korea$work,contact_matrix_9age_korea$other)
#contact matrix policy package 1: 100%*home+100%*school+60%*workplace+80%*others+5%*quarantine
contact.matrix_package1_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school,contact_matrix_9age_korea$work,contact_matrix_9age_korea$other)
#contact matrix policy package 2: 100%*home+100%*school+60%*workplace+40%*others+10%*quarantine
contact.matrix_package2_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school*0,contact_matrix_9age_korea$work,contact_matrix_9age_korea$other*0.6)
#contact matrix policy package 3: 100%*home+0%*school+30%*workplace+40%*others+10%quarantine
contact.matrix_package3_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school*0,contact_matrix_9age_korea$work*0.8,contact_matrix_9age_korea$other*0.2)
#contact matrix policy package 4: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine
contact.matrix_package4_daegu<-list(contact_matrix_9age_korea$home,contact_matrix_9age_korea$school*0,contact_matrix_9age_korea$work*0.8,contact_matrix_9age_korea$other*0.2)



contact_matrix_9age_kenya <- readRDS("contact_matrix_9age_kenya.rds")
#contact matrix without intervention
contact_matrix_9age_without_intervention_Nairobi <- list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work,contact_matrix_9age_kenya$other)
#contact matrix policy package 1: 100%*home+100%*school+60%*workplace+80%*others+5%*quarantine
contact.matrix_package1_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work,contact_matrix_9age_kenya$other)
#contact matrix policy package 2: 100%*home+100%*school+60%*workplace+40%*others+10%*quarantine
contact.matrix_package2_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work*0.8,contact_matrix_9age_kenya$other)
#contact matrix policy package 3: 100%*home+0%*school+30%*workplace+40%*others+10%quarantine
contact.matrix_package3_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school,contact_matrix_9age_kenya$work*0.8,contact_matrix_9age_kenya$other*0.4)
#contact matrix policy package 4: 100%*home+0%*school+20%*workplace+20%*others+20%quarantine
contact.matrix_package4_Nairobi<-list(contact_matrix_9age_kenya$home,contact_matrix_9age_kenya$school*0,contact_matrix_9age_kenya$work*0.8,contact_matrix_9age_kenya$other*0.2)

#transmissibility
transmissibility_9age_los_angeles <- calculate_transimissibility(R0t = R0t, recov.rate = recov.rate, pop_age_structure = pop_age_structure_los_angeles, calculate_transmission_probability = 1, contact.matrix = contact_matrix_9age_without_intervention_los_angeles)
transmissibility_9age_nyc <- calculate_transimissibility(R0t = R0t, recov.rate = recov.rate, pop_age_structure = pop_age_structure_nyc, calculate_transmission_probability = 1, contact.matrix = contact_matrix_9age_without_intervention_nyc)
transmissibility_9age_daegu <- calculate_transimissibility(R0t = R0t, recov.rate = recov.rate, pop_age_structure = pop_age_structure_daegu, calculate_transmission_probability = 1, contact.matrix = contact_matrix_9age_without_intervention_daegu)
transmissibility_9age_Nairobi <- calculate_transimissibility(R0t = R0t, recov.rate = recov.rate, pop_age_structure = pop_age_structure_Nairobi, calculate_transmission_probability = 1, contact.matrix = contact_matrix_9age_without_intervention_Nairobi)


##
transm.rate_without_intervention_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact_matrix_9age_without_intervention_nyc)
saveRDS(transm.rate_without_intervention_nyc,"transm.rate_without_intervention_nyc.rds")
transm.rate_package1_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package1_nyc)
saveRDS(transm.rate_package1_nyc,"transm.rate_package1_nyc.rds")
transm.rate_package2_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package2_nyc)
saveRDS(transm.rate_package2_nyc,"transm.rate_package2_nyc.rds")
transm.rate_package3_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package3_nyc)
saveRDS(transm.rate_package3_nyc,"transm.rate_package3_nyc.rds")
transm.rate_package4_nyc <- unlist(transmissibility_9age_nyc)*Reduce('+', contact.matrix_package4_nyc)
saveRDS(transm.rate_package4_nyc,"transm.rate_package4_nyc.rds")


transm.rate_without_intervention_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact_matrix_9age_without_intervention_los_angeles)
saveRDS(transm.rate_without_intervention_los_angeles,"transm.rate_without_intervention_los_angeles.rds")
transm.rate_package1_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package1_los_angeles)
saveRDS(transm.rate_package1_los_angeles,"transm.rate_package1_los_angeles.rds")
transm.rate_package2_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package2_los_angeles)
saveRDS(transm.rate_package2_los_angeles,"transm.rate_package2_los_angeles.rds")
transm.rate_package3_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package3_los_angeles)
saveRDS(transm.rate_package3_los_angeles,"transm.rate_package3_los_angeles.rds")
transm.rate_package4_los_angeles <- unlist(transmissibility_9age_los_angeles)*Reduce('+', contact.matrix_package4_los_angeles)
saveRDS(transm.rate_package4_los_angeles,"transm.rate_package4_los_angeles.rds")


transm.rate_without_intervention_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact_matrix_9age_without_intervention_daegu)
saveRDS(transm.rate_without_intervention_daegu,"transm.rate_without_intervention_daegu.rds")
transm.rate_package1_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package1_daegu)
saveRDS(transm.rate_package1_daegu,"transm.rate_package1_daegu.rds")
transm.rate_package2_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package2_daegu)
saveRDS(transm.rate_package2_daegu,"transm.rate_package2_daegu.rds")
transm.rate_package3_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package3_daegu)
saveRDS(transm.rate_package3_daegu,"transm.rate_package3_daegu.rds")
transm.rate_package4_daegu <- unlist(transmissibility_9age_daegu)*Reduce('+', contact.matrix_package4_daegu)
saveRDS(transm.rate_package4_daegu,"transm.rate_package4_daegu.rds")


transm.rate_without_intervention_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact_matrix_9age_without_intervention_Nairobi)
saveRDS(transm.rate_without_intervention_Nairobi,"transm.rate_without_intervention_Nairobi.rds")
transm.rate_package1_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package1_Nairobi)
saveRDS(transm.rate_package1_Nairobi,"transm.rate_package1_Nairobi.rds")
transm.rate_package2_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package2_Nairobi)
saveRDS(transm.rate_package2_Nairobi,"transm.rate_package2_Nairobi.rds")
transm.rate_package3_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package3_Nairobi)
saveRDS(transm.rate_package3_Nairobi,"transm.rate_package3_Nairobi.rds")
transm.rate_package4_Nairobi <- unlist(transmissibility_9age_Nairobi)*Reduce('+', contact.matrix_package4_Nairobi)
saveRDS(transm.rate_package4_Nairobi,"transm.rate_package4_Nairobi.rds")

#para 9
hosp.rate_Nairobi <- c(0.001,0.003,0.012,0.032,0.049,0.102,0.166,0.243,0.273) #imperial college & Estimates of the severity of coronavirus disease 2019
hosp.rate_daegu <- c(0.001,0.003,0.012,0.032,0.049,0.102,0.166,0.243,0.273) #imperial college & Estimates of the severity of coronavirus disease 2019
hosp.rate_nyc <- c(0.001,0.003,0.012,0.032,0.049,0.102,0.166,0.243,0.273) #imperial college & Estimates of the severity of coronavirus disease 2019
hosp.rate_los_angeles <- c(0.001,0.003,0.012,0.032,0.049,0.102,0.166,0.243,0.273) #imperial college & Estimates of the severity of coronavirus disease 2019
#para 10
fatal.rate_Nairobi = c(0.0026/100,0.0148/100,0.06/100,0.146/100,0.295/100,1.25/100,3.99/100,8.61/100,13.4/100)#c(0.00161/100,0.00695/100,0.0309/100,0.0844/100,0.161/100,0.595/100,1.93/100,4.28/100,7.8/100)#Estimates of the severity of coronavirus disease 2019: a model-based analysis
fatal.rate_daegu = c(0.0026/100,0.0148/100,0.06/100,0.146/100,0.295/100,1.25/100,3.99/100,8.61/100,13.4/100)#c(0.00161/100,0.00695/100,0.0309/100,0.0844/100,0.161/100,0.595/100,1.93/100,4.28/100,7.8/100)#Estimates of the severity of coronavirus disease 2019: a model-based analysis
fatal.rate_nyc = c(0.0026/100,0.0148/100,0.06/100,0.146/100,0.295/100,1.25/100,3.99/100,8.61/100,13.4/100)#c(0.00161/100,0.00695/100,0.0309/100,0.0844/100,0.161/100,0.595/100,1.93/100,4.28/100,7.8/100)#Estimates of the severity of coronavirus disease 2019: a model-based analysis
fatal.rate_los_angeles = c(0.0026/100,0.0148/100,0.06/100,0.146/100,0.295/100,1.25/100,3.99/100,8.61/100,13.4/100)#c(0.00161/100,0.00695/100,0.0309/100,0.0844/100,0.161/100,0.595/100,1.93/100,4.28/100,7.8/100)#Estimates of the severity of coronavirus disease 2019: a model-based analysis

#para 11
quar.rate_Nairobi =c(rep(0.00001,9))
quar.rate_daegu =c(rep(0.00001,9))
quar.rate_nyc =c(rep(0.00001,9))
quar.rate_los_angeles =c(rep(0.00001,9))
#without intervention
parameters_nyc <- list(transm.rate = transm.rate_without_intervention_nyc,
                       incub.rate = incub.rate,#
                       recov.rate = recov.rate,
                       hosp.rate = hosp.rate_nyc,
                       fatal.rate = fatal.rate_nyc,
                       quar.rate = quar.rate_nyc,
                       death.rate = death.rate_nyc,
                       birth.rate = birth.rate_nyc,
                       n = pop_age_structure_nyc*pop_nyc)
saveRDS(parameters_nyc,"parameters_nyc.rds")

parameters_los_angeles <- list(transm.rate = transm.rate_without_intervention_los_angeles,
                               incub.rate = incub.rate,#
                               recov.rate = recov.rate,
                               hosp.rate = hosp.rate_los_angeles,
                               fatal.rate = fatal.rate_los_angeles,
                               quar.rate = quar.rate_los_angeles,
                               death.rate = death.rate_los_angeles,
                               birth.rate = birth.rate_los_angeles,
                               n = pop_age_structure_los_angeles*pop_los_angeles)
saveRDS(parameters_los_angeles,"parameters_los_angeles.rds")

parameters_daegu <- list(transm.rate = transm.rate_without_intervention_daegu,
                         incub.rate = incub.rate,#
                         recov.rate = recov.rate,
                         hosp.rate = hosp.rate_daegu,
                         fatal.rate = fatal.rate_daegu,
                         quar.rate = quar.rate_daegu,
                         death.rate = death.rate_daegu,
                         birth.rate = birth.rate_daegu,
                         n = pop_age_structure_daegu*pop_daegu)
saveRDS(parameters_daegu,"parameters_daegu.rds")

parameters_Nairobi <- list(transm.rate = transm.rate_without_intervention_Nairobi,
                           incub.rate = incub.rate,#
                           recov.rate = recov.rate,
                           hosp.rate = hosp.rate_Nairobi,
                           fatal.rate = fatal.rate_Nairobi,
                           quar.rate = quar.rate_Nairobi,
                           death.rate = death.rate_Nairobi,
                           birth.rate = birth.rate_Nairobi,
                           n = pop_age_structure_Nairobi*pop_Nairobi)
saveRDS(parameters_Nairobi,"parameters_Nairobi.rds")





#No intervention nyc
constraint_no_intervention_nyc<- list(transm.rate = matrix(1,9,9),
                                      incub.rate = c(rep(1,9)),#
                                      recov.rate = c(rep(1,9)),
                                      hosp.rate = c(rep(1,9)),
                                      fatal.rate = c(rep(1,9)),
                                      quar.rate = c(rep(1,9)),
                                      death.rate = c(rep(1,9)), 
                                      birth.rate = c(rep(1,9)),
                                      n = c(rep(1,9)))
saveRDS(constraint_no_intervention_nyc,"constraint_no_intervention_nyc.rds")

#100%*home+100%*school+100%*workplace+90%*others+10%quarantine+95%transimissibility
constraint_package1_nyc<- list(transm.rate = transm.rate_package1_nyc/transm.rate_without_intervention_nyc,
                               incub.rate = c(rep(1,9)),#
                               recov.rate = c(rep(1,9)),
                               hosp.rate = c(rep(1,9)),
                               fatal.rate = c(rep(1,9)),
                               quar.rate = c(rep(10000,9)),
                               death.rate = c(rep(1,9)), 
                               birth.rate = c(rep(1,9)),
                               n = c(rep(1,9)))
saveRDS(constraint_package1_nyc,"constraint_package1_nyc.rds")



#100%*home+0%*school+95%*workplace+50%*others+30%quarantine
constraint_package2_nyc<- list(transm.rate = transm.rate_package2_nyc/transm.rate_without_intervention_nyc,
                               incub.rate = c(rep(1,9)),#
                               recov.rate = c(rep(1,9)),
                               hosp.rate = c(rep(1,9)),
                               fatal.rate = c(rep(1,9)),
                               quar.rate = c(rep(30000,9)),
                               death.rate = c(rep(1,9)), 
                               birth.rate = c(rep(1,9)),
                               n = c(rep(1,9)))

saveRDS(constraint_package2_nyc,"constraint_package2_nyc.rds")

#100%*home+0%*school+20%*workplace+20%*others+50%quarantine

constraint_package3_nyc<- list(transm.rate = transm.rate_package3_nyc/transm.rate_without_intervention_nyc,
                               incub.rate = c(rep(1,9)),#
                               recov.rate = c(rep(1,9)),
                               hosp.rate = c(rep(1,9)),
                               fatal.rate = c(rep(1,9)),
                               quar.rate = c(rep(50000,9)),
                               death.rate = c(rep(1,9)), 
                               birth.rate = c(rep(1,9)),
                               n = c(rep(1,9)))
saveRDS(constraint_package3_nyc,"constraint_package3_nyc.rds")

#100%*home+0%*school+20%*workplace+20%*others+80%quarantine

constraint_package4_nyc <- list(transm.rate = transm.rate_package4_nyc/transm.rate_without_intervention_nyc,
                                incub.rate = c(rep(1,9)),#
                                recov.rate = c(rep(1,9)),
                                hosp.rate = c(rep(1,9)),
                                fatal.rate = c(rep(1,9)),
                                quar.rate = c(rep(80000,9)),
                                death.rate = c(rep(1,9)), 
                                birth.rate = c(rep(1,9)),
                                n = c(rep(1,9)))
saveRDS(constraint_package4_nyc,"constraint_package4_nyc.rds")


#constraint for los angeles
#No intervention
constraint_no_intervention_los_angeles<- list(transm.rate = matrix(1,9,9),
                                              incub.rate = c(rep(1,9)),#
                                              recov.rate = c(rep(1,9)),
                                              hosp.rate = c(rep(1,9)),
                                              fatal.rate = c(rep(1,9)),
                                              quar.rate = c(rep(1,9)),
                                              death.rate = c(rep(1,9)), 
                                              birth.rate = c(rep(1,9)),
                                              n = c(rep(1,9)))
saveRDS(constraint_no_intervention_los_angeles,"constraint_no_intervention_los_angeles.rds")

constraint_package1_los_angeles<- list(transm.rate = transm.rate_package1_los_angeles/transm.rate_without_intervention_los_angeles,
                                       incub.rate = c(rep(1,9)),#
                                       recov.rate = c(rep(1,9)),
                                       hosp.rate = c(rep(1,9)),
                                       fatal.rate = c(rep(1,9)),
                                       quar.rate = c(rep(50000,9)),
                                       death.rate = c(rep(1,9)), 
                                       birth.rate = c(rep(1,9)),
                                       n = c(rep(1,9)))
saveRDS(constraint_package1_los_angeles,"constraint_package1_los_angeles.rds")



constraint_package2_los_angeles<- list(transm.rate = transm.rate_package2_los_angeles/transm.rate_without_intervention_los_angeles,
                                       incub.rate = c(rep(1,9)),#
                                       recov.rate = c(rep(1,9)),
                                       hosp.rate = c(rep(1,9)),
                                       fatal.rate = c(rep(1,9)),
                                       quar.rate = c(rep(50000,9)),
                                       death.rate = c(rep(1,9)), 
                                       birth.rate = c(rep(1,9)),
                                       n = c(rep(1,9)))

saveRDS(constraint_package2_los_angeles,"constraint_package2_los_angeles.rds")



constraint_package3_los_angeles<- list(transm.rate = transm.rate_package3_los_angeles/transm.rate_without_intervention_los_angeles,
                                       incub.rate = c(rep(1,9)),#
                                       recov.rate = c(rep(1,9)),
                                       hosp.rate = c(rep(1,9)),
                                       fatal.rate = c(rep(1,9)),
                                       quar.rate = c(rep(50000,9)),
                                       death.rate = c(rep(1,9)), 
                                       birth.rate = c(rep(1,9)),
                                       n = c(rep(1,9)))
saveRDS(constraint_package3_los_angeles,"constraint_package3_los_angeles.rds")



constraint_package4_los_angeles <- list(transm.rate = transm.rate_package4_los_angeles/transm.rate_without_intervention_los_angeles,
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(80000,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_package4_los_angeles,"constraint_package4_los_angeles.rds")



#constraint for daegu
#No intervention
constraint_no_intervention_daegu<- list(transm.rate = matrix(1,9,9),
                                        incub.rate = c(rep(1,9)),#
                                        recov.rate = c(rep(1,9)),
                                        hosp.rate = c(rep(1,9)),
                                        fatal.rate = c(rep(1,9)),
                                        quar.rate = c(rep(1,9)),
                                        death.rate = c(rep(1,9)), 
                                        birth.rate = c(rep(1,9)),
                                        n = c(rep(1,9)))
saveRDS(constraint_no_intervention_daegu,"constraint_no_intervention_daegu.rds")


constraint_package1_daegu<- list(transm.rate = transm.rate_package1_daegu/transm.rate_without_intervention_daegu*0.98,
                                 incub.rate = c(rep(1,9)),#
                                 recov.rate = c(rep(1,9)),
                                 hosp.rate = c(rep(1,9)),
                                 fatal.rate = c(rep(1,9)),
                                 quar.rate = c(rep(10000,9)),
                                 death.rate = c(rep(1,9)), 
                                 birth.rate = c(rep(1,9)),
                                 n = c(rep(1,9)))
saveRDS(constraint_package1_daegu,"constraint_package1_daegu.rds")




constraint_package2_daegu<- list(transm.rate = transm.rate_package2_daegu/transm.rate_without_intervention_daegu*0.95,
                                 incub.rate = c(rep(1,9)),#
                                 recov.rate = c(rep(1,9)),
                                 hosp.rate = c(rep(1,9)),
                                 fatal.rate = c(rep(1,9)),
                                 quar.rate = c(rep(20000,9)),
                                 death.rate = c(rep(1,9)), 
                                 birth.rate = c(rep(1,9)),
                                 n = c(rep(1,9)))

saveRDS(constraint_package2_daegu,"constraint_package2_daegu.rds")



constraint_package3_daegu<- list(transm.rate = transm.rate_package3_daegu/transm.rate_without_intervention_daegu*0.95,
                                 incub.rate = c(rep(1,9)),#
                                 recov.rate = c(rep(1,9)),
                                 hosp.rate = c(rep(1,9)),
                                 fatal.rate = c(rep(1,9)),
                                 quar.rate = c(rep(30000,9)),
                                 death.rate = c(rep(1,9)), 
                                 birth.rate = c(rep(1,9)),
                                 n = c(rep(1,9)))
saveRDS(constraint_package3_daegu,"constraint_package3_daegu.rds")



constraint_package4_daegu <- list(transm.rate = transm.rate_package4_daegu/transm.rate_without_intervention_daegu*0.95,
                                  incub.rate = c(rep(1,9)),#
                                  recov.rate = c(rep(1,9)),
                                  hosp.rate = c(rep(1,9)),
                                  fatal.rate = c(rep(1,9)),
                                  quar.rate = c(rep(80000,9)),
                                  death.rate = c(rep(1,9)), 
                                  birth.rate = c(rep(1,9)),
                                  n = c(rep(1,9)))
saveRDS(constraint_package4_daegu,"constraint_package4_daegu.rds")




#constraint for Nairobi
#No intervention
constraint_no_intervention_Nairobi<- list(transm.rate = matrix(1,9,9),
                                          incub.rate = c(rep(1,9)),#
                                          recov.rate = c(rep(1,9)),
                                          hosp.rate = c(rep(1,9)),
                                          fatal.rate = c(rep(1,9)),
                                          quar.rate = c(rep(10000,9)),
                                          death.rate = c(rep(1,9)), 
                                          birth.rate = c(rep(1,9)),
                                          n = c(rep(1,9)))
saveRDS(constraint_no_intervention_Nairobi,"constraint_no_intervention_Nairobi.rds")


constraint_package1_Nairobi<- list(transm.rate = transm.rate_package1_Nairobi/transm.rate_without_intervention_Nairobi,
                                   incub.rate = c(rep(1,9)),#
                                   recov.rate = c(rep(1,9)),
                                   hosp.rate = c(rep(1,9)),
                                   fatal.rate = c(rep(1,9)),
                                   quar.rate = c(rep(30000,9)),
                                   death.rate = c(rep(1,9)), 
                                   birth.rate = c(rep(1,9)),
                                   n = c(rep(1,9)))
saveRDS(constraint_package1_Nairobi,"constraint_package1_Nairobi.rds")




constraint_package2_Nairobi<- list(transm.rate = transm.rate_package2_Nairobi/transm.rate_without_intervention_Nairobi,
                                   incub.rate = c(rep(1,9)),#
                                   recov.rate = c(rep(1,9)),
                                   hosp.rate = c(rep(1,9)),
                                   fatal.rate = c(rep(1,9)),
                                   quar.rate = c(rep(30000,9)),
                                   death.rate = c(rep(1,9)), 
                                   birth.rate = c(rep(1,9)),
                                   n = c(rep(1,9)))

saveRDS(constraint_package2_Nairobi,"constraint_package2_Nairobi.rds")



constraint_package3_Nairobi<- list(transm.rate = transm.rate_package3_Nairobi/transm.rate_without_intervention_Nairobi*0.95,
                                   incub.rate = c(rep(1,9)),#
                                   recov.rate = c(rep(1,9)),
                                   hosp.rate = c(rep(1,9)),
                                   fatal.rate = c(rep(1,9)),
                                   quar.rate = c(rep(30000,9)),
                                   death.rate = c(rep(1,9)), 
                                   birth.rate = c(rep(1,9)),
                                   n = c(rep(1,9)))
saveRDS(constraint_package3_Nairobi,"constraint_package3_Nairobi.rds")



constraint_package4_Nairobi <- list(transm.rate = transm.rate_package4_Nairobi/transm.rate_without_intervention_Nairobi*0.95,
                                    incub.rate = c(rep(1,9)),#
                                    recov.rate = c(rep(1,9)),
                                    hosp.rate = c(rep(1,9)),
                                    fatal.rate = c(rep(1,9)),
                                    quar.rate = c(rep(50000,9)),
                                    death.rate = c(rep(1,9)), 
                                    birth.rate = c(rep(1,9)),
                                    n = c(rep(1,9)))
saveRDS(constraint_package4_Nairobi,"constraint_package4_Nairobi.rds")




initials_los_angeles <- c(S. = pop_age_structure_los_angeles*pop_los_angeles-pop_age_structure_los_angeles*pop_los_angeles*0.00001-(pop_age_structure_los_angeles*pop_los_angeles*0.00001)/incub.rate, 
                          E. = c((pop_age_structure_los_angeles*pop_los_angeles*0.00001)/incub.rate),
                          I. = c(pop_age_structure_los_angeles*pop_los_angeles*0.00001),
                          H. = c(rep(0,9)),
                          D. = c(rep(0,9)),
                          Q. = c(rep(0,9)),
                          R. = c(rep(0,9)),
                          Ic. = c(pop_age_structure_los_angeles*pop_los_angeles*0.00001))
saveRDS(initials_los_angeles,"initials_los_angeles.rds")





initials_nyc <- c(S. = pop_age_structure_nyc*pop_nyc-c(pop_age_structure_nyc*pop_nyc*0.0002)-(pop_age_structure_nyc*pop_nyc*0.0002)/incub.rate, 
                  E. = c(c(pop_age_structure_nyc*pop_nyc*0.0002)/incub.rate),
                  I. = c(pop_age_structure_nyc*pop_nyc*0.0002),
                  H. = c(rep(0,9)),
                  D. = c(rep(0,9)),
                  Q. = c(rep(0,9)),
                  R. = c(rep(0,9)),
                  Ic. = c(pop_age_structure_nyc*pop_nyc*0.0002))
saveRDS(initials_nyc,"initials_nyc.rds")





initials_daegu <- c(S. = pop_age_structure_daegu*pop_daegu-c(pop_age_structure_daegu*pop_daegu*0.00005)-c((pop_age_structure_daegu*pop_daegu*0.00005)/incub.rate), 
                    E. = c((pop_age_structure_daegu*pop_daegu*0.00005)/incub.rate),
                    I. = c(pop_age_structure_daegu*pop_daegu*0.00005),
                    H. = c(rep(0,9)),
                    D. = c(rep(0,9)),
                    Q. = c(rep(0,9)),
                    R. = c(rep(0,9)),
                    Ic. = c(pop_age_structure_daegu*pop_daegu*0.00005))
saveRDS(initials_daegu,"initials_daegu.rds")



initials_Nairobi <- c(S. = pop_age_structure_Nairobi*pop_Nairobi-c(pop_age_structure_Nairobi*pop_Nairobi*0.00005)-c((pop_age_structure_Nairobi*pop_Nairobi*0.00005)/incub.rate), 
                      E. = c((pop_age_structure_Nairobi*pop_Nairobi*0.00005)/incub.rate),
                      I. = c(pop_age_structure_Nairobi*pop_Nairobi*0.00005),
                      H. = c(rep(0,9)),
                      D. = c(rep(0,9)),
                      Q. = c(rep(0,9)),
                      R. = c(rep(0,9)),
                      Ic. = c(pop_age_structure_Nairobi*pop_Nairobi*0.00005))
saveRDS(initials_Nairobi,"initials_Nairobi.rds")

#==============================================Los angeles================================================
#no intervention

#simulate the cases
SEIHDQR_9age_simulation_los_angeles_no_inter_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles, init = initials_los_angeles, time = 0:365, outbreak_start_date = as.Date("2020-01-26"), inter_start_date1 = as.Date('2020-03-04'), inter_start_date2 = as.Date("2020-03-12"), inter_start_date3 = as.Date("2020-03-16"), inter_start_date4 = as.Date("2020-03-19"),inter_end_date4 = as.Date("2020-11-04"), constraint1 = constraint_no_intervention_los_angeles, constraint2 = constraint_no_intervention_los_angeles, constraint3 = constraint_no_intervention_los_angeles, constraint4 = constraint_no_intervention_los_angeles, constraint5 = constraint_no_intervention_los_angeles, constraint6 =constraint_no_intervention_los_angeles)

#plots
SEIHDQR_9age_simulation_los_angeles_no_inter_estimate_long <- SEIHDQR_9age_simulation_los_angeles_no_inter_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_los_angeles_no_inter_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_los_angeles_no_inter_estimate_long <- cbind(SEIHDQR_9age_simulation_los_angeles_no_inter_estimate_long, state_age)

SEIHDQR_9age_simulation_los_angeles_no_inter_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_los_angeles_no_inter_estimate_long$time, origin = "2020-01-26")
write.csv(SEIHDQR_9age_simulation_los_angeles_no_inter_estimate_long ,"SEIHDQR_9age_simulation_los_angeles_no_inter_estimate_long.csv",row.names = F)



#simulate the cases
SEIHDQR_9age_simulation_los_angeles_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles, init = initials_los_angeles, time = 0:365, outbreak_start_date = as.Date("2020-01-26"), inter_start_date1 = as.Date('2020-03-04'), inter_start_date2 = as.Date("2020-03-12"), inter_start_date3 = as.Date("2020-03-16"), inter_start_date4 = as.Date("2020-03-19"),inter_end_date4 = as.Date("2020-11-04"), constraint1 = constraint_no_intervention_los_angeles, constraint2 = constraint_package1_los_angeles, constraint3 = constraint_package2_los_angeles, constraint4 = constraint_package3_los_angeles, constraint5 = constraint_package4_los_angeles, constraint6 =constraint_no_intervention_los_angeles)#2 days time lag


#plots
SEIHDQR_9age_simulation_los_angeles_estimate_long <- SEIHDQR_9age_simulation_los_angeles_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_los_angeles_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_los_angeles_estimate_long <- cbind(SEIHDQR_9age_simulation_los_angeles_estimate_long, state_age)
SEIHDQR_9age_simulation_los_angeles_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_los_angeles_estimate_long$time, origin = "2020-01-26")
write.csv(SEIHDQR_9age_simulation_los_angeles_estimate_long ,"SEIHDQR_9age_simulation_los_angeles_estimate_long.csv",row.names = F)




#simulate the cases
SEIHDQR_9age_simulation_los_angeles_1week_earlier_estimate <-SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_los_angeles, init = initials_los_angeles, time = 0:365, outbreak_start_date = as.Date("2020-01-26"), inter_start_date1 = as.Date('2020-02-26'), inter_start_date2 = as.Date("2020-03-05"), inter_start_date3 = as.Date("2020-03-09"), inter_start_date4 = as.Date("2020-03-12"),inter_end_date4 = as.Date("2020-11-04"), constraint1 = constraint_no_intervention_los_angeles, constraint2 = constraint_package1_los_angeles, constraint3 = constraint_package2_los_angeles, constraint4 = constraint_package4_los_angeles, constraint5 = constraint_package4_los_angeles, constraint6 =constraint_no_intervention_los_angeles)# 


#=============================nyc=============================

#no intervention

#simulate the cases
SEIHDQR_9age_simulation_nyc_no_inter_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc, init = initials_nyc, time = 0:365, outbreak_start_date = as.Date("2020-03-02"), inter_start_date1 = as.Date('2020-03-05'), inter_start_date2 = as.Date("2020-03-16"), inter_start_date3 = as.Date("2020-03-22"), inter_start_date4 = as.Date("2020-04-02"),inter_end_date4 = as.Date("2020-11-05"), constraint1 = constraint_no_intervention_nyc, constraint2 = constraint_no_intervention_nyc, constraint3 = constraint_no_intervention_nyc, constraint4 = constraint_no_intervention_nyc, constraint5 = constraint_no_intervention_nyc, constraint6 =constraint_no_intervention_nyc)

#plots
SEIHDQR_9age_simulation_nyc_no_inter_estimate_long <- SEIHDQR_9age_simulation_nyc_no_inter_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_nyc_no_inter_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_nyc_no_inter_estimate_long <- cbind(SEIHDQR_9age_simulation_nyc_no_inter_estimate_long, state_age)
SEIHDQR_9age_simulation_nyc_no_inter_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_nyc_no_inter_estimate_long$time, origin = "2020-03-02")
write.csv(SEIHDQR_9age_simulation_nyc_no_inter_estimate_long ,"SEIHDQR_9age_simulation_nyc_no_inter_estimate_long.csv",row.names = F)



#simulate the cases
SEIHDQR_9age_simulation_nyc_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc, init = initials_nyc, time = 0:365, outbreak_start_date = as.Date("2020-03-02"), inter_start_date1 = as.Date('2020-03-05'), inter_start_date2 = as.Date("2020-03-16"), inter_start_date3 = as.Date("2020-03-22"), inter_start_date4 = as.Date("2020-04-02"),inter_end_date4 = as.Date("2020-11-05"), constraint1 = constraint_no_intervention_nyc, constraint2 = constraint_package1_nyc, constraint3 = constraint_package2_nyc, constraint4 = constraint_package3_nyc, constraint5 = constraint_package4_nyc, constraint6 =constraint_no_intervention_nyc)#2 days time lag # if intervention stops in August, the epidemic will have the second wave in November.


#plots
SEIHDQR_9age_simulation_nyc_estimate_long <- SEIHDQR_9age_simulation_nyc_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_nyc_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_nyc_estimate_long <- cbind(SEIHDQR_9age_simulation_nyc_estimate_long, state_age)
SEIHDQR_9age_simulation_nyc_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_nyc_estimate_long$time, origin = "2020-03-02")
write.csv(SEIHDQR_9age_simulation_nyc_estimate_long ,"SEIHDQR_9age_simulation_nyc_estimate_long.csv",row.names = F)


#intervention came one week earlier
#simulate the cases
SEIHDQR_9age_simulation_nyc_1week_earlier_estimate <-SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_nyc, init = initials_nyc, time = 0:365,  outbreak_start_date = as.Date("2020-03-02"), inter_start_date1 = as.Date('2020-03-05'), inter_start_date2 = as.Date("2020-03-09"), inter_start_date3 = as.Date("2020-03-13"), inter_start_date4 = as.Date("2020-03-26"),inter_end_date4 = as.Date("2020-11-05"), constraint1 = constraint_no_intervention_nyc, constraint2 = constraint_package1_nyc, constraint3 = constraint_package2_nyc, constraint4 = constraint_package4_nyc, constraint5 = constraint_package4_nyc, constraint6 =constraint_no_intervention_nyc)# 


#plots
SEIHDQR_9age_simulation_nyc_1week_earlier_estimate_long <- SEIHDQR_9age_simulation_nyc_1week_earlier_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_nyc_1week_earlier_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_nyc_1week_earlier_estimate_long <- cbind(SEIHDQR_9age_simulation_nyc_1week_earlier_estimate_long, state_age)
SEIHDQR_9age_simulation_nyc_1week_earlier_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_nyc_1week_earlier_estimate_long$time, origin = "2020-03-02")
write.csv(SEIHDQR_9age_simulation_nyc_1week_earlier_estimate_long ,"SEIHDQR_9age_simulation_nyc_1week_earlier_estimate_long.csv",row.names = F)





#=============================Daegu=============================

#no intervention

#simulate the cases
SEIHDQR_9age_simulation_daegu_no_inter_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu, init = initials_daegu, time = 0:365, outbreak_start_date = as.Date("2020-01-20"), inter_start_date1 = as.Date('2020-01-28'), inter_start_date2 = as.Date("2020-02-20"), inter_start_date3 = as.Date("2020-02-25"), inter_start_date4 = as.Date("2020-04-01"),inter_end_date4 = as.Date("2020-09-28"), constraint1 = constraint_no_intervention_daegu, constraint2 = constraint_no_intervention_daegu, constraint3 = constraint_no_intervention_daegu, constraint4 = constraint_no_intervention_daegu, constraint5 = constraint_no_intervention_daegu, constraint6 =constraint_no_intervention_daegu)

#plots
SEIHDQR_9age_simulation_daegu_no_inter_estimate_long <- SEIHDQR_9age_simulation_daegu_no_inter_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_daegu_no_inter_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_daegu_no_inter_estimate_long <- cbind(SEIHDQR_9age_simulation_daegu_no_inter_estimate_long, state_age)
SEIHDQR_9age_simulation_daegu_no_inter_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_daegu_no_inter_estimate_long$time, origin = "2020-01-20")
write.csv(SEIHDQR_9age_simulation_daegu_no_inter_estimate_long ,"SEIHDQR_9age_simulation_daegu_no_inter_estimate_long.csv",row.names = F)


#simulate the cases
SEIHDQR_9age_simulation_daegu_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu, init = initials_daegu, time = 0:365, outbreak_start_date = as.Date("2020-01-20"), inter_start_date1 = as.Date('2020-01-28'), inter_start_date2 = as.Date("2020-02-20"), inter_start_date3 = as.Date("2020-02-25"), inter_start_date4 = as.Date("2020-04-01"),inter_end_date4 = as.Date("2020-09-28"), constraint1 = constraint_no_intervention_daegu, constraint2 = constraint_package1_daegu, constraint3 = constraint_package2_daegu, constraint4 = constraint_package3_daegu, constraint5 = constraint_package4_daegu, constraint6 =constraint_no_intervention_daegu)#2 days time lag # if intervention stops in August, the epidemic will have the second wave in November.


#plots
SEIHDQR_9age_simulation_daegu_estimate_long <- SEIHDQR_9age_simulation_daegu_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_daegu_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_daegu_estimate_long <- cbind(SEIHDQR_9age_simulation_daegu_estimate_long, state_age)
SEIHDQR_9age_simulation_daegu_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_daegu_estimate_long$time, origin = "2020-01-20")
write.csv(SEIHDQR_9age_simulation_daegu_estimate_long ,"SEIHDQR_9age_simulation_daegu_estimate_long.csv",row.names = F)




#intervention came one week earlier
#simulate the cases
SEIHDQR_9age_simulation_daegu_1week_earlier_estimate <-SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_daegu, init = initials_daegu, time = 0:365,  outbreak_start_date = as.Date("2020-01-20"), inter_start_date1 = as.Date('2020-01-28'), inter_start_date2 = as.Date("2020-02-20"), inter_start_date3 = as.Date("2020-02-25"), inter_start_date4 = as.Date("2020-04-01"),inter_end_date4 = as.Date("2020-09-28"), constraint1 = constraint_no_intervention_daegu, constraint2 = constraint_package1_daegu, constraint3 = constraint_package2_daegu, constraint4 = constraint_package4_daegu, constraint5 = constraint_package4_daegu, constraint6 =constraint_no_intervention_daegu)# 


#plots
SEIHDQR_9age_simulation_daegu_1week_earlier_estimate_long <- SEIHDQR_9age_simulation_daegu_1week_earlier_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_daegu_1week_earlier_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_daegu_1week_earlier_estimate_long <- cbind(SEIHDQR_9age_simulation_daegu_1week_earlier_estimate_long, state_age)
SEIHDQR_9age_simulation_daegu_1week_earlier_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_daegu_1week_earlier_estimate_long$time, origin = "2020-01-20")
write.csv(SEIHDQR_9age_simulation_daegu_1week_earlier_estimate_long ,"SEIHDQR_9age_simulation_daegu_1week_earlier_estimate_long.csv",row.names = F)



#=============================Nairobi=============================

#no intervention

#simulate the cases
SEIHDQR_9age_simulation_Nairobi_no_inter_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi, init = initials_Nairobi, time = 0:365, outbreak_start_date = as.Date("2020-03-13"), inter_start_date1 = as.Date('2020-03-15'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-11-15"), constraint1 = constraint_no_intervention_Nairobi, constraint2 = constraint_no_intervention_Nairobi, constraint3 = constraint_no_intervention_Nairobi, constraint4 = constraint_no_intervention_Nairobi, constraint5 = constraint_no_intervention_Nairobi, constraint6 =constraint_no_intervention_Nairobi)

SEIHDQR_9age_simulation_Nairobi_no_inter_estimate_long <- SEIHDQR_9age_simulation_Nairobi_no_inter_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_Nairobi_no_inter_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_Nairobi_no_inter_estimate_long <- cbind(SEIHDQR_9age_simulation_Nairobi_no_inter_estimate_long, state_age)
SEIHDQR_9age_simulation_Nairobi_no_inter_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_Nairobi_no_inter_estimate_long$time, origin = "2020-03-13")
write.csv(SEIHDQR_9age_simulation_Nairobi_no_inter_estimate_long ,"SEIHDQR_9age_simulation_Nairobi_no_inter_estimate_long.csv",row.names = F)




#simulate the cases
SEIHDQR_9age_simulation_Nairobi_estimate <- SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi, init = initials_Nairobi, time = 0:365, outbreak_start_date = as.Date("2020-03-13"), inter_start_date1 = as.Date('2020-03-15'), inter_start_date2 = as.Date("2020-04-01"), inter_start_date3 = as.Date("2020-04-07"), inter_start_date4 = as.Date("2020-04-20"),inter_end_date4 = as.Date("2020-12-31"), constraint1 = constraint_no_intervention_Nairobi, constraint2 = constraint_package1_Nairobi, constraint3 = constraint_package2_Nairobi, constraint4 = constraint_package3_Nairobi, constraint5 = constraint_package4_Nairobi, constraint6 =constraint_no_intervention_Nairobi)#2 days time lag # if intervention stops in August, the epidemic will have the second wave in November.


#plots
SEIHDQR_9age_simulation_Nairobi_estimate_long <- SEIHDQR_9age_simulation_Nairobi_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_Nairobi_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_Nairobi_estimate_long <- cbind(SEIHDQR_9age_simulation_Nairobi_estimate_long, state_age)

SEIHDQR_9age_simulation_Nairobi_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_Nairobi_estimate_long$time, origin = "2020-03-13")
write.csv(SEIHDQR_9age_simulation_Nairobi_estimate_long ,"SEIHDQR_9age_simulation_Nairobi_estimate_long.csv",row.names = F)



#intervention came one week earlier
#simulate the cases
SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate <-SEIHDQR_9age_simulation_6stage_intervention_model(pars = parameters_Nairobi, init = initials_Nairobi, time = 0:365,outbreak_start_date = as.Date("2020-03-13"), inter_start_date1 = as.Date('2020-03-15'), inter_start_date2 = as.Date("2020-03-25"), inter_start_date3 = as.Date("2020-04-01"), inter_start_date4 = as.Date("2020-04-13"),inter_end_date4 = as.Date("2020-12-31"), constraint1 = constraint_no_intervention_Nairobi, constraint2 = constraint_package1_Nairobi, constraint3 = constraint_package2_Nairobi, constraint4 = constraint_package4_Nairobi, constraint5 = constraint_package4_Nairobi, constraint6 =constraint_no_intervention_Nairobi)# 


#plots
SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate_long <- SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate %>% gather(SEIHDQR, estimate, -c(time, period))
state_age<-as.data.frame(do.call(rbind, strsplit(SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate_long$SEIHDQR, "[.]")))
colnames(state_age)<-c("state","age_group")
SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate_long <- cbind(SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate_long, state_age)

SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate_long$time<-as.Date(SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate_long$time, origin = "2020-03-13")
write.csv(SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate_long ,"SEIHDQR_9age_simulation_Nairobi_1week_earlier_estimate_long.csv",row.names = F)
