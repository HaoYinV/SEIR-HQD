library(EpiEstim)
library(coda)
library(data.table)
#SI estimate from data
covid_cases_deaths_si<-fread("serial-interval-china.csv")
covid_cases_deaths_si<-covid_cases_deaths_si[,c("Index - symptom onset date","Secondary - symptom onset date")]
covid_cases_deaths_si$SL<-as.Date(covid_cases_deaths_si$`Secondary - symptom onset date`,"%m/%d/%Y")-as.Date(covid_cases_deaths_si$`Index - symptom onset date`,"%m/%d/%Y")
covid_cases_deaths_si<-covid_cases_deaths_si[which(covid_cases_deaths_si[,SL]>=0)]
covid_cases_deaths_si$SR<-covid_cases_deaths_si$SL+1

si_data_china<-data.frame(EL=as.integer(rep(0,nrow(covid_cases_deaths_si))),ER=as.integer(rep(1,nrow(covid_cases_deaths_si))),SL=as.integer(covid_cases_deaths_si$SL),SR=as.integer(covid_cases_deaths_si$SR))
si_data_china$type<-0
write.csv(si_data_china,"si_data_china.csv",row.names = F)




load("covid19_infected_dead.RData") #load data on infected and dead cases in each city

covid_si<-fread("si_data_china.csv") 
incid <- covid19_infected_dead_la$case_ma #seven days moving average of infected cases
incid <- incid[incid>0.5] 
plot(incid)
mcmc_seed<-1
overall_seed<-2
method = "si_from_data"
mcmc_control <- make_mcmc_control(burnin = 1000, thin = 100, seed = mcmc_seed)
t_start <- seq(2, length(incid) - 6)
t_end <- t_start + 6
config <- make_config(incid = incid,t_start=t_start, t_end=t_end, method = method, si_parametric_distr = "G", mcmc_control = mcmc_control, n1 = 100, n2 = 50, seed = overall_seed)
#' \item{n1}{For method "uncertain_si" and "si_from_data"; positive integer
#' giving the size of the sample of SI distributions to be drawn (see details).}
#' 
#' \item{n2}{For methods "uncertain_si", "si_from_data" and "si_from_sample";
#' positive integer giving the size of the sample drawn from the posterior
#' distribution of R for each serial interval distribution considered (see
#' details).}
R_si_from_data<- estimate_R(incid, method = method, si_data = covid_si, config = config)
saveRDS(R_si_from_data,"R_si_from_data_la.rds")


#coda::traceplot(as.mcmc(R_si_from_data$R))
#coda::autocorr.plot(as.mcmc(R_si_from_data$R), lag.max=1000)
pdf(file = "Rt_plot_la.pdf", width = 10, height = 16)
Rt_plot_la <- plot(R_si_from_data)
dev.off()
R_si_from_data_la<- readRDS("R_si_from_data_la.rds")
#mean(R_si_from_data_la$R[1,]$`Mean(R)`)  #3.19



#new york
incid <- covid19_infected_dead_ny$case_ma

incid <- incid[incid>0]
mcmc_seed<-1
overall_seed<-2
method = "si_from_data"
mcmc_control <- make_mcmc_control(burnin = 1000, thin = 100, seed = mcmc_seed)
t_start <- seq(2, length(incid) - 6)
t_end <- t_start + 6
config <- make_config(incid = incid,t_start=t_start, t_end=t_end, method = method, si_parametric_distr = "G", mcmc_control = mcmc_control, n1 = 100, n2 = 50, seed = overall_seed)

R_si_from_data<- estimate_R(incid, method = method, si_data = covid_si, config = config)
saveRDS(R_si_from_data,"R_si_from_data_ny.rds")

coda::traceplot(as.mcmc(R_si_from_data$R))
coda::autocorr.plot(as.mcmc(R_si_from_data$R), lag.max=500)
pdf(file = "Rt_plot_ny.pdf", width = 10, height = 16)
Rt_plot_ny <- plot(R_si_from_data)
dev.off()

R_si_from_data_ny <- readRDS("R_si_from_data_ny.rds")

mean(R_si_from_data_ny$R[1:30,]$`Mean(R)`)  #3.6


#daegu
incid <- covid19_infected_dead_daegu$case_ma
incid <- incid[incid>0.5]
plot(incid)
mcmc_seed<-1
overall_seed<-2
method = "si_from_data"
mcmc_control <- make_mcmc_control(burnin = 1000, thin = 100, seed = mcmc_seed)
t_start <- seq(2, length(incid) - 6)
t_end <- t_start + 6
config <- make_config(incid = incid,t_start=t_start, t_end=t_end, method = method, si_parametric_distr = "G", mcmc_control = mcmc_control, n1 = 100, n2 = 50, seed = overall_seed)

R_si_from_data<- estimate_R(incid, method = method, si_data = covid_si, config = config)
saveRDS(R_si_from_data,"R_si_from_data_daegu.rds")

coda::traceplot(as.mcmc(R_si_from_data$R))
coda::autocorr.plot(as.mcmc(R_si_from_data$R), lag.max=500)
pdf(file = "Rt_plot_daegu.pdf", width = 10, height = 16)
Rt_plot_daegu <- plot(R_si_from_data)
dev.off()
R_si_from_data_daegu<- readRDS("R_si_from_data_daegu.rds")
#mean(R_si_from_data_daegu$R[1:14,]$`Mean(R)`)  #3.75



#nairobi
incid <- covid19_infected_dead_nairobi$case_ma
incid <- incid[incid>0.5]
plot(incid)
mcmc_seed<-1
overall_seed<-2
method = "si_from_data"
mcmc_control <- make_mcmc_control(burnin = 1000, thin = 100, seed = mcmc_seed)
t_start <- seq(2, length(incid) - 6)
t_end <- t_start + 6
config <- make_config(incid = incid,t_start=t_start, t_end=t_end, method = method, si_parametric_distr = "G", mcmc_control = mcmc_control, n1 = 100, n2 = 50, seed = overall_seed)

R_si_from_data<- estimate_R(incid, method = method, si_data = covid_si, config = config)
saveRDS(R_si_from_data,"R_si_from_data_nairobi.rds")
R_nairobi<- readRDS("R_si_from_data_nairobi.rds")
