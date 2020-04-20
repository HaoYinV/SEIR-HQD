
##########################################################################
#model of scenario simulations using SEIR-HQD 
#codes for study "Impacts of Early Interventions on the Age-Specific Incidence of COVID-19 in New York, Los Angeles, Daegu and Nairobi"
##########################################################################
#=============================================data analysis and plot graphs=============================================



SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long<-fread("SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long.csv")
SEIHDQR_9age_simulation_4cities_inter_package1_age_effect_estimate_long<-fread("SEIHDQR_9age_simulation_4cities_inter_package1_age_effect_estimate_long.csv") 
SEIHDQR_9age_simulation_4cities_inter_package2_age_effect_estimate_long<-fread("SEIHDQR_9age_simulation_4cities_inter_package2_age_effect_estimate_long.csv") 
SEIHDQR_9age_simulation_4cities_inter_package3_age_effect_estimate_long<-fread("SEIHDQR_9age_simulation_4cities_inter_package3_age_effect_estimate_long.csv")
SEIHDQR_9age_simulation_4cities_inter_package4_age_effect_estimate_long<-fread("SEIHDQR_9age_simulation_4cities_inter_package4_age_effect_estimate_long.csv")
SEIHDQR_9age_simulation_4cities_inter_package5_age_effect_estimate_long<-fread("SEIHDQR_9age_simulation_4cities_inter_package5_age_effect_estimate_long.csv")
SEIHDQR_9age_simulation_4cities_inter_package6_age_effect_estimate_long<-fread("SEIHDQR_9age_simulation_4cities_inter_package6_age_effect_estimate_long.csv")   
SEIHDQR_9age_simulation_4cities_inter_package7_age_effect_estimate_long<-fread("SEIHDQR_9age_simulation_4cities_inter_package7_age_effect_estimate_long.csv")

SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long$policy<-"No Intervention"
SEIHDQR_9age_simulation_4cities_inter_package1_age_effect_estimate_long$policy <- "School closure" #School close
SEIHDQR_9age_simulation_4cities_inter_package2_age_effect_estimate_long$policy <- "50% working from home" #50% work from home
SEIHDQR_9age_simulation_4cities_inter_package3_age_effect_estimate_long$policy <- "50% other mobility" #50% reduction in other mobility
SEIHDQR_9age_simulation_4cities_inter_package4_age_effect_estimate_long$policy <- "10% quarantine rate" #30% quarantine rate
SEIHDQR_9age_simulation_4cities_inter_package5_age_effect_estimate_long$policy <- "Combined policy" #combine Policy ABDF
SEIHDQR_9age_simulation_4cities_inter_package6_age_effect_estimate_long$policy <- "80% working from home" #80% work from home
SEIHDQR_9age_simulation_4cities_inter_package7_age_effect_estimate_long$policy <- "20% other mobility" #80% reduction in other mobility

SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect <- rbind(SEIHDQR_9age_simulation_4cities_inter_package1_age_effect_estimate_long,SEIHDQR_9age_simulation_4cities_inter_package2_age_effect_estimate_long,SEIHDQR_9age_simulation_4cities_inter_package3_age_effect_estimate_long,SEIHDQR_9age_simulation_4cities_inter_package4_age_effect_estimate_long,SEIHDQR_9age_simulation_4cities_inter_package5_age_effect_estimate_long,SEIHDQR_9age_simulation_4cities_inter_package6_age_effect_estimate_long,SEIHDQR_9age_simulation_4cities_inter_package7_age_effect_estimate_long)
SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long$period<-NULL
SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long$policy<-NULL
colnames(SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long)<-c("time","city","no_inter_estimate","state","age_group")
SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar<-merge(SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long,SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect, by=c("time","city","state","age_group"))

SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar$estimate_diff_policy<-SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar$no_inter_estimate-SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar$estimate

SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar$prop_diff_policy<-SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar$estimate_diff_policy/SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar$no_inter_estimate
write.csv(SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar,"SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar.csv",row.names = F)
SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long_sub<-subset(SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long, state=="S"|state=="E"|state=="I"|state=="H"|state=="D"|state=="Q"|state=="R")
neworder<-c("S","E","I","H","D","Q","R")
SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long_sub$state<-factor(SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long_sub$state, levels=neworder)
neworder_city<-c("New York","Los Angeles","Daegu","Nairobi")
SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long_sub$city<-factor(SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long_sub$city, levels=neworder_city)



line_plot_4cities_no_intervention_age_effect_all_states_compar<- ggplot(SEIHDQR_9age_simulation_4cities_no_inter_age_effect_estimate_long_sub, aes(x= as.Date(time), y= no_inter_estimate,  color=as.factor(age_group)))+
  geom_line(aes(linetype = state),alpha=0.8, size=1.4)+
  scale_y_continuous(position = "right")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%b")+
  scale_color_lancet(name = "Age")+
  theme(axis.title = element_text(size = 20),
        panel.background = element_rect(fill="white"),
        strip.text = element_text(size = 20,face="bold", colour = "black"),#strip text size
        legend.text = element_text(size=20),#legend text
        axis.title.x = element_text(size=20, face="bold",margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.title.y = element_text(size=20, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.text.x = element_text(size=20, angle = 90),
        axis.text.y = element_text(size=20, angle = 0))+
  xlab("Time")+ 
  ylab("Real-time cases per million population")+
  facet_grid(state~city, scales = "free")+
  guides(color=guide_legend("Age"))+
  ggtitle("No intervention")

ggsave("line_plot_4cities_no_intervention_age_effect_all_states_compar.pdf", width = 22, height = 20)

neworder_policy<-c("School closure","50% working from home","80% working from home","50% other mobility","20% other mobility","10% quarantine rate","Combined policy")
SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar$policy<-factor(SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar$policy, levels=neworder_policy)


line_plot_4cities_policy_intervention_age_effect_all_states_compar<- ggplot(SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar, aes(x= as.Date(time), y= estimate_diff_policy,  color=as.factor(age_group)))+
  geom_line(aes(linetype = state),alpha=1, size=1.0)+
  scale_y_continuous(position = "right")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%b")+
  scale_color_viridis_d(name = "Age")+
  theme(axis.title = element_text(size = 10),
        panel.background = element_rect(fill="white"),
        
        strip.text = element_text(size = 8,face="bold", colour = "black"),#strip text size
        legend.text = element_text(size=10),#legend text
        axis.title.x = element_text(size=8, face="bold",margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.title.y = element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.text.x = element_text(size=10, angle = 90),
        axis.text.y = element_text(size=10, angle = 0))+
  xlab("Time")+ 
  ylab("Real-time cases per million population")+
  facet_grid(state~city+policy, scales = "free")+
  guides(color=guide_legend("Age"))+
  ggtitle("Policy effects comparison")

ggsave("line_plot_4cities_policy_intervention_age_effect_all_states_compar.pdf", width = 35, height = 24)

I_9age_simulation_4cities_policy_intervention_age_effect_compar<-subset(SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar, state == "I") 


line_plot_4cities_policy_intervention_age_effect_Infected_compar<- ggplot(I_9age_simulation_4cities_policy_intervention_age_effect_compar, aes(x= as.Date(time), y= estimate,  color=as.factor(age_group)))+

  geom_line(aes(linetype = state),alpha=1, size=1.0)+
  
  scale_y_continuous(position = "right")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%b")+
  scale_color_lancet(name = "Age")+
  theme(axis.title = element_text(size = 20),
        panel.background = element_rect(fill="white"),
        strip.text = element_text(size = 20,face="bold", colour = "black"),#strip text size
        legend.text = element_text(size=20),#legend text
        axis.title.x = element_text(size=20, face="bold",margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.title.y = element_text(size=20, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.text.x = element_text(size=20, angle = 90),
        axis.text.y = element_text(size=20, angle = 0))+
  xlab("Time")+ 
  ylab("Real-time cases per million population")+
  facet_grid(city~policy, scales = "free")+
  guides(color=guide_legend("Age"))+
  ggtitle("Policy effects comparison")

ggsave("line_plot_4cities_policy_intervention_age_effect_Infected_compar.pdf", width = 22, height = 20)


line_plot_4cities_no_intervention_age_effect_Infected_compar<- ggplot(I_9age_simulation_4cities_policy_intervention_age_effect_compar, aes(x= as.Date(time), y= no_inter_estimate,  color=as.factor(age_group)))+
  geom_line(aes(linetype = state),alpha=0.2, size=1.0)+
  scale_y_continuous(position = "right")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%b")+
  scale_color_lancet(name = "Age")+
  theme(axis.title = element_text(size = 14),
        panel.background = element_rect(fill="white"),
        strip.text = element_text(size = 14,face="bold", colour = "black"),#strip text size
        legend.text = element_text(size=14),#legend text
        axis.title.x = element_text(size=14, face="bold",margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.title.y = element_text(size=14, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.text.x = element_text(size=14, angle = 90),
        axis.text.y = element_text(size=14, angle = 0))+
  xlab("Time")+ 
  ylab("Real-time cases per million population")+
  facet_grid(city~policy, scales = "free")+
  guides(color=guide_legend("Age"))+
  ggtitle("No intervention comparison")

ggsave("line_plot_4cities_no_intervention_age_effect_Infected_compar.pdf", width = 20, height = 20)


D_9age_simulation_4cities_policy_intervention_age_effect_compar<-subset(SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar, state == "D") 

line_plot_4cities_policy_intervention_age_effect_death_compar<- ggplot(D_9age_simulation_4cities_policy_intervention_age_effect_compar, aes(x= as.Date(time), y= estimate,  color=as.factor(age_group)))+

  geom_line(aes(linetype = state),alpha=1, size=1.0)+
  
  scale_y_continuous(position = "right")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%b")+
  scale_color_lancet(name = "Age")+
  theme(axis.title = element_text(size = 10),
        panel.background = element_rect(fill="white"),
        strip.text = element_text(size = 10,face="bold", colour = "black"),#strip text size
        legend.text = element_text(size=10),#legend text
        axis.title.x = element_text(size=10, face="bold",margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.title.y = element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.text.x = element_text(size=10, angle = 90),
        axis.text.y = element_text(size=10, angle = 0))+
  xlab("Time")+ 
  ylab("Real-time cases per million population")+
  facet_grid(city~policy, scales = "free")+
  guides(color=guide_legend("Age"))+
  ggtitle("Policy effects comparison")

ggsave("line_plot_4cities_policy_intervention_age_effect_death_compar.pdf", width = 18, height = 16)




Ic_4cities_age_effect<-subset(SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar, state=="Ic") #real-time
D_4cities_age_effect<-subset(SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar, state=="D") #cumulative
H_4cities_age_effect<-subset(SEIHDQR_9age_simulation_4cities_policy_intervention_age_effect_compar, state=="H") #cumulative
Ic_D_H_4cities_age_effect<-rbind(Ic_4cities_age_effect,D_4cities_age_effect,H_4cities_age_effect)
write.csv(Ic_D_H_4cities_age_effect,"Ic_D_H_4cities_age_effect.csv",row.names = F)
#edit cumulative to real-time
Ic_4cities_age_effect<-Ic_4cities_age_effect[order(Ic_4cities_age_effect$policy,Ic_4cities_age_effect$city,Ic_4cities_age_effect$age_group,Ic_4cities_age_effect$time)]

Ic_real_time_4cities_age_effect <- Ic_4cities_age_effect %>% group_by(policy, city, age_group) %>% arrange(time) %>% mutate(real_time_inter_estimate=c(9999,diff(estimate)))
Ic_real_time_4cities_age_effect$real_time_inter_estimate<-ifelse(Ic_real_time_4cities_age_effect$real_time_inter_estimate==9999,Ic_real_time_4cities_age_effect$estimate, Ic_real_time_4cities_age_effect$real_time_inter_estimate)


Ic_real_time_4cities_age_effect <- Ic_real_time_4cities_age_effect %>% group_by(policy, city,age_group) %>% arrange(time) %>% mutate(real_time_no_inter_estimate=c(9999,diff(no_inter_estimate)))
Ic_real_time_4cities_age_effect$real_time_no_inter_estimate<-ifelse(Ic_real_time_4cities_age_effect$real_time_no_inter_estimate==9999, Ic_real_time_4cities_age_effect$no_inter_estimate, Ic_real_time_4cities_age_effect$real_time_no_inter_estimate)

Ic_real_time_4cities_age_effect$real_time_estimate_diff<-Ic_real_time_4cities_age_effect$real_time_no_inter_estimate-Ic_real_time_4cities_age_effect$real_time_inter_estimate
write.csv(Ic_real_time_4cities_age_effect,"Ic_real_time_4cities_age_effect.csv",row.names = F)

Ic_real_time_4cities_age_effect<-fread("Ic_real_time_4cities_age_effect.csv")
Ic_real_time_4cities_age_effect<-subset(Ic_real_time_4cities_age_effect, period=="before_intervention1"| period== "during_intervention1" | period=="during_intervention2" | period== "during_intervention3" | period== "during_intervention4")
Ic_real_time_4cities_age_effect_agg<-aggregate(cbind(real_time_inter_estimate,real_time_no_inter_estimate, real_time_estimate_diff)~city+policy+age_group, FUN=sum, na.rm=T,Ic_real_time_4cities_age_effect)
Ic_real_time_4cities_age_effect_agg$real_time_estimate_diff_prop<-Ic_real_time_4cities_age_effect_agg$real_time_estimate_diff/Ic_real_time_4cities_age_effect_agg$real_time_no_inter_estimate

write.csv(Ic_real_time_4cities_age_effect_agg,"Ic_real_time_4cities_age_effect_agg.csv",row.names = F)

Ic_real_time_4cities_age_effect_agg<-fread("Ic_real_time_4cities_age_effect_agg.csv")
neworder_city<-c("New York","Los Angeles","Daegu","Nairobi")
Ic_real_time_4cities_age_effect_agg$city<-factor(Ic_real_time_4cities_age_effect_agg$city, levels=neworder_city)
neworder_policy<-c("School closure","50% working from home","20% working from home","50% other mobility","20% other mobility","10% quarantine rate","Combined policy")
Ic_real_time_4cities_age_effect_agg$policy<-factor(Ic_real_time_4cities_age_effect_agg$policy, levels=neworder_policy)
barplot_real_time_avoided_age <- ggplot(Ic_real_time_4cities_age_effect_agg, aes(x=age_group, y=real_time_estimate_diff/1000))+
  geom_bar(position="dodge", stat="identity", aes(fill=policy))+
  
  geom_text(aes(y=real_time_estimate_diff/1000+15,label = paste0(round(real_time_estimate_diff_prop, 2)), group=policy), position = position_dodge(width=1),angle=90, col = "black",size=5)+
  scale_y_continuous(name = "Avoided infected cases by age (thousand)")+ 
  scale_x_continuous(name = "Age", breaks = seq(1,9,1), labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"))+
  scale_fill_viridis_d(option="plasma", begin = 0.1)+
  facet_wrap(.~city, ncol = 2)+
  theme(axis.title = element_text(size = 22),
        strip.text = element_text(size = 22),
        strip.background = element_rect(size=0.5, fill="grey95"),
        panel.background = element_rect(fill="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 20, face="bold"),
        legend.text = element_text(size=22),
        axis.text = element_text(size=22),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(fill="Policy")
ggsave("barplot_real_time_avoided_age.pdf", width = 22, height = 14)


Ic_real_time_4cities_policy_age_effect_agg<-aggregate(cbind(real_time_inter_estimate,real_time_no_inter_estimate, real_time_estimate_diff)~city+policy, FUN=sum, na.rm=T,Ic_real_time_4cities_age_effect)
write.csv(Ic_real_time_4cities_policy_age_effect_agg,"Ic_real_time_4cities_policy_age_effect_agg.csv",row.names = F)



neworder_city<-c("New York","Los Angeles","Daegu","Nairobi")
Ic_real_time_4cities_age_effect$city<-factor(Ic_real_time_4cities_age_effect$city, levels=neworder_city)

#View(Ic_real_time_4cities_age_effect)
line_plot_avoided_Infect_real_time_4cities_age_effect<- ggplot(Ic_real_time_4cities_age_effect, aes(x= as.Date(time), y= real_time_estimate_diff,  color=as.factor(age_group)))+

  geom_line(aes(linetype = state),alpha=1, size=1.0)+
  scale_y_continuous(position = "right")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%b")+
  scale_color_lancet(name = "Age")+
  theme(axis.title = element_text(size =20),
        panel.background = element_rect(fill="white"),
        strip.text = element_text(size = 20,face="bold", colour = "black"),#strip text size
        legend.text = element_text(size=20),#legend text
        axis.title.x = element_text(size=20, face="bold",margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.title.y = element_text(size=20, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.text.x = element_text(size=20, angle = 90),
        axis.text.y = element_text(size=20, angle = 0))+
  xlab("Time")+ 
  ylab("Real-time avoided cases per million population")+
  facet_grid(city~policy, scales = "free")+
  #xlim(-0.4,0.2)+
  #ylim(-10,30)+
  guides(color=guide_legend("Age"))+
  ggtitle("Policy effects comparison")

ggsave("line_plot_avoided_Infect_real_time_4cities_age_effect.pdf", width = 22, height = 20)


D_4cities_age_effect<-D_4cities_age_effect[order(D_4cities_age_effect$city,D_4cities_age_effect$policy,D_4cities_age_effect$age_group,D_4cities_age_effect$time)]


D_real_time_4cities_age_effect <- D_4cities_age_effect %>% group_by(city, policy, age_group) %>% mutate(real_time_inter_estimate=c(9999,diff(estimate)))
D_real_time_4cities_age_effect$real_time_inter_estimate<-ifelse(D_real_time_4cities_age_effect$real_time_inter_estimate==9999,D_real_time_4cities_age_effect$estimate, D_real_time_4cities_age_effect$real_time_inter_estimate)


D_real_time_4cities_age_effect <- D_real_time_4cities_age_effect %>% group_by(city, policy, age_group) %>% mutate(real_time_no_inter_estimate=c(9999,diff(no_inter_estimate)))
D_real_time_4cities_age_effect$real_time_no_inter_estimate<-ifelse(D_real_time_4cities_age_effect$real_time_no_inter_estimate==9999, D_real_time_4cities_age_effect$no_inter_estimate, D_real_time_4cities_age_effect$real_time_no_inter_estimate)

D_real_time_4cities_age_effect$real_time_estimate_diff<-D_real_time_4cities_age_effect$real_time_no_inter_estimate-D_real_time_4cities_age_effect$real_time_inter_estimate
write.csv(D_real_time_4cities_age_effect,"D_real_time_4cities_age_effect.csv",row.names = F)


D_real_time_4cities_age_effect_agg<-aggregate(cbind(real_time_inter_estimate,real_time_no_inter_estimate, real_time_estimate_diff)~city+policy+age_group, FUN=sum, na.rm=T,D_real_time_4cities_age_effect)
D_real_time_4cities_age_effect_agg$real_time_estimate_diff_prop<-D_real_time_4cities_age_effect_agg$real_time_estimate_diff/D_real_time_4cities_age_effect_agg$real_time_no_inter_estimate

write.csv(D_real_time_4cities_age_effect_agg,"D_real_time_4cities_age_effect_agg.csv",row.names = F)

D_real_time_4cities_policy_age_effect_agg<-aggregate(cbind(real_time_inter_estimate,real_time_no_inter_estimate, real_time_estimate_diff)~city+policy, FUN=sum, na.rm=T,D_real_time_4cities_age_effect)
write.csv(D_real_time_4cities_policy_age_effect_agg,"D_real_time_4cities_policy_age_effect_agg.csv",row.names = F)



neworder_city<-c("New York","Los Angeles","Daegu","Nairobi")
D_real_time_4cities_age_effect$city<-factor(D_real_time_4cities_age_effect$city, levels=neworder_city)

#View(D_real_time_4cities_age_effect)
line_plot_avoided_Death_real_time_4cities_age_effect<- ggplot(D_real_time_4cities_age_effect, aes(x= as.Date(time), y= real_time_estimate_diff,  color=as.factor(age_group)))+
  geom_line(aes(linetype = state),alpha=1, size=1.0)+
  
  scale_y_continuous(position = "right")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%b")+
  scale_color_lancet(name = "Age")+
  theme(axis.title = element_text(size = 10),
        panel.background = element_rect(fill="white"),
        
        strip.text = element_text(size = 10,face="bold", colour = "black"),#strip text size
        legend.text = element_text(size=10),#legend text
        axis.title.x = element_text(size=10, face="bold",margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.title.y = element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.text.x = element_text(size=10, angle = 90),
        axis.text.y = element_text(size=10, angle = 0))+
  xlab("Time")+ 
  ylab("Real-time avoided cases per million population")+
  facet_grid(city~policy, scales = "free")+
  #xlim(-0.4,0.2)+
  #ylim(-10,30)+
  guides(color=guide_legend("Age"))+
  ggtitle("Policy effects comparison")

ggsave("line_plot_avoided_Death_real_time_4cities_age_effect.pdf", width = 18, height = 16)

#cumulative
H_4cities_age_effect<-H_4cities_age_effect[order(H_4cities_age_effect$city,H_4cities_age_effect$policy,H_4cities_age_effect$age_group,H_4cities_age_effect$time)]

write.csv(H_4cities_age_effect,"H_4cities_age_effect.csv",row.names = F)




H_4cities_age_effect_policy_age_agg<-aggregate(estimate_diff_policy~city+policy+age_group, FUN=sum, na.rm=T,H_4cities_age_effect)
H_4cities_age_effect_policy_agg<-aggregate(estimate_diff_policy~city+policy, FUN=sum, na.rm=T,H_4cities_age_effect)
write.csv(H_4cities_age_effect_policy_age_agg,"H_4cities_age_effect_policy_age_agg.csv",row.names = F)
write.csv(H_4cities_age_effect_policy_agg,"H_4cities_age_effect_policy_agg.csv",row.names = F)



neworder_city<-c("New York","Los Angeles","Daegu","Nairobi")
H_4cities_age_effect$city<-factor(H_4cities_age_effect$city, levels=neworder_city)

#View(H_real_time_4cities_age_effect)
line_plot_avoided_Hospitalization_real_time_4cities_age_effect<- ggplot(H_4cities_age_effect, aes(x= as.Date(time), y= estimate_diff_policy,  color=as.factor(age_group)))+
  geom_line(aes(linetype = state),alpha=1, size=1.0)+
  
  scale_y_continuous(position = "right")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%b")+
  scale_color_lancet(name = "Age")+
  # geom_smooth(method = loess,level=0.95, span=0, size=1, alpha=0.3)+
  theme(axis.title = element_text(size = 10),
        panel.background = element_rect(fill="white"),
        strip.text = element_text(size = 10,face="bold", colour = "black"),#strip text size
        legend.text = element_text(size=10),#legend text
        axis.title.x = element_text(size=10, face="bold",margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.title.y = element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.text.x = element_text(size=10, angle = 90),
        axis.text.y = element_text(size=10, angle = 0))+
  xlab("Time")+ 
  ylab("Real-time avoided cases per million population")+
  facet_grid(city~policy, scales = "free")+
  guides(color=guide_legend("Age"))+
  ggtitle("Policy effects comparison")

ggsave("line_plot_avoided_Hospitalization_real_time_4cities_age_effect.pdf", width = 18, height = 16)


line_plot_avoid_H_real_time_4cities_age_effect<- ggplot(H_real_time_4cities_age_effect, aes(x= as.Date(time), y= real_time_no_inter_estimate,  color=as.factor(age_group)))+
  geom_line(aes(linetype = state),alpha=1, size=1.0)+
  
  scale_y_continuous(position = "right")+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 day", date_labels = "%b")+
  scale_color_lancet(name = "Age")+
  theme(axis.title = element_text(size = 10),
        panel.background = element_rect(fill="white"),
        strip.text = element_text(size = 10,face="bold", colour = "black"),#strip text size
        legend.text = element_text(size=10),#legend text
        axis.title.x = element_text(size=10, face="bold",margin = margin(t = 20, r = 0, b = 20, l = 0)),
        axis.title.y = element_text(size=10, face="bold", margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.text.x = element_text(size=10, angle = 90),
        axis.text.y = element_text(size=10, angle = 0))+
  xlab("Time")+ 
  ylab("Real-time avoided cases per million population")+
  facet_grid(city~policy, scales = "free")+
  guides(color=guide_legend("Age"))+
  ggtitle("Policy effects comparison")

ggsave("line_plot_4cities_policy_intervention_age_hospitalization_effect_compar.pdf", width = 18, height = 16)