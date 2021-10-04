
library(pricesensitivitymeter)
library(dplyr)
library(ggplot2)
# defining data frames ==== 

Westendorp_control <- experiment_1 %>% 
                                   # select only the control group (N=205)
                                   .[.$Manipulation_Groups==1,]%>%
                                   # select compelte cases for Westedorp (N=186)
                                   .[complete.cases(.[,35]),]%>%
                                   # exclude outliers in Too expensive (N=184) 
                                   .[.$Westendorp_Too_Expensive<50,]

Westendorp_vegan <- experiment_1 %>% 
                                   # select only the vegan group (N=195)
                                   .[.$Manipulation_Groups==2,]%>%
                                   # select compelte cases for Westedorp (N=185)
                                   .[complete.cases(.[,35]),]%>%
                                   # exclude outliers in Too expensive (N=180) 
                                   .[.$Westendorp_Too_Expensive<50,]

Westendorp_vegan_h <- experiment_2 %>% 
                                    # select only the vegan+health group (N=167)
                                    .[.$Manipulation_Groups==3,]%>%
                                    # select compelte cases for Westedorp (N=153)
                                    .[complete.cases(.[,35]),]%>%
                                    # exclude outliers in Too expensive (N=145)
                                    .[.$Westendorp_Too_Expensive<50,]

Westendorp_vegan_a <- experiment_2 %>% 
                                    # select only the vegan+animals group (N=180)
                                    .[.$Manipulation_Groups==4,]%>%
                                    # select compelte cases for Westedorp (N=164)
                                    .[complete.cases(.[,35]),]%>%
                                    # exclude outliers in Too expensive (N=160)
                                    .[.$Westendorp_Too_Expensive<50,]

#PSM analysis ====

# getting Westendorp analysis parameters

psm_control <- psm_analysis(toocheap='Westendorp_Too_Cheap', 
                             cheap= 'Westendorp_Cheap',
                             expensive='Westendorp_Expensive',
                             tooexpensive='Westendorp_Too_Expensive',
                             data=Westendorp_control)

psm_vegan <- psm_analysis(toocheap='Westendorp_Too_Cheap', 
                            cheap= 'Westendorp_Cheap',
                            expensive='Westendorp_Expensive',
                            tooexpensive='Westendorp_Too_Expensive',
                            data=Westendorp_vegan)

psm_vegan_h <- psm_analysis(toocheap='Westendorp_Too_Cheap', 
                          cheap= 'Westendorp_Cheap',
                          expensive='Westendorp_Expensive',
                          tooexpensive='Westendorp_Too_Expensive',
                          data=Westendorp_vegan_h)

psm_vegan_a <- psm_analysis(toocheap='Westendorp_Too_Cheap', 
                            cheap= 'Westendorp_Cheap',
                            expensive='Westendorp_Expensive',
                            tooexpensive='Westendorp_Too_Expensive',
                            data=Westendorp_vegan_a)


# plotting control and vegan PSM

plot_control <- psm_plot(psm_control, label_idp = F, label_opp = F)
plot_vegan <- psm_plot(psm_vegan, label_idp = F, label_opp = F)

plot_control + theme_minimal()


plot_control_finished <- psm_plot(psm_control, 
                         label_idp = F, label_opp = F, 
                         pricerange_alpha = 0.05,
                         line_type = c("too cheap" = "solid",
                                       "not cheap" = "solid",
                                       "not expensive" = "solid",
                                       "too expensive" = "solid"),
                         line_color = c("too cheap" = "#66a182",
                                        "not cheap" = "#00798c",
                                        "not expensive" = "#edae49",
                                        "too expensive" = "#d1495b")) + 
                          theme_classic() + ggtitle("Control")+
                          labs(y= "Cumulative Percentage (%)", x = "Price")+ 
                          theme(plot.title = element_text(hjust = 0.5),legend.position = c(0.8, 0.4)) 
                  
plot_vegan_finished <- psm_plot(psm_vegan, 
                                  label_idp = F, label_opp = F, 
                                  pricerange_alpha = 0.05,
                                  line_type = c("too cheap" = "solid",
                                                "not cheap" = "solid",
                                                "not expensive" = "solid",
                                                "too expensive" = "solid"),
                                  line_color = c("too cheap" = "#66a182",
                                                 "not cheap" = "#00798c",
                                                 "not expensive" = "#edae49",
                                                 "too expensive" = "#d1495b")) + 
                                  theme_classic() + 
                                  labs(title = "Vegan",y= "Cumulative Percentage (%)", x = "Price")+ 
                                  theme(legend.position = c(0.8, 0.4)) 
                                  
plot_control_finished
