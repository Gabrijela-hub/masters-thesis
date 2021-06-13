
library(pricesensitivitymeter)
library(dplyr)

# defining data frames ==== 

Westendorp_control <- experiment_1 %>% 
                                   # select only the control group
                                   .[.$Manipulation_Groups==1,]%>%
                                   # select compelte cases for Westedorp
                                   .[complete.cases(.[,35]),]%>%
                                   # exclude outliers in Too expensive 
                                   .[.$Westendorp_Too_Expensive<50,]

Westendorp_vegan <- experiment_1 %>% 
                                   # select only the vegan group
                                   .[.$Manipulation_Groups==2,]%>%
                                   # select compelte cases for Westedorp
                                   .[complete.cases(.[,35]),]%>%
                                   # exclude outliers in Too expensive 
                                   .[.$Westendorp_Too_Expensive<50,]

Westendorp_vegan_h <- experiment_2 %>% 
                                    # select only the vegan+health group
                                    .[.$Manipulation_Groups==3,]%>%
                                    # select compelte cases for Westedorp
                                    .[complete.cases(.[,35]),]%>%
                                    # exclude outliers in Too expensive 
                                    .[.$Westendorp_Too_Expensive<50,]
Westendorp_vegan_a <- experiment_2 %>% 
                                    # select only the vegan+animals group
                                    .[.$Manipulation_Groups==4,]%>%
                                    # select compelte cases for Westedorp
                                    .[complete.cases(.[,35]),]%>%
                                    # exclude outliers in Too expensive 
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


