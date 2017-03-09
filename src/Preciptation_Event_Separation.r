
#Precip Evt Separation function for 5 min interval

Precip_Evt_Sep= function(dt,IntE_P)
    #dt:       data of time and rain
    #IntE_P:   Inter event period 
    #           (time step based your time interval
    #            For example: in a 5 min time interval series
    #            a 4 hour inter event period is corresponding to
    #            48 for IntE_P)
{
  #The header of time and rain should be
  # Time    Rain
  
  dt %>% 
    #select(Time,Rain) %>% 
    mutate(Cum_Precip_4hr_L=roll_sum(Rain,IntE_P+1,align='left',fill=0)-Rain,
         Cum_Precip_4hr_R=roll_sum(Rain,IntE_P+1,align='right',fill=0)-Rain) %>% 
      mutate(St=ifelse(Cum_Precip_4hr_R==0 & Rain>0,1,0),
             End=ifelse(Cum_Precip_4hr_L==0 & Rain>0,1,0)) %>% 
      mutate(Evt_lab=St+End) %>% 
      mutate(Evt_lab=cumsum(Evt_lab)) %>% 
      mutate(Evt_lab=ifelse(Evt_lab %% 2==0 & lag(Evt_lab)<Evt_lab,Evt_lab-1,Evt_lab)) %>% 
      mutate(Evt_lab=ifelse(Evt_lab %% 2==0,0,(Evt_lab+1) %/% 2)) %>%
        select(Time,Rain,Evt_lab) %>% 
        mutate(Evt_lab=ifelse(is.na(Evt_lab),0,Evt_lab)) %>% 
    return
}
