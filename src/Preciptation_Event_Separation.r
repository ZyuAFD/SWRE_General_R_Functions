
#Precip Evt Separation function 

Precip_Evt_Sep= function(dt,T_intv,IntE_P,units='mins')
    #dt:       data of time and rain
    #T_intv:   Time interval of the time series (mins)
    #IntE_P:   Inter event period 
    #           (time step based your time interval
    #            For example: in a 5 min time interval series
    #            a 4 hour inter event period is corresponding to
    #            48 for IntE_P)
    # output: 
    #   Odd Evt_lab: Rain event
    #   Even Evt_lab: Dry event
{
  #The header of time and rain should be
  # Time    Rain
  
    dt %<>% arrange(Time)
    
    # print out the gaps with NA Rain
    print('Here are all the gaps with NA rain.')
    dt %>% 
        filter(is.na(Rain)) %>% 
        arrange(Time) %>% 
        mutate(lag=as.numeric(Time-lag(Time),units=units)) %>% 
        mutate(Gap_St=ifelse(lag>T_intv | is.na(lag),'Start','NA')) %>% 
        mutate(Gap_End=ifelse(lead(lag)>T_intv | is.na(lead(lag)),'End','NA')) %>% 
        mutate(Gap_Lab=(Gap_St=='Start')+(Gap_End=='End')) %>% 
        mutate(Gap_n=(cumsum(Gap_Lab)+1) %/% 2) %>% 
        group_by(Gap_n) %>% 
        summarise(Start=min(Time),
                  End=max(Time)) %>% 
        mutate(Duration_hr=as.numeric(End-Start,units=units)) %>% 
        print
    
    #generate rain events

    data.frame(
        Time=c(min(dt$Time)-minutes(T_intv),
                max(dt$Time)+minutes(T_intv))
        ) %>% 
        bind_rows(dt) %>% 
        Regular_Time(T_intv) %>% 
        replace_na(list(Rain=0)) %>% 
        mutate(Cum_Precip_4hr_L=roll_sum(Rain,IntE_P+1,align='left',fill=0),
               Cum_Precip_4hr_R=roll_sum(Rain,IntE_P+1,align='right',fill=0)) %>% 
        mutate(St_wet=ifelse(lag(Cum_Precip_4hr_R)==0 & Rain>0,1,0),
               St_dry=ifelse(lag(Cum_Precip_4hr_L)>0 & Cum_Precip_4hr_L==0 & Rain==0,1,0)) %>% 
        replace_na(list(St_wet=0,St_dry=0)) %>% 
        mutate(Evt_lab=St_wet+St_dry) %>% 
        mutate(Evt_lab=cumsum(Evt_lab)) %>% 
        select(Time,Rain,Evt_lab) %>% 
        return
}
