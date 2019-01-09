# Round time to 5 min step

Round_5min=function(Datetime)
{
    mins=round(
        difftime(Datetime,
                 as.Date(Datetime),
                 units='mins') %>% 
            as.numeric /5)*5
    
    
    return(as.POSIXct((as.Date(Datetime)+lubridate::minutes(mins)), 
                      format = "%Y-%m-%d %H:%M:%S")
    )
    
}


# Regular 5 min time interval

Regular_5min=function(Dt)
    # First column is DateTime
{
    
    let(list(TIME=colnames(Dt)[1]),
        Dt %<>% 
            mutate(TIME=Round_5min(TIME)) 
    )
    
    let(list(TIME=colnames(Dt)[1]),
        
        Dt %>% 
            select(TIME) %>% 
            summarize(min(TIME),max(TIME)) -> Dt_rng
    )
    
    
    colnames(Dt_rng)=c('MinDt','MaxDt')
    
    Dt_rng %<>% 
        mutate(MinDt=Round_5min(MinDt),
               MaxDt=Round_5min(MaxDt))
    
    Tm_srs=seq(
        from=Dt_rng$MinDt[1],
        to=Dt_rng$MaxDt[1],
        by=300
    ) 
    rm(Dt_rng)
    
    data.frame(Time=Tm_srs) %>%  
        left_join(.,Dt,by=c('Time'=colnames(Dt)[1])) %>% 
        return
    
}



Round_Time=function(Datetime,minuts)
    #minuts:  time step (minutes)
{
    mins=round(
        difftime(Datetime,
                 as.Date(Datetime),
                 units='mins') %>% 
            as.numeric /minuts)*minuts
    
    
    return(as.POSIXct((as.Date(Datetime)+lubridate::minutes(mins)), 
                      format = "%Y-%m-%d %H:%M:%S")
    )
    
}

Regular_Time=function(Dt,minuts)
    #minuts:  time step (minutes)
    # First column is DateTime
{
    
    let(list(TIME=colnames(Dt)[1]),
        Dt %<>% 
            mutate(TIME=Round_Time(TIME,minuts)) 
    )
    
    let(list(TIME=colnames(Dt)[1]),
        
        Dt %>% 
            select(TIME) %>% 
            summarize(min(TIME),max(TIME)) -> Dt_rng
    )
    
    
    colnames(Dt_rng)=c('MinDt','MaxDt')
    
    Dt_rng %<>% 
        mutate(MinDt=Round_Time(MinDt,minuts),
               MaxDt=Round_Time(MaxDt,minuts))
    
    Tm_srs=seq(
        from=Dt_rng$MinDt[1],
        to=Dt_rng$MaxDt[1],
        by=60*minuts
    ) 
    rm(Dt_rng)
    
    data.frame(Time=Tm_srs) %>%  
        left_join(.,Dt,by=c('Time'=colnames(Dt)[1])) %>% 
        return
    
}
