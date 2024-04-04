#genericToSimplified
genericToSimplified <- function(file){
  
  
  #read data in case it's not already done
  data <- file
  if(!is.data.frame(file)) data <- readDataFile(file)
  
  new_data<-data%>%
    mutate(combine=paste0(measurement,"_",measurement_type))%>%
    select(-measurement,-measurement_type)%>%
    mutate(measurement_value=as.character(measurement_value))%>%
    pivot_longer(c(measurement_unit,measurement_value),names_to="tempo",values_to="measurement_cat")%>%
    mutate(combine=ifelse(endsWith(tempo,"_unit"),paste0(combine,"_unit"),combine))%>%
    select(-tempo)%>%
    pivot_wider(names_from = combine,values_from=measurement_cat,names_sort=T)%>%
    mutate(year=substring(as.character(time_start),1,4))%>%
    rowwise()%>%
    mutate(month=ifelse(length(seq(time_start, time_end, by = "month"))==1,substring(as.character(time_start),6,7),NA),
           quarter=ifelse(length(seq(time_start, time_end, by = "month"))==3,switch(substring(as.character(time_start),6,7),
                                                                                    "01"="1",
                                                                                    "04"="2",
                                                                                    "07"="3",
                                                                                    "10"="4",NA),NA))%>%
    ungroup()%>%
    select(-time_start,-time_end,-time)
  
  if(all(is.na(new_data$month)))new_data$month<-NULL
  if(all(is.na(new_data$quarter)))new_data$quarter<-NULL
  
  return(new_data)
}