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
    mutate(period = ifelse(
      length(seq(time_start, time_end, by = "month"))==1, as.character(as.integer(substring(as.character(time_start),6,7))),
      ifelse(length(seq(time_start, time_end, by = "month"))==3,switch(substring(as.character(time_start),6,7),
                                                                       "01"="Q1",
                                                                       "04"="Q2",
                                                                       "07"="Q3",
                                                                       "10"="Q4",NA),NA)
      ))%>%
    ungroup()%>%
    select(-time_start,-time_end,-time)
  
  return(new_data)
}