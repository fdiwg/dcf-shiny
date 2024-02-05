#simplifiedToGeneric
simplifiedToGeneric <- function(file, format_spec, measurements){
  
  format_spec_cols = sapply(format_spec$column_specs, function(x){x$name})
  
  #read data in case it's not already done
  data <- file
  if(!is.data.frame(file)) data <- readDataFile(file)
  
  newdata_all = do.call("rbind", lapply(measurements, function(measurement){

    attr_target_cols = colnames(data)[sapply(colnames(data), function(x){!any(sapply(measurements, function(m){startsWith(x,m)}))}) ]
    measurement_target_cols = colnames(data)[sapply(colnames(data), function(x){startsWith(x, paste0(measurement, "_"))})]
    target_cols = c(attr_target_cols, measurement_target_cols)
    
    mdata = data[,target_cols]
    names(mdata)<-gsub("_unit","__measurement_unit",names(mdata))
    names(mdata)<-gsub("_status","__measurement_status",names(mdata))
    measurement_types = gsub("_unit", "", measurement_target_cols) %>% unique()
    names(mdata)[names(mdata) %in% measurement_types]<-paste0(names(mdata)[names(mdata) %in% measurement_types],"__measurement_value")
    names(mdata)[grepl("__measurement",names(mdata),fixed = T)]<-gsub(paste0(measurement,"_"),"",names(mdata)[grepl("__measurement",names(mdata),fixed = T)])
    
    #in case we have quarter/month definitions we harmonize it to 'period' for later transformation
    if("quarter" %in% names(mdata)) names(mdata)[names(mdata)=="quarter"] = "period"
    if("month" %in% names(mdata)) names(mdata)[names(mdata)=="month"] = "period"
    
    newdata<-mdata%>%
      tidyr::pivot_longer(names(mdata)[grepl("__measurement",names(mdata),fixed = T)],names_to = c("measurement_type", ".value"), 
                          names_sep="__" )%>%
      mutate(measurement = measurement)%>%
      rowwise()%>%
      mutate(date=ifelse("period"%in%names(mdata),paste0(year,"-",period),paste0(year,"-NA")))%>%
      ungroup()%>%
      mutate(time_start=dateFormating(date,"start"),
             time_end=dateFormating(date,"end"),
             time=paste0(time_start,"/",time_end))
    measurement_block_cols = c("measurement", "measurement_type", "measurement_value", "measurement_unit", "measurement_status")
    mdata_measurement_block_cols = measurement_block_cols[measurement_block_cols %in% colnames(newdata)[startsWith(colnames(newdata),"measurement")]]
    newdata = newdata[,c(colnames(newdata)[!sapply(colnames(newdata), function(x){x %in% mdata_measurement_block_cols})], mdata_measurement_block_cols)]
    
    #check if measurement_unit / measurement_status are available if not initialize them
    if(!"measurement_unit" %in% colnames(newdata)) newdata$measurement_unit <- NA
    if(!"measurement_status" %in% colnames(newdata)) newdata$measurement_status <- NA

    return(newdata)
  }))
  
  return(newdata_all)
}