handle_dictionary_rdb <- function(config, source = NULL){
 
  dict <- NULL
  
  if(is.null(source)){
    errMsg <- "No data task defined as 'source', aborting..."
    config$logger.error(errMsg)
    stop(errMsg)
  }else{
    config$logger.info(sprintf("Source '%s' defined: Parsing RDB dictionary for data task '%s'", source, source))
  
    getDataTaskTablename <- function(task_id){
      table_id <- tolower(gsub("-", "_", gsub("\\.", "_", task_id)))
      return(table_id)
    }
  
    #ref_data
    reftable <-getDataTaskTablename(source)
    ref_data <- sf::st_read(
      config$software$output$dbi, 
      query = sprintf("SELECT countries.geometry, raw.* 
       FROM (SELECT flagstate, species, CAST(EXTRACT(YEAR from time_end) as integer) as year, sum(measurement_value) as measurement_value FROM %s as raw WHERE measurement_type = 'nominal' GROUP BY flagstate, species, year) as raw 
       LEFT JOIN countries ON raw.flagstate = countries.iso_3", reftable)
    )
    
    #dictionnary
    target_columns = colnames(ref_data)
    #create dictionary
    pid <- sprintf("rdb_wecafc_%s", source)
    dict <- geoflow::geoflow_dictionary$new()
    #create feature type
    featuretype <- geoflow::geoflow_featuretype$new(id = pid)
    for(target_column in target_columns){
      target_column_label <- paste0(toupper(substr(target_column,0,1)), substr(target_column,2,nchar(target_column)))
      member <- geoflow::geoflow_featuremember$new(
        type = if(target_column == "measurement_value"){"variable"}else{"attribute"},
        code = target_column,
        name = target_column_label,
        def = target_column_label,
        defSource = NA,
        minOccurs = 0,
        maxOccurs = Inf,
        registerId = if(target_column %in% c("year","measurement_value","geometry")){NA}else{paste0("register_", target_column)} #TODO we should be able to link a standard register here from fdiwg
      )
      featuretype$addMember(member)
    }
    featuretype$addMember(geoflow::geoflow_featuremember$new(
      type = "attribute",
      code = "aggregation_method",
      name = "aggregation_method",
      def = NA,
      defSource = NA,
      minOccurs = 1,
      maxOccurs = 1,
      registerId = "register_aggregation_method"
    ))
    dict$addFeatureType(featuretype)
    
    #registers
    source("https://raw.githubusercontent.com/eblondel/dcf-shiny/main/modules/plugins/wecafc-firms/geoflow/wecafc-firms_geoflow_registers.R")
    handlers = sapply(featuretype$members, function(x){x$registerId})
    handlers <- handlers[!is.na(handlers)]
    for(handler in handlers){
      
      fun <- eval(parse(text = handler))
      if(class(fun)=="try-error"){
        stop(errMsg)
      }
      if(class(fun)!="function"){
        stop(errMsg)
      }
      register_to_fetch <- geoflow::geoflow_register$new(
        id = handler, 
        def = "", 
        fun = fun
      )
      register_to_fetch$fetch(config)
      dict$addRegister(register_to_fetch)
    }
    
    return(dict)
  }
  
}