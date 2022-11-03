handle_entities_rdb <- function(config, source = NULL){
  
  entities = list()
  
  if(is.null(source)){
    errMsg <- "No data task defined as 'source', aborting..."
    config$logger.error(errMsg)
    stop(errMsg)
  }else{
    config$logger.info(sprintf("Source '%s' defined: Parsing RDB entity for data task '%s'", source, source))
    
    getDataTaskTablename <- function(task_id){
      table_id <- tolower(gsub("-", "_", gsub("\\.", "_", task_id)))
      return(table_id)
    }
    
    out_entity <- switch(source,
      "task-I.2" = {
        #geoflow entity
        entity <- geoflow::geoflow_entity$new()
        
        #ref_data
        reftable <-getDataTaskTablename(source)
        ref_data <- sf::st_read(
          config$software$output$dbi, 
          query = "SELECT countries.geometry, raw.* 
                   FROM (SELECT flagstate, species, CAST(EXTRACT(YEAR from time_end) as integer) as year, sum(measurement_value) as measurement_value FROM task_i_2 as raw WHERE measurement_type = 'nominal' GROUP BY flagstate, species, year) as raw 
                   LEFT JOIN countries ON raw.flagstate = countries.iso_3"
        )
        
        #pid
        pid <- sprintf("rdb_wecafc_%s", source)
        entity$identifiers[["id"]] = pid
        
        #language
        entity$setLanguage("eng")
        
        #descriptions
        entity$setTitle(key = "title", "WECAFC - FIRMS Regional dataset - Nominal catches")
        entity$setDescription(key = "abstract", "This database contains nominal catches data collected from WECAFC-FIRMS country members, through the WECAFC-FIRMS Data Collection Reference Framework (DCRF)")
        
        #subjects
        #General
        gen_subj <- geoflow::geoflow_subject$new()
        gen_subj$setKey("theme")
        gen_subj$setName("General")
        gen_subj$addKeyword("FAO", "http://www.fao.org")
        gen_subj$addKeyword("FAO-NFIS", "http://www.fao.org/fishery")
        gen_subj$addKeyword("fishery")
        gen_subj$addKeyword("fisheries")
        gen_subj$addKeyword("CWP")
        gen_subj$addKeyword("GEMS")
        gen_subj$addKeyword("capture")
        entity$addSubject(gen_subj)
        
        #contacts
        poc = geoflow::geoflow_contact$new();  poc$setId("nfi"); poc$role = "pointOfContact"; entity$addContact(poc);
        poc2 = geoflow::geoflow_contact$new();  poc2$setId("Aureliano.Gentile@fao.org"); poc2$role = "pointOfContact"; entity$addContact(poc2);
        poc3 = geoflow::geoflow_contact$new();  poc3$setId("Yann.Laurent@fao.org"); poc3$role = "pointOfContact"; entity$addContact(poc3);
        poc4 = geoflow::geoflow_contact$new(); poc4$setId("James.Geehan@fao.org"); poc4$role = "pointOfContact"; entity$addContact(poc4);
        md = geoflow::geoflow_contact$new(); md$setId("Emmanuel.Blondel@fao.org"); md$role = "metadata"; entity$addContact(md);
        proc = geoflow::geoflow_contact$new(); proc$setId("Emmanuel.Blondel@fao.org"); proc$role = "processor"; entity$addContact(proc);
        
        #relations
        #TODO thumbnail
        
        #date
        entity$addDate("creation", as(Sys.Date(), "character"))
        
        #type
        entity$setType(type = "dataset")
        
        #spatial
        entity$setSrid(4326)
        entity$setSpatialBbox(data = ref_data)
        entity$setSpatialExtent(data = ref_data)
        
        #dictionnary
        target_columns = colnames(ref_data)
        #create dictionary
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
        
        newconfig = config
        if(is.null(newconfig$metadata$content)) newconfig$metadata$content <- list()
        newconfig$metadata$content$dictionary <- dict
        newconfig$registers <- dict$getRegisters()
        name <- deparse(substitute(config))
        assign(name, newconfig, envir = parent.frame())
        
        #data object
        data_obj <- geoflow::geoflow_data$new()
        data_obj$setFeatureType(pid)
        data_obj$setSourceType("dbtable")
        data_obj$setUploadType("dbquery")
        data_obj$setUpload(FALSE)
        data_obj$setLayername(pid)	
        datasource = reftable
        data_obj$setSource(datasource)
        data_obj$addStyle("generic")
        
        #data features
        data_obj$setFeatures(ref_data)
        
        #add virtual params
        dimensions = c("flagstate", "species", "year", "aggregation_method")
        for(dimension in dimensions){
          regexpValue <- switch(dimension,
                                "year" = "^[\\w. +]+$",
                                "flagstate" = "^[\\w. +]+$",
                                "species" = "^[\\w. +]+$",
                                "aggregation_method"="^[none|avg_by_year|sum]+$",
                                "^[\\w. +]+$"
          )
          values <- NULL
          if(dimension!="aggregation_method"){
            values <- unique(ref_data[[dimension]])
          }
          
          defaultValue <-switch(dimension,
                                "year" = paste(values,  collapse="+"),
                                "flagstate" = paste(values,  collapse="+"),
                                "species" = paste(values,  collapse="+"),
                                "aggregation_method" = "sum"
          )
          data_obj$setParameter(dimension, dimension, regexpValue, defaultValue)
        }
        
        data_obj$setGeometryField("geometry")
        data_obj$setGeometryType("MultiPolygon")
        
        data_obj$setSql("SELECT
            	CASE '%aggregation_method%' WHEN 'none' THEN NULL WHEN 'avg_by_year' THEN geometry WHEN 'sum' THEN geometry ELSE geometry END as geometry,
            	query.* FROM (
            		SELECT 
            		raw.flagstate,
            		CASE '%aggregation_method%' WHEN 'none' THEN raw.species WHEN 'avg_by_year' THEN CAST(NULL as text) WHEN 'sum' THEN CAST(NULL as text) ELSE raw.species END as species, 
            		CASE '%aggregation_method%' WHEN 'none' THEN raw.year WHEN 'avg_by_year' THEN CAST(NULL as integer) WHEN 'sum' THEN CAST(NULL as integer) ELSE raw.year END as year,
            		CASE '%aggregation_method%' WHEN 'none' THEN sum(raw.measurement_value) WHEN 'avg_by_year' THEN sum(raw.measurement_value)/(max(temporal_extent.time)-min(temporal_extent.time)+1) WHEN 'sum' THEN sum(raw.measurement_value) ELSE sum(raw.measurement_value) END as measurement_value
            		from 
            		(SELECT flagstate, species, CAST(EXTRACT(YEAR from time_end) as integer) as year, sum(measurement_value) as measurement_value 
            		 FROM task_i_2 as raw WHERE measurement_type = 'nominal' GROUP BY flagstate, species, year) as raw, 
            		(select regexp_split_to_table(regexp_replace('%year%',' ', '+', 'g'),E'\\\\+')::numeric as time) as temporal_extent
            		WHERE 
            			raw.flagstate IN( select regexp_split_to_table(regexp_replace('%flagstate%',' ', '+', 'g'),E'\\\\+')) AND 
            			raw.species IN( select regexp_split_to_table(regexp_replace('%species%',' ', '+', 'g'),E'\\\\+'))AND 
                  		raw.year IN( select regexp_split_to_table(regexp_replace('%year%',' ', '+', 'g'),E'\\\\+')::numeric) 
            		GROUP BY 
            			raw.flagstate, 
            			CASE '%aggregation_method%' WHEN 'none' THEN raw.species WHEN 'avg_by_year' THEN NULL WHEN 'sum' THEN NULL ELSE raw.species END, 
            			CASE '%aggregation_method%' WHEN 'none' THEN raw.year WHEN 'avg_by_year' THEN NULL WHEN 'sum' THEN NULL ELSE raw.year END
            	) as query
            LEFT JOIN countries ON query.flagstate = countries.iso_3")
        
        #set upload source
        data_obj$setUploadSource(pid)
        
        entity$setData(data_obj);
        
        #assign featureTypeObj!
        featureTypeObj <- dict$getFeatureTypeById(id = pid)
        if(!is.null(featureTypeObj)){
          entity$data$setFeatureTypeObj(featureTypeObj)
        }
        
        entity
      },
      stop(sprintf("No publisher implemented yet for task '%s'", source))
    )
    
    #add entity
    entities = list(out_entity)
  }
  
  return(entities)
}