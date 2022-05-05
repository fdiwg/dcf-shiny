validate_task<-function(file,format,rules){

  task<-read_json(rules)
  
  rules<-task$formats[[format]]$columns
  
  errors<-data.frame(
    type=character(),
    row=character(),
    column=character(),
    category=character(),
    message=character()
  )
  
  tests<-data.frame(
          code=c("E01","E02","E03","E04","I01"),
          name=c("Readable Dataset",
                 "Structure of Dataset",
                 "No missing Values",
                 "Respect to Standards",
                 "Skipped Information")
          )
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file)
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file)
    }else{
      errors<-rbind(errors,data.frame(type="ERROR",rule="E01",row="-",column="-",category="Dataset format not readable",message=sprintf("Dataset can't be read")))
      out = list(
        errors = errors,
        tests= tests,
        valid = FALSE
      )
      
      return(out)
    }
  }
  
  data_names<-names(data)
  
  # GENERIC RULES
  ## ERRORS DETECTION
  
  for (i in names(rules)){
   
     x<-rules[[i]]
   
  ### MANDATORY COLUMNS PRESENCE
    allowed_names<-c(x$id,unlist(x$aliases))
    cond<-any(allowed_names%in%data_names)
    
    if(!cond){
      errors<-rbind(errors,data.frame(type="ERROR",rule="E02",row="-",column=x$id,category="Missing mandatory column",message=sprintf("Column '%s' is missing",x$id)))
    }else{
      usedName<-allowed_names[allowed_names%in%data_names]
      checkedCol<-data[[usedName]]
   
  ### NA VALUES PRESENCE
      if(!x$na_allowed){
        cond<-any(is.na(checkedCol))
        if(cond){
          cond<-all(is.na(checkedCol))
          if(cond){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row="-",column=usedName,category="Not allowed missing value",message=sprintf("All values of '%s' are missing",usedName)))
          }else{
            rows<-which(is.na(checkedCol))
            for(rowid in rows){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row=rowid,column=usedName,category="Not allowed missing value",message=sprintf("Missing values in '%s'",usedName)))
            }
          }
        }
      }
   
   ### VALUES VALIDITY
   #### VALUE IN REFERENTIALS  
   if(!is.null(x$ref)){
     ref<-readr::read_csv(x$ref)
     
     cond<-all(unique(checkedCol[!is.na(checkedCol)])%in%ref$code)
     if(!cond){
       rows<-setdiff(which(!checkedCol%in%ref$code),which(is.na(checkedCol)))
       for(rowid in rows){
         errors<-rbind(errors,data.frame(type="ERROR",rule="E04",row=rowid,column=usedName,category="Invalid value",message=sprintf("value '%s' is not valid value of '%s' referential",checkedCol[rowid],usedName)))
       }
     }
   }else{
   #### SPECIALS RULES
    if(x$id=="catch_unit"){
      valid_unit<-unique(units::valid_udunits()$symbol)   
      cond<-all(unique(checkedCol[!is.na(checkedCol)])%in%valid_unit)
      if(!cond){
        rows<-setdiff(which(!checkedCol%in%valid_unit),which(is.na(checkedCol)))
        for(rowid in rows){
          errors<-rbind(errors,data.frame(type="ERROR",rule="E04",row=rowid,column=usedName,category="Invalid value",message=sprintf("value '%s' is not valid value of '%s' referential",checkedCol[rowid],usedName)))
        }
      }
    }
    if(x$id=="year"){
        cond<-nchar(checkedCol)!=4|!any(startsWith(as.character(checkedCol),c("1","2")))|checkedCol>as.integer(substr(Sys.Date(),1,4))
        if(any(cond)){
          rows<-which(cond)
          for(rowid in rows){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E05",row=rowid,column=usedName,category="Invalid value",message=sprintf("value '%s' is not valid year",checkedCol[rowid])))
          }
        }
      }
    }
   }
   
   data_names<-data_names[!data_names %in% usedName]
  }
  
  ### SUPPLEMENTARIES COLUMNS
  
  for(addcol in data_names){
    errors<-rbind(errors,data.frame(type="INFO",rule="I01",row="-",column=addcol,category="Column not used",message=sprintf("'%s' is not a mandatory column and will be skiped",addcol)))
  }
  
  ### VALIDITY RESULTS
  if(nrow(subset(errors,type=="ERROR"))>0){
    valid<-FALSE
  }else{
    valid<-TRUE
  }
  out = list(
    errors = errors,
    tests = tests, 
    valid = valid
  )
  
  return(out)

}



