validate_task<-function(file,format,rules){

  task<-jsonlite::read_json(rules)
  
  rules<-task$formats[[format]]$columns
  
  errors<-data.frame(
    type=character(),
    row=character(),
    column=character(),
    category=character(),
    message=character()
  )
  
  tests<-data.frame(
          code=c("E01","E02","E03","E04","E05","I01"),
          name=c("Readable Dataset",
                 "Structure of Dataset",
                 "No missing Values",
                 "Categorial Values Respect Standards",
                 "Valid Dates",
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
  generic_cols<-names(unlist(lapply(rules, function(x){x$rule_type[x$rule_type=="generic"]})))
  special_cols<-names(unlist(lapply(rules, function(x){x$rule_type[x$rule_type=="special"]})))
  
  # ERRORS DETECTION
  ##MANDATORIES COLUMNS
  
  for (i in generic_cols){
   
     x<-rules[[i]]
   
  ### COLUMNS PRESENCE
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
   #### VALUES IN REFERENTIALS  
   if(!is.null(x$ref)){
     ref<-readr::read_csv(x$ref)
     cond<-all(unique(checkedCol[!is.na(checkedCol)])%in%ref$code)
     if(!cond){
       rows<-setdiff(which(!checkedCol%in%ref$code),which(is.na(checkedCol)))
       for(rowid in rows){
         errors<-rbind(errors,data.frame(type="ERROR",rule="E04",row=rowid,column=usedName,category="Invalid value",message=sprintf("Value '%s' is not a valid value of '%s' referential",checkedCol[rowid],usedName)))
       }
     }
   }
   # #### VALUE IN ALLOWED VALUES
     if(!is.null(x$allowed_values)){
       ref<-unlist(x$allowed_values)
       cond<-all(unique(checkedCol[!is.na(checkedCol)])%in%ref)
       if(!cond){
         rows<-setdiff(which(!checkedCol%in%ref),which(is.na(checkedCol)))
         for(rowid in rows){
           errors<-rbind(errors,data.frame(type="ERROR",rule="E04",row=rowid,column=usedName,category="Invalid value",message=sprintf("Value '%s' is not a valid value of '%s' referential",checkedCol[rowid],usedName)))
         }
       }
     }
   #### ADDITIONALS RULES
   ##### UNITS VALIDITY
    if(x$id%in%c("catch_unit","measurement_unit")){
      valid_unit<-unique(units::valid_udunits()$symbol)   
      cond<-all(unique(checkedCol[!is.na(checkedCol)])%in%valid_unit)
      if(!cond){
        rows<-setdiff(which(!checkedCol%in%valid_unit),which(is.na(checkedCol)))
        for(rowid in rows){
          errors<-rbind(errors,data.frame(type="ERROR",rule="E04",row=rowid,column=usedName,category="Invalid value",message=sprintf("value '%s' is not valid value of '%s' referential",checkedCol[rowid],usedName)))
        }
      }
    }
   ##### YEAR VALIDITY
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
   
   data_names<-data_names[!data_names %in% usedName]
  }
  
  #CONDITIONALS COLUMNS
  ## PERID VALIDITY
  if(all(c("time","time_start","time_end")%in% special_cols)){
  
  ## TIME ON COLUMN FORMAT  
    date_cols<-list()
    for (i in c("time","time_start","time_end")){
      x<-rules[[i]]
      type<-ifelse(i=="time","combined","separated")
      allowed_names<-c(x$id,unlist(x$aliases))
      cond<-any(allowed_names%in%data_names)
      if(cond){
        usedName<-allowed_names[allowed_names%in%data_names]
        checkedCol<-data[[usedName]]
        cond<-any(is.na(checkedCol))
        if(!cond){
          date_cols[[type]][i]<-list(checkedCol)
        }else{
          date_cols[[type]][i]<-list(NULL)
        }
      }else{
        date_cols[[type]][i]<-list(NULL)
      }
    }
    
    if(is.null(date_cols$combined$time)&is.null(date_cols$separated$time_start)&is.null(date_cols$separated$time_end)){
      errors<-rbind(errors,data.frame(type="ERROR",rule="E02",row="-",column="time",category="Missing mandatory column",message="'time' or 'time_start' and 'time_end' columns are missing"))
    }else{
      if(!is.null(date_cols$combined$time)){
        #COMBINED TIME
        
        checkedCol<-date_cols$combined$time
        
        x<-rules[["time"]]
        allowed_names<-c(x$id,unlist(x$aliases))
        usedName<-allowed_names[allowed_names%in%data_names]
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
        
        ### VALUE FORMAT
        decode_date<-strsplit(checkedCol,"/")
        
        cond<-sapply(decode_date,function(x){
        time_start<-x[1]
        valid_start<-grepl("(((20[012]\\d|19\\d\\d)|(1\\d|2[0123]))-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])))",time_start)
        time_end<-x[2]
        valid_end<-grepl("(((20[012]\\d|19\\d\\d)|(1\\d|2[0123]))-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])))",time_start)
        if(valid_start&valid_end){
          date_order<-as.Date(time_end)>=as.Date(time_start)
          if(date_order){
            date_impossible<-(as.Date(time_start)>Sys.Date()|as.Date(time_end)>Sys.Date())  
            if(date_impossible){
              return("Date after today")
            }else{
              return("valid")
            }
          }else{
            return("Date end before date start")
          }
        }else{
          return("Invalid date format")
        }
        })
        if(any(cond!="valid")){
          rows<-which(cond!="valid")
          for(rowid in rows){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E05",row=rowid,column="time",category="Invalid date",message=cond[rowid]))
          }
        }
       
        data_names<-data_names[!data_names %in% usedName]   
        
      }else{
        #SEPARATED TIME
        if(is.null(date_cols$separated$time_start)|is.null(date_cols$separated$time_end)){
          if(is.null(date_cols$separated$time_start)){
            #TIME START MISSING
            errors<-rbind(errors,data.frame(type="ERROR",rule="E02",row="-",column="time_start",category="Missing mandatory column",message="'time_end' is completed but 'time_start' is missing"))
          }else{
            #TIME END MISSING
            errors<-rbind(errors,data.frame(type="ERROR",rule="E02",row="-",column="time_end",category="Missing mandatory column",message="'time_start' is completed but 'time_end' is missing"))
          }
        }else{
            #BOTH COLUMN COMPLETED
          
          #TIME START
          time_start<-date_cols$separated$time_start
          x<-rules[["time_start"]]
          allowed_names<-c(x$id,unlist(x$aliases))
          usedName<-allowed_names[allowed_names%in%data_names]
          ### NA VALUES PRESENCE
          if(!x$na_allowed){
            cond<-any(is.na(time_start))
            if(cond){
              cond<-all(is.na(time_start))
              if(cond){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row="-",column=usedName,category="Not allowed missing value",message=sprintf("All values of '%s' are missing",usedName)))
              }else{
                rows<-which(is.na(time_start))
                for(rowid in rows){
                  errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row=rowid,column=usedName,category="Not allowed missing value",message=sprintf("Missing values in '%s'",usedName)))
                }
              }
            }
          }
          valid_start<-grepl("(((20[012]\\d|19\\d\\d)|(1\\d|2[0123]))-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])))",time_start)
          if(all(valid_start)){
            date_impossible<-as.Date(time_start)>Sys.Date()
            if(any(date_impossible)){
              rows<-which(date_impossible)
              for(rowid in rows){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E05",row=rowid,column=usedName,category="Invalid date",message="Date after today"))
              }
            }
          }else{
            rows<-which(!valid_start)
            for(rowid in rows){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E05",row=rowid,column=usedName,category="Invalid date",message="Invalid date format"))
            }
          }
          
          data_names<-data_names[!data_names %in% usedName]
          
          #TIME END
          time_end<-date_cols$separated$time_end
          x<-rules[["time_end"]]
          allowed_names<-c(x$id,unlist(x$aliases))
          usedName<-allowed_names[allowed_names%in%data_names]
          ### NA VALUES PRESENCE
          if(!x$na_allowed){
            cond<-any(is.na(time_end))
            if(cond){
              cond<-all(is.na(time_end))
              if(cond){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row="-",column=usedName,category="Not allowed missing value",message=sprintf("All values of '%s' are missing",usedName)))
              }else{
                rows<-which(is.na(time_end))
                for(rowid in rows){
                  errors<-rbind(errors,data.frame(type="ERROR",rule="E03",row=rowid,column=usedName,category="Not allowed missing value",message=sprintf("Missing values in '%s'",usedName)))
                }
              }
            }
          }
          valid_end<-grepl("(((20[012]\\d|19\\d\\d)|(1\\d|2[0123]))-((0[0-9])|(1[012]))-((0[1-9])|([12][0-9])|(3[01])))",time_end)
          if(all(valid_end)){
            date_impossible<-as.Date(time_end)>Sys.Date()
            if(any(date_impossible)){
              rows<-which(date_impossible)
              for(rowid in rows){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E05",row=rowid,column=usedName,category="Invalid date",message="Date after today"))
              }
            }
          }else{
            rows<-which(!valid_end)
            for(rowid in rows){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E05",row=rowid,column=usedName,category="Invalid date",message="Invalid date format"))
            }
          }
          
          data_names<-data_names[!data_names %in% usedName]

          if(all(valid_start)&all(valid_end)){
            date_order<-as.Date(time_end)>=as.Date(time_start)
            if(!all(date_order)){
              rows<-which(!date_order)
              for(rowid in rows){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E05",row=rowid,column="-",category="Invalid date","Date end before date start"))
              }
            }
          }
            
        }
      }
    }
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



