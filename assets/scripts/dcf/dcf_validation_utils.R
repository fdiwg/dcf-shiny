

#getTasks
getTasks <- function(config,withId=FALSE){
  tasks <- config$dcf$tasks
  task_list <- names(tasks)
  if(withId){
    task_list<- setNames(task_list, sprintf("%s [%s]",unlist(lapply(tasks, function(x){x$name})),task_list))
  }else{
    task_list<- setNames(task_list, unlist(lapply(tasks, function(x){x$name})))
  }
  return(task_list)
}

#getTaskProperties
getTaskProperties <- function(config,id){
  task <- config$dcf$tasks[[id]]
  return(task)
}

#eval_variable_expression
eval_variable_expression <- function(str){
  eval_str <- eval(parse(text = whisker::whisker.render(str, as.list(Sys.getenv()))))
  return(eval_str)
}
#data_time
data_time <- function(start_year, end_year){
  as.Date(sprintf("%s-01-01/%s-12-31", start_year, end_year))
}
#data_time_start
data_time_start <- function(year){
  as.Date(sprintf("%s-01-01", year))
}
#data_time_end
data_time_end <- function(year){
  as.Date(sprintf("%s-12-31", year))
}

#qtodate
qtodate<- function(date,period="start"){
  
  date<-unlist(strsplit(date,"-") )
  
  Y<-date[1]
  Q<-date[2]
  
  Q<-switch (Q,
             Q1=c('01-01','03-31'),
             Q2=c('04-01','06-30'),
             Q3=c('07-01','09-30'),
             Q4=c('10-01','12-31')
  )
  
  date<-paste0(Y,"-",Q)
  
  if(period=="start")return(date[1])
  if(period=="end")return(date[2])
  if(period=="start+end")return(paste0(date[1],"/",date[2]))
  
}

#ymtodate
ymtodate<- function(date,period="start"){
  start_date<-paste0(date,"-01")
  end_date<-as.character(ceiling_date(ymd(start_date),unit= "month") -1)
  if(period=="start")return(start_date)
  if(period=="end")return(end_date)
  if(period=="start+end")return(paste0(start_date,"/",end_date))
  
}

#ytodate
ytodate<- function(date,period="start"){
  Y<-unlist(strsplit(date[i],"-"))[1]
  start_date<-paste0(Y,"-01-01")
  end_date<-paste0(Y,"-12-31")
  if(period=="start")return(start_date)
  if(period=="end")return(end_date)
  if(period=="start+end")return(paste0(start_date,"/",end_date))
}

#dateFormating
dateFormating<- function(date,period="start"){
  dates<-c()
  for(i in c(1:length(date))){
    if(startsWith(unlist(strsplit(date[i],"-"))[2],"Q")){
      x<-qtodate(date[i],period=period)
    }else if(unlist(strsplit(date[i],"-"))[2]=="NA"){
      x<-ytodate(date[i],period=period)
    }else{
      x<-ymtodate(date[i],period=period)
    }
    dates<-c(dates,x)
  }
  return(dates)
}

#readTaskColumnDefinitions
readTaskColumnDefinitions<- function(file, format, config = NULL, reporting_entity = NULL){
  task_def<-jsonlite::read_json(file)
  task_def <- task_def$formats[[format]]$columns
  if(!is.null(config$dcf$reporting_entities)) if(config$dcf$reporting_entities$validation){
    if(!is.null(task_def[[config$dcf$reporting_entities$name]]$ref)) task_def[[config$dcf$reporting_entities$name]]$ref <- NULL
    task_def[[config$dcf$reporting_entities$name]]$allowed_values <- reporting_entity
    task_def[[config$dcf$reporting_entities$name]]$rule_type <- "reporting_entity"
  }
  return(task_def)
}

#validateData
validateData<-function(file, task_def, config = NULL){
  
  rules<-task_def
  
  errors<-data.frame(
    type=character(),
    rule=character(),
    row=character(),
    column=character(),
    category=character(),
    message=character()
  )
  
  #TODO read a file (json) with possible validity rules
  #TODO filter on validity rules that apply this app context
  tests<-data.frame(
    code=c("E01","E02","E03","E04","E05", "E06", "I01"),
    name=c("Readable Dataset",
           "Structure of Dataset",
           "No missing Values",
           "Conformity with Standards",
           "Valid Reporting entity",
           "Valid Dates",
           "Skipped Information")
  )
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
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
  reporting_entity_col <- NULL
  if(!is.null(config$dcf$reporting_entities)) if(config$dcf$reporting_entities$validation){
    reporting_entity_cols <- rules[sapply(rules, function(x){x$rule_type=="reporting_entity"})]
    reporting_entity_col <- reporting_entity_cols[[1]]
  }
  
  # ERRORS DETECTION
  ##MANDATORIES COLUMNS
  INFO("Check generic columns")
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
      ##### YEAR VALIDITY
      if(x$id=="year"){
        cond<-nchar(checkedCol)!=4|!any(startsWith(as.character(checkedCol),c("1","2")))|checkedCol>as.integer(substr(Sys.Date(),1,4))
        if(any(cond)){
          rows<-which(cond)
          for(rowid in rows){
            errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid value",message=sprintf("value '%s' is not valid year",checkedCol[rowid])))
          }
        }
      }
      
    }
    
    data_names<-data_names[!data_names %in% usedName]
  }
  
  #CONDITIONALS COLUMNS
  ## PERID VALIDITY
  INFO("Check special columns (time)")
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
            errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column="time",category="Invalid date",message=cond[rowid]))
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
                errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid date",message="Date after today"))
              }
            }
          }else{
            rows<-which(!valid_start)
            for(rowid in rows){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid date",message="Invalid date format"))
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
                errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid date",message="Date after today"))
              }
            }
          }else{
            rows<-which(!valid_end)
            for(rowid in rows){
              errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column=usedName,category="Invalid date",message="Invalid date format"))
            }
          }
          
          data_names<-data_names[!data_names %in% usedName]
          
          if(all(valid_start)&all(valid_end)){
            date_order<-as.Date(time_end)>=as.Date(time_start)
            if(!all(date_order)){
              rows<-which(!date_order)
              for(rowid in rows){
                errors<-rbind(errors,data.frame(type="ERROR",rule="E06",row=rowid,column="-",category="Invalid date","Date end before date start"))
              }
            }
          }
          
        }
      }
    }
  } 
  
  #REPORTING ENTITY COLUMN
  INFO("Check reporting entity column")
  if(!is.null(reporting_entity_col)){
    
    print(reporting_entity_col)
    x<-reporting_entity_col
    
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
      #### VALUE IN ALLOWED VALUES
      INFO("Check reporting entity allowed values")
      print(x)
      if(!is.null(x$allowed_values)){
        ref<-unlist(x$allowed_values)
        cond<-any(!(unique(checkedCol[!is.na(checkedCol)])%in%ref))
        if(cond){
          errors<-rbind(errors,data.frame(type="ERROR",rule="E05",row="-",column=usedName,category="Invalid reporting entity",message=sprintf("At least one reporting entity does not match the selected reporting entity ('%s') for the '%s' column.",x$allowed_values, usedName)))
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

#standardizeNames
standardizeNames<-function(file,task_def,exclude_unused=T){
  
  rules<-task_def
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{}
  }
  
  rules_cols<-names(rules)
  data_names<-names(data)
  
  for (i in rules_cols){
    x<-rules[[i]]
    std_name<-x$id
    alt_names<-unlist(x$aliases)
    if(std_name%in%data_names){}else{
      if(any(alt_names%in%data_names)){
        usedName<-alt_names[alt_names%in%data_names]
        names(data)[names(data) == usedName] <- std_name
      }
    }
  }
  
  if(exclude_unused){
    data<-data[rules_cols]
  }
  
  return(data)
}

#simplifiedToGeneric
simplifiedToGeneric<-function(file,rules){
  
  rules<-jsonlite::read_json(rules)
  task_def <- rules$formats[["generic"]]$columns
  
  type=rules$measurement
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data<-read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{}
  }
  
  names(data)<-gsub("_unit","__measurement_unit",names(data))
  measurement_type<-paste0(unlist(task_def$measurement$allowed_values),"_",unlist(task_def$measurement_type$allowed_values))
  names(data)[names(data) %in% measurement_type]<-paste0(names(data)[names(data) %in% measurement_type],"__measurement_value")
  names(data)[grepl("__measurement",names(data),fixed = T)]<-gsub(paste0(task_def$measurement$allowed_values[[1]],"_"),"",names(data)[grepl("__measurement",names(data),fixed = T)])
  
  newdata<-data%>%
    pivot_longer(names(data)[grepl("__measurement",names(data),fixed = T)],names_to = c("measurement_type", ".value"), 
                 names_sep="__" )%>%
    mutate(measurement=task_def$measurement$allowed_values[[1]])%>%
    rowwise()%>%
    mutate(date=ifelse("period"%in%names(data),paste0(year,"-",period),paste0(year,"-NA")))%>%
    ungroup()%>%
    mutate(time_start=dateFormating(date,"start"),
           time_end=dateFormating(date,"end"),
           time=paste0(time_start,"/",time_end))
  
  if("measurement_obs"%in%names(task_def)){
    names(newdata)[endsWith(names(newdata),"_obs",names(newdata))] <- "measurement_obs" 
  }
  
  newdata<-newdata[names(task_def)]
  
  return(newdata)
}

#validateCallRules
validateCallRules <- function(file, rules){
  
  errors<-data.frame(
    type=character(),
    row=character(),
    column=character(),
    category=character(),
    message=character()
  )
  
  tests<-data.frame(
    code=c("SE01","SE02","SW01","SE03"),
    name=c("Consistancy with data call period",
           "Presence of mandatory years",
           "Presence of historical serie",
           "Consistancy with reporting entities"),
    status=c("NOT TESTED","NOT TESTED","NOT TESTED","NOT TESTED"),
    icon=rep(paste0(tags$span(shiny::icon("ban"), title = "Not tested", style = "color:grey;"), collapse=""),4)
  )
  
  if(is.data.frame(file)){
    data<-file
  }else{
    if(any(endsWith(file,c("xls","xlsx")))){
      data <- readxl::read_excel(file,col_types = "text")
    }else if(any(endsWith(file,"csv"))){
      data<-readr::read_csv(file,col_types = readr::cols(.default = "c"))
    }else{}
  }
  
  #TIME DATA CALL CONSISTANCY
  if("time_start" %in% names(rules)){
    rule<-eval_variable_expression(rules[["time_start"]])
    #FIRST CHECK : NO YEAR IN DATA BEFORE DATACALL REQUEST
    data_date<-as.Date(unique(data$time_start))
    cond<-data_date[data_date<rule]
    if(length(cond>0)){
      for(date in cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE01",row="-",column="time_start",category="date before allowed period",message=sprintf("At least one data is associate to the inconsistant date '%s' no respect data call's start date '%s'",date,rule)))
      }
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"FAILED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"PASSED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
  }
  
  if("time_end" %in% names(rules)){
    rule<-eval_variable_expression(rules[["time_end"]])
    #FIRST CHECK : NO YEAR IN DATA AFTER DATACALL REQUEST
    data_date<-as.Date(unique(data$time_end))
    cond<-data_date[data_date>rule]
    if(length(cond>0)){
      for(date in cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE01",row="-",column="time_end",category="date after allowed period",message=sprintf("At least one data is associate to the inconsistant date '%s' no respect data call's end date '%s'",date,rule)))
      }
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"FAILED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"PASSED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
    #SECOND CHECK : LAST YEAR REQUESTED MUST BE PRESENT
    cond<-year(max(data_date))==year(rule)
    if(!cond){
      errors<-rbind(errors,data.frame(type="ERROR",rule="SE02",row="-",column="time_end",category="missing last year",message=sprintf("Last year requested by the data call is missing of data",year(rule))))
      if(tests[tests$code=="SE02",]$status!="FAILED"){
        tests[tests$code=="SE02",]$status<-"FAILED"
        tests[tests$code=="SE02",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE02",]$status!="FAILED"){
        tests[tests$code=="SE02",]$status<-"PASSED"
        tests[tests$code=="SE02",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
  }
  
  if(all(c("time_start","time_end") %in% names(rules))){
    rule<-c(eval_variable_expression(rules[["time_start"]]),eval_variable_expression(rules[["time_end"]]))
    #THIRD CHECK : ALL YEAR PRESENT
    if(year(rule[1])!=year(rule[2])){
      req_y<-seq(year(rule[1]),year(rule[2]))
      data_y<-seq(year(min(as.Date(unique(data$time_start)))),year(max(as.Date(unique(data$time_end)))))
      cond<-req_y[!req_y%in%data_y]
      if(length(cond)>0){
        for(year in cond){
          errors<-rbind(errors,data.frame(type="WARNING",rule="SW01",row="-",column="time_start",category="missing historical year",message=sprintf("Historical year '%s' is missing of data call",year)))
          errors<-rbind(errors,data.frame(type="WARNING",rule="SW01",row="-",column="time_end",category="missing historical year",message=sprintf("Historical year '%s' is missing of data call",year)))
        }
        if(tests[tests$code=="SW01",]$status!="WARNING"){
          tests[tests$code=="SW01",]$status<-"WARNING"
          tests[tests$code=="SW01",]$icon<-paste0(tags$span(shiny::icon("exclamation-triangle"), title = "Warning", style = "color:orange;"), collapse="")
        }
      }else{
        if(tests[tests$code=="SW01",]$status!="WARNING"){
          tests[tests$code=="SW01",]$status<-"PASSED"
          tests[tests$code=="SW01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
        }
      }
    }
  }
  
  if("time" %in% names(rules)){
    rule<-eval_variable_expression(rules[["time"]])
    #FIRST CHECK : ALL YEAR IN DATA MUST BE INCLUDED IN DATACALL REQUEST
    decode_date<-unlist(strsplit(unique(data$time),"/"))
    time_start<-as.Date(decode_date[1])
    time_end<-as.Date(decode_date[2])
    
    cond<-data_date[time_start<rule[1]]
    if(length(cond>0)){
      for(date in cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE01",row="-",column="time",category="date before allowed period",message=sprintf("At least one data is associate to the inconsistant date '%s' no respect data call's start date '%s'",date,rule[1])))
      }
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"FAILED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"PASSED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
    
    cond<-data_date[time_start>rule[2]]
    if(length(cond>0)){
      for(date in cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE01",row="-",column="time",category="date after allowed period",message=sprintf("At least one data is associate to the inconsistant date '%s' no respect data call's end date '%s'",date,rule[2])))
      }
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"FAILED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE01",]$status!="FAILED"){
        tests[tests$code=="SE01",]$status<-"PASSED"
        tests[tests$code=="SE01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
    
    #SECOND CHECK : LAST YEAR REQUESTED MUST BE PRESENT
    cond<-year(max(time_end))==year(rule[2])
    
    if(!cond){
      errors<-rbind(errors,data.frame(type="ERROR",rule="SE02",row="-",column="time",category="missing last year",message=sprintf("Last year requested by the data call is missing of data",year(rule[2]))))
      if(tests[tests$code=="SE02",]$status!="FAILED"){
        tests[tests$code=="SE02",]$status<-"FAILED"
        tests[tests$code=="SE02",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      if(tests[tests$code=="SE02",]$status!="FAILED"){
        tests[tests$code=="SE02",]$status<-"PASSED"
        tests[tests$code=="SE02",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
      }
    }
    
    #THIRD CHECK : ALL YEAR PRESENT
    if(year(rule[1])!=year(rule[2])){
      req_y<-seq(year(rule[1]),year(rule[2]))
      data_y<-seq(year(min(time_start)),year(max(time_end)))
      cond<-req_y[!req_y%in%data_y]
      if(length(cond)>0){
        for(year in cond){
          errors<-rbind(errors,data.frame(type="WARNING",rule="SW01",row="-",column="time",category="missing historical year",message=sprintf("Historical year '%s' is missing of data call",year)))
        }
        if(tests[tests$code=="SW01",]$status!="WARNING"){
          tests[tests$code=="SW01",]$status<-"WARNING"
          tests[tests$code=="SW01",]$icon<-paste0(tags$span(shiny::icon("exclamation-triangle"), title = "Warning", style = "color:orange;"), collapse="")
        }
      }else{
        if(tests[tests$code=="SW01",]$status!="WARNING"){
          tests[tests$code=="SW01",]$status<-"PASSED"
          tests[tests$code=="SW01",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
        }
      }
    }
  }
  
  if("reporting_entity" %in% names(rules)){
    rule<-rules[["reporting_entity"]]
    #FIRST CHECK :ONLY ON REPORTING ENTITY IN DATA
    data_reporting_entities<-unique(data$flagstate)
    cond<-length(data_reporting_entities)
    if(cond>1){
      errors<-rbind(errors,data.frame(type="ERROR",rule="SE03",row="-",column="flagstate",category="unique reporting entity",message=sprintf("%s reporting entities are detected in the dataset (%s) and only '%s' should be include",cond,paste0(data_reporting_entities,collapse=","),rule)))
      if(tests[tests$code=="SE03",]$status!="FAILED"){
        tests[tests$code=="SE03",]$status<-"FAILED"
        tests[tests$code=="SE03",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
      }
    }else{
      #SECOND CHECK :REPORTING ENTITY CORRESPONDING TO CORRECT REPORTING ENTITY
      cond<-data_reporting_entities==rule
      if(!cond){
        errors<-rbind(errors,data.frame(type="ERROR",rule="SE03",row="-",column="flagstate",category="conform reporting entity",message=sprintf("reporting entity '%s' is not corresponding to the selected reporting entity '%s'",data_reporting_entities,rule)))
        if(tests[tests$code=="SE03",]$status!="FAILED"){
          tests[tests$code=="SE03",]$status<-"FAILED"
          tests[tests$code=="SE03",]$icon<-paste0(tags$span(shiny::icon("times-circle"), title = "Fail", style = "color:red;"), collapse="")
        }
      }else{
        if(tests[tests$code=="SE03",]$status!="FAILED"){
          tests[tests$code=="SE03",]$status<-"PASSED"
          tests[tests$code=="SE03",]$icon<-paste0(tags$span(shiny::icon("check-circle"), title = "Pass", style = "color:green;"), collapse="")
        }
      }
    }
  }
  
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

#completeWithMissingEntities
completeWithMissingEntities<-function(config,pool,profile,data,user_only=FALSE){
  if(user_only){
    reporting_entities<-data.frame(code=getDBUserReportingEntities(pool,profile))
  }else{
    reporting_entities<-config$dcf$reporting_entities$codelist_ref 
  }
  
  if(nrow(data)>0){
    missing_entities<-subset(reporting_entities,!code%in%data$reporting_entity)$code
    if(length(missing_entities>0)){
      
      print(missing_entities)
      print(length(missing_entities))
      missing <- data.frame(
        id = rep("",length(missing_entities)),
        data_call_id = rep(data$data_call_id[1],length(missing_entities)),
        data_call_folder = rep("",length(missing_entities)),
        task_id = rep(data$task_id[1],length(missing_entities)),
        reporting_entity = missing_entities,
        temporal_extent = rep("",length(missing_entities)),
        submitter = rep("",length(missing_entities)),
        creationTime = rep(NA,length(missing_entities)),
        lastModifiedBy = rep("",length(missing_entities)),
        lastModificationTime = rep(NA,length(missing_entities)),
        status = rep("MISSING",length(missing_entities)),
        stringsAsFactors = FALSE)
      
      data<-rbind(data,missing)
    }
  }else{
    data <- data.frame(
      id = rep("",length(reporting_entities)),
      data_call_id = rep("",length(reporting_entities)),
      data_call_folder = rep("",length(reporting_entities)),
      task_id = rep("",length(reporting_entities)),
      reporting_entity = reporting_entities,
      temporal_extent = rep("",length(reporting_entities)),
      submitter = rep("",length(reporting_entities)),
      creationTime = rep(NA,length(reporting_entities)),
      lastModifiedBy = rep("",length(reporting_entities)),
      lastModificationTime = rep(NA,length(reporting_entities)),
      status = rep("MISSING",length(reporting_entities)),
      stringsAsFactors = FALSE)
  }
  return(data)
}
