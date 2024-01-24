buildTemplate = function(format_spec){
  task_template <- do.call(rbind,lapply(format_spec$column_specs, function(x){
    name<-x$name
    label<-if(length(x$aliases)>0){x$aliases[[1]]}else{x$name}
    mandatory<-x$required
    default_value = NA
    ref<-list(NA)
    editable = TRUE
    if(name=="year"){
      default_value<-NA_character_
      year_list<-as.character(rev(seq(1900,as.integer(substr(Sys.Date(),1,4)))))
      ref<-list(tibble(code=year_list,label=year_list))
      editable<-TRUE
    }else if(x$hasCodelist()){
      cl_vrule = x$rules[sapply(x$rules, is, "vrule_codelist")][[1]]
      has_ref_values = !is.null(cl_vrule$ref_values)
      if(has_ref_values){
        if(length(cl_vrule$ref_values)==1){
          default_value = unlist(cl_vrule$ref_values)
          ref<-list(NA)
          editable<-FALSE
        }else{
          default_value<-NA_character_
          ref<-list(tibble(code=unlist(cl_vrule$ref_values),label=unlist(cl_vrule$ref_values)))
          editable<-TRUE
        }
      }else{
        default_value<-NA_character_
        ref<-list(cl_vrule$ref_data)
        editable<-TRUE
      }
    }else{
      #other rules
    }
    data<-tibble(name=name,label=label,mandatory=mandatory,default_value=default_value,ref=ref,editable=editable)
  }))
  return(task_template)
}