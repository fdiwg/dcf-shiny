#register_species
register_species <- function(config){
  req = readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/regional/wecafc/cl_species.csv")
  out <- as.data.frame(req[,c("code","uri","label","definition")])
  out <- out[!is.na(out$code),]
  return(out)
}

#register_flagstate
register_flagstate <- function(config){
  req = readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/main/regional/wecafc/cl_flagstate.csv")
  out <- as.data.frame(req[,c("code","uri","label","definition")])
  out <- out[!is.na(out$code),]
  return(out)
}

#register_aggregation_method
register_aggregation_method <- function(config){
  out <- data.frame(
    code = c("none", "avg_by_year", "sum"),
    uri = NA,
    label = c("None", "Yearly average", "Sum"),
    definition = NA
  )
  return(out)
}
