#loadProfile
loadProfile <- function(jwt){
  (strings <- strsplit(jwt, ".", fixed = TRUE)[[1]])
  out_jwt <- jsonlite::parse_json(rawToChar(jose::base64url_decode(strings[2])))
  out_jwt$expired <- as(Sys.time(), "numeric") > out_jwt$exp
  out_jwt$jwt <- jwt
  
  #TODO how to know the current VRE context?
  vre_contexts <- names(out_jwt$resource_access)[startsWith(names(out_jwt$resource_access), "%2Fd4science.research-infrastructures.eu")]
  if(length(vre_contexts)==0) stop("No VRE context available!")
  print(vre_contexts)
  out_jwt$vre_context <- vre_contexts[[1]]
  
  out_jwt$vre_resource_access <- out_jwt$resource_access[[out_jwt$vre_context]]
  if(out_jwt$expired) stop("JWT token is expired")
  if(!out_jwt$expired){
    req <- httr::POST(
      sprintf("%s/protocol/openid-connect/token",out_jwt$iss),
      encode = "form",
      add_headers("Authorization" = paste("Bearer", jwt)),
      body = list(
        grant_type = I("urn:ietf:params:oauth:grant-type:uma-ticket"),
        audience = URLencode(out_jwt$vre_context, reserved = TRUE)
      )
    )
    if(httr::status_code(req)==200){
      out_jwt$access <- content(req)
    }else{
      stop("JWT Token is not authorized!")
    }
  }
  
  return(out_jwt)
}