#sendMessage
sendMessage <- function(subject, body, recipients, profile){
  sent <- FALSE
  req <- httr::POST(
    url = "https://api.d4science.org/rest/2/messages/write-message", 
    body = list(
      subject = subject,
      body = body,
      recipients = recipients
    ), 
    add_headers("Authorization" = paste("Bearer",profile$access$access_token)),
    encode = "json"
  )
  if(httr::status_code(req)==201) sent <- TRUE
  return(sent)
}

