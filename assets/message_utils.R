#sendMessage
sendMessage <- function(subject, body, recipients, attachment_ids = list(), profile){
  sent <- FALSE
  req <- httr::POST(
    url = "https://api.d4science.org/rest/2/messages/write-message", 
    body = list(
      subject = subject,
      body = body,
      recipients = recipients,
      attachment_ids = attachment_ids
    ), 
    add_headers("Authorization" = paste("Bearer",profile$access$access_token)),
    encode = "json"
  )
  if(httr::status_code(req)==201) sent <- TRUE
  return(sent)
}

#getMessages
getMessages <- function(profile){
  req <- httr::GET(
    url = "https://api.d4science.org/rest/2/messages/get-received-messages",
    add_headers("Authorization" = paste("Bearer",profile$access$access_token))
  )
  resp <- httr::content(req)
  return(resp)
}

