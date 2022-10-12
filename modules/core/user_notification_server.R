user_notification_server <- function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      pool <- components$POOL
      store <- components$STORAGEHUB
      ns <- session$ns
      
      target_data<-reactiveVal(NULL)
      
      observe({
        target_period<-Sys.Date()-months(6)
        datacall<-getDataCalls(pool)
        names(datacall)[5]<-"datacall_status"
        datacall<-subset(datacall,date_end>=target_period)
        submissions<-getSubmissions(config = config, store = store, user_only = TRUE)
        names(submissions)[10]<-"submission_status"
        target<-merge(datacall,submissions,all.x=T,all.y=F)
        target_data<-target_data(target)
      })
      
      output$datacall_news <- renderUI({
        req(!is.null(target_data()))
        
        data<-target_data()
        #No recent data call
        if(nrow(data)==0){
          message<-h4("No new informations")
        }else{
          message<-do.call("tagList", lapply(1:nrow(data), function(i){
            
            #Data call recentely closed with non submission
            if(data[i,"datacall_status"]=="CLOSED"){
              if(is.na(data[i,"submission_status"])){
                text<-sprintf("The datacall '%s' is now close since '%s' with no submission. Please contact your regional data manager.",data[i,"task_id"],data[i,"date_end"]) 
                title<-"Submission Missing"
                icon<-"exclamation"
                class="callout callout-danger"
              }else if(data[i,"submission_status"]=="REJECTED"){
                #Data call recentely closed with rejected submission
                text<-sprintf("The datacall '%s' is now close since '%s' and your submission was rejected. Please contact your regional data manager.",data[i,"task_id"],data[i,"date_end"]) 
                title<-"Submission Rejected"
                icon<-"exclamation"
                class="callout callout-danger"
              }else if(data[i,"submission_status"]=="ACCEPTED"){
                #Data call recentely closed with approved submission
                text<-sprintf("The datacall '%s' is now close since '%s' and your submission is complete.",data[i,"task_id"],data[i,"date_end"]) 
                title<-"Submission Approved"
                icon<-"check"
                class="callout callout-success"
              }
            }else{
              if(is.na(data[i,"submission_status"])){
                #Data call open with no submission
                text<-sprintf("A new datacall is actually open for task '%s'. Please submitted your data before '%s'.",data[i,"task_id"],data[i,"date_end"])
                title<-"New Datacall Open"
                icon<-"info"
                class="callout callout-warning"
              }else if(data[i,"submission_status"]=="REJECTED"){
                #Data call open with rejected submission
                text<-sprintf("Your submission for '%s' was rejected. Please provide corection and send a new submission before '%s'.",data[i,"task_id"],data[i,"date_end"])
                title<-"Submission Rejected"
                icon<-"exclamation"
                class="callout callout-danger"
                #Data call open with submitted submission
              }else if(data[i,"submission_status"]=="PENDING"){
                #Data call open with pending submission
                text<-sprintf("Your submission for '%s' was in progress. Please provide corection and send a new submission before '%s'.",data[i,"task_id"],data[i,"date_end"])
                title<-"Submission in progress"
                icon<-"loader"
                class="callout callout-info"
              }else if(data[i,"submission_status"]=="ACCEPTED"){
                #Data call open with approved submission
                text<-sprintf("Your submission for '%s' was approved . No action to do.",data[i,"task_id"],data[i,"date_end"])
                title<-"Submission Approved"
                icon<-"check"
                class="callout callout-success"
              }
            }
            
            item<-div(
              class = class,
              h3(style="margin-top :0px;",icon(icon),title),
              p(text)
            )
 
            return(item)
            
          }))
        }
        
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}