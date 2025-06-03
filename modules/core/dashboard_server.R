dashboard_server <- function(id, parent.session, config, profile, components,reloader){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
  
      pool <- components$POOL
      store <- components$STORAGEHUB
      
      ns <- session$ns
      
      current_datacalls<-reactiveVal(NULL)
      
      output$welcome <- renderUI({
        h2(sprintf("Welcome %s!", profile$name))
      })
      
      output$user_roles<-renderUI({
        roles_list <- getAllRoles(config)
        names(roles_list)=NULL
        roles_list = unlist(roles_list)
        my_roles<-roles_list[roles_list%in%profile$shiny_app_roles]
        ref_entity <- getReportingEntityCodes(config)
        ref_entity_db <- ref_entity[ref_entity$code %in% getDBUserReportingEntities(profile = profile, pool = pool),]
        if(nrow(ref_entity_db>0)){
          entity_choices <- ref_entity_db$code
          reporting_entities_list<-setNames(entity_choices, ref_entity_db$label)
          if(config$dcf$reporting_entities$icon == "flag"){
            assignedRE<-p(HTML(paste0(sprintf("<span><img src=\"https://raw.githubusercontent.com/fdiwg/flags/main/%s.gif\" height=16 width=32/> %s[%s]</span>",tolower(reporting_entities_list),names(reporting_entities_list),reporting_entities_list),collapse = "<br>")),style="column-count: 2;max-height:250px;overflow-y: auto;")
          }else if(config$dcf$reporting_entities$icon == "rfmo"){
            assignedRE<-p(HTML(paste0(sprintf("<span><img src=\"https://www.fao.org/fishery/services/storage/fs/fishery/images/organization/logo/%s.jpg\" height=16 width=32/> %s[%s]</span>",tolower(reporting_entities_list),names(reporting_entities_list),reporting_entities_list),collapse = "<br>")),style="column-count: 2;max-height:250px;overflow-y: auto;")
          }else{
            assignedRE<-p(HTML(paste0(sprintf("<span>%s[%s]</span>",tolower(reporting_entities_list),names(reporting_entities_list),reporting_entities_list),collapse = "<br>")),style="column-count: 2;max-height:250px;overflow-y: auto;")
          }
        }else{
          assignedRE<-"(no assigned reporting entity)"  
        }
        
        box(title=HTML("<b>My profile</b>"),collapsible = T,width = 12,
            infoBox("Assigned roles",HTML(paste0(names(my_roles),collapse = "<br>")), icon = icon("user"), fill = TRUE,color="red",width = 6),
            infoBox("Assigned reporting entities",assignedRE, icon = icon("file-pen"), fill = TRUE,color="yellow",width = 6)
        )
      })
      
      output$datacall_stat<-renderUI({
        datacalls<-getDataCalls(pool)
        datacalls<-datacalls%>%
          group_by(task_id)%>%
          filter(date_end==max(date_end))%>%
          rowwise()%>%
          mutate(status=ifelse(status=="OPENED","OPEN","CLOSED"))%>%
          mutate(time_remaining=as.numeric(date_end-Sys.Date(),unit="days"))%>%
          mutate(time_label=ifelse(time_remaining>0,sprintf("expires on <u>%s</u> (<b>%s day(s)<b/> remaining)",date_end,time_remaining),sprintf("expired since %s (%s day(s))",date_end,abs(time_remaining))))%>%
          ungroup()%>%
          arrange(task_id)
        
        current_datacalls<-current_datacalls(datacalls)
        req(nrow(current_datacalls())>0)
        boxes<-list()
          for(i in 1:nrow(current_datacalls())){
            datacall<-current_datacalls()[i,]
            user_submissions<-getSubmissions(config = config, pool = pool , profile = profile, store = store, user_only =  if(is_profile_authorized("manager", profile)){FALSE}else{TRUE},data_calls_id=datacall$id_data_call,full_entities=TRUE)
            user_submissions<-user_submissions%>%
            rowwise()%>%
            mutate(color=switch(status,
                                           "ACCEPTED"="#8CE3A0",
                                           "SUBMITTED"="#FFC95C",
                                           "MISSING"="#FF5C5C",
                                           "REJECTED"="#E74B51"))%>%
              arrange(reporting_entity)%>%
              ungroup()
            
            nb_entities<-length(unique(user_submissions$reporting_entity))
            if(is_profile_authorized("manager",profile)){
              transmitted_submissions<-length(unique(subset(user_submissions,status=="ACCEPTED")$reporting_entity))
              transmitted_label<-"transmitted and validated"
            }else{
              transmitted_submissions<-length(unique(subset(user_submissions,status%in%c("SUBMITTED","ACCEPTED"))$reporting_entity))  
              transmitted_label<-"transmitted"
            }
            submitted_submissions<-length(unique(subset(user_submissions,status=="SUBMITTED")$reporting_entity))
            
        boxes[[i]]<-box(title=datacall$task_id,width=3,
        infoBox("Status",datacall$status, icon = icon(ifelse(datacall$status=="OPEN","check","xmark")), fill = TRUE,color=ifelse(datacall$status=="OPEN","green","red"),width = 12),
        infoBox("Period",HTML(datacall$time_label), icon = icon("clock"), fill = TRUE,color=ifelse(datacall$status=="OPEN","teal","orange"),width = 12),
        progressInfoBox(title="Submission completion", text=sprintf('%s/%s',transmitted_submissions,nb_entities),value=transmitted_submissions,description=paste0(round(transmitted_submissions/nb_entities*100,0),"% ",transmitted_label), max = nb_entities,icon = icon("share"),fill = TRUE, color = ifelse(transmitted_submissions!=nb_entities,"yellow","green"),width =12),
        if(is_profile_authorized("manager",profile)){infoBox("Pending submissions",submitted_submissions, icon = icon("info"),color=ifelse(submitted_submissions>0,"yellow","green"),fill=TRUE,width = 12)}else{NULL},
        infoBox("Submission detail", div(style="max-height:250px;overflow-y: auto;",HTML(paste0(sprintf("<span> %s : </span><span class='badge' style='background-color:%s'>%s</span>",user_submissions$reporting_entity,user_submissions$color,user_submissions$status),collapse="<br>"))), icon = icon("list"), fill = TRUE,color="light-blue",width = 12)
      )
          }
        return(box(title=HTML("<b>Current datacalls</b>"),collapsible = T,width = 12,boxes))
        
      })
      
      output$ressource_management<-renderUI({
        if(is_profile_authorized("manager",profile)){
        users <- getUsers(pool,profile)
        dbUsers<-subset(users,db=="TRUE")
        dbUsersNoRole<-subset(dbUsers,roles=="")
        dcUsersSubmitter<-subset(dbUsers,grepl("submitter|manager",roles))
        dbUsersSubmitterNoRE<-subset(dbUsers,reporting_entities=="")
        ref_entity <- getReportingEntityCodes(config)
        RENoDb <- ref_entity[!ref_entity$code %in% getDBUserReportingEntities(profile = profile, pool = pool),]
        
        box(title=HTML("<b>Administration</b>"),collapsible = T,width = 12,
            box(title=HTML("<b>Users</b>"),width = 6, 
            infoBox("Users",nrow(users), icon = icon("users"), fill = TRUE,color="aqua",width = 6),
            valueBox(subtitle="Users without roles",value=nrow(dbUsersNoRole), icon = icon("user-xmark"),color=ifelse(nrow(dbUsersNoRole)>0,"yellow","green"),width = 6),
            infoBox("Users in db",nrow(dbUsers), icon = icon("users-viewfinder"), fill = TRUE,color="aqua",width = 6),
            valueBox(subtitle="SUBMITTER NOT ASSOCIATED TO A REPORTING ENTITY",value=nrow(dbUsersSubmitterNoRE), icon = icon("user-xmark"),color=ifelse(nrow(dbUsersSubmitterNoRE)>0,"yellow","green"),width = 6)
            ),
            box(title=HTML("<b>Reporting entities</b>"),width = 6,
            infoBox("Reporting entities",nrow(ref_entity), icon = icon("file-signature"), fill = TRUE,color="purple",width = 6),
            valueBox(subtitle="REPORTING ENTITIES NOT ASSIGNED TO A SUBMITTER",value=nrow(RENoDb), icon = icon("file-circle-exclamation"),color=ifelse(nrow(RENoDb)>0,"yellow","green"),width = 6)
            )
        )
        }else{NULL}
      })
      
      #----------------------------------------------------------------------------------- 
    }
  )
  
}