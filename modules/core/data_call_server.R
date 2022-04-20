data_call_server <- function(id, parent.session, config, profile, pool){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      
      ns <- session$ns
      
      #Data call management (CRUD
      
      #getDataCalls
      getDataCalls <- function(pool){
        DBI::dbReadTable(pool, "dcf_data_call")
      }
      
      #createDataCall
      createDataCall <- function(pool, task = "", start = Sys.Date(), end = Sys.Date(), status = "OPENED"){
        conn <- pool::poolCheckout(pool)
        idx <- nrow(getDataCalls(pool))+1
        insert_sql <- sprintf(
          "INSERT INTO dcf_data_call(id_data_call, task_id, date_start, date_end, status, folder_id) 
           VALUES (%s, '%s', '%s', '%s', '%s', '%s');", idx, task, as(start,"character"), as(end,"character"), status, "FOLDER_ID")
        out_sql <- try(DBI::dbSendQuery(conn, insert_sql))
        created <- !is(out_sql, "try-error")
        created
      }
      
      #updateDataCall
      updateDataCall <- function(pool, id_call, task = "", start = Sys.Date(), end = NULL, status = "OPENED"){
        
      }
      
      #deleteDataCall
      deleteDataCall <- function(pool, id_call){
        conn <- pool::poolCheckout(pool)
        delete_sql <- sprintf("DELETE FROM dcf_data_call WHERE id_data_call = %s", id_call)
        out_sql <- try(DBI::dbSendQuery(conn, insert_sql))
        deleted <- !is(out_sql, "try-error")
        deleted
      }
      
      #dcTableHandler
      dcTableHandler <- function(data, uuids){
        
        if(nrow(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            out_tib <- tibble::tibble(
              "Data call ID" = data[i,"id_data_call"],
              "Task ID" = data[i,"task_id"],
              "Start date" = data[i,"date_start"],
              "End date" = data[i,"date_end"],
              "Status" = data[i,"status"],
              Actions = as(actionButton(inputId = ns(paste0('button_edit_', uuids[i])), class="btn btn-info", style = "margin-right: 2px;",
                                        title = "Edit data call", label = "", icon = icon("tasks")),"character")
            )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble( 
            "Data call ID" = character(0),
            "Task ID" = character(0),
            "Start date" = character(0),
            "End date" = character(0),
            "Status" = character(0),                  
            Actions = character(0)
          )
        }
        return(data)
      }
      
      #renderDataCalls
      renderDataCalls <- function(data){
          
          uuids <- NULL
          if(!is.null(data)) if(nrow(data)>0) for(i in 1:nrow(data)){
            one_uuid = uuid::UUIDgenerate() 
            uuids <- c(uuids, one_uuid)
          }
          
          output$tbl_data_calls <- DT::renderDT(
            dcTableHandler(data, uuids),
            selection='single', escape=FALSE,rownames=FALSE,
            options=list(
              lengthChange = FALSE,
              paging = FALSE,
              searching = FALSE,
              preDrawCallback = JS(
                'function() {
                  Shiny.unbindAll(this.api().table().node()); }'
              ),
              drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); }'
              ),
              autoWidth = FALSE,
              columnDefs = list(
                list(width = '100px', targets = c(0)),
                list(width = '400px', targets = c(1),
                     render = JS("function(data, type, full, meta) {
                           var html = data;
                           if(data.startsWith(\"http://\") | data.startsWith(\"https://\")){
                              html = '<a href=\"' + data + '\" target=\"_blank\">'+data+'</a>';
                           }
                           return html;
                        }"))
              )
            )
          )
        }
      
      #render tables
      observe({
        renderDataCalls(getDataCalls(pool))
      })
      
      
      
      #data call form
      showDataCallModal <- function(new = TRUE, task = "", start = Sys.Date(), end = NULL, status = "OPENED"){
        title_prefix <- ifelse(new, "Add", "Modify")
        form_action <- tolower(title_prefix)
        showModal(modalDialog(title = sprintf("%s data call", title_prefix),
                              selectInput(ns("data_call_form_task"), "Task:",choices = getTasks(config), selected = task),
                              dateInput(ns("data_call_form_start"), "Start date", value = start),
                              dateInput(ns("data_call_form_end"), "End date", value = end),
                              selectInput(ns("data_call_form_status"), "Status", choices = list("OPENED", "CLOSED"), selected = "OPENED"),
                              actionButton(ns(sprintf("data_call_%s_go", form_action)), title_prefix),
                              easyClose = TRUE, footer = NULL ))
      }
      #contact/add
      observeEvent(input$add_data_call,{
        showDataCallModal(new = TRUE)
      })
      observeEvent(input$data_call_add_go, {
        created <- createDataCall(
          pool = pool,
          task = input$data_call_form_task,
          start = input$data_call_form_start,
          end = input$data_call_form_end,
          status = input$data_call_form_status
        )
        removeModal()
        renderDataCalls(getDataCalls(pool))
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}