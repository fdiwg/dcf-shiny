function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      SH <- components$STORAGEHUB
      
      getRDBDatasets <- function(){
        task_folders <- SH$listWSItems(config$dataspace_id)
        tasks <- data.frame(
          "task_id" = task_folders$name,
          "task_name" = sapply(task_folders$name, function(x){config$dcf$tasks[[x]]$name}),
          "creation_date" = as.POSIXct(task_folders$creationTime/1000, origin = "1970-01-01"),
          "update_date" = sapply(task_folders$id, function(x){
            max(SH$listWSItems(parentFolderID = x)$lastModificationTime)
          }),
          "actions" = sapply(task_folders$id, function(x){
            items <- SH$listWSItems(parentFolderID = x)
            last_modification_time <- max(items$lastModificationTime)
            items <- items[items$lastModificationTime == last_modification_time,]
            items <- items [endsWith(items$name,"_db_new.csv"),]
            SH$getPublicFileLinkByID(items$id)
          })
        )
        tasks$update_date <- as.POSIXct(tasks$update_date/1000, origin = "1970-01-01")
        return(tasks)
      }
      
      #dcTableHandler
      rdbTableHandler <- function(data){
        
        if(nrow(data)>0){
          data <- do.call("rbind", lapply(1:nrow(data), function(i){
            out_tib <- tibble::tibble(
              "Task ID" = data[i,"task_id"],
              "Task name" = data[i,"task_name"],
              "Creation date" = data[i, "creation_date"],
              "Update date" = data[i,"update_date"],
              "Actions" = data[i,"actions"]
            )
            return(out_tib)
          }
          ))
        }else{
          data <- tibble::tibble( 
            "Task ID" = character(0),
            "Task name" = character(0),
            "Creation date" = character(0),
            "Update date" = character(0),
            "Actions" = character(0)
          )
        }
        return(data)
      }
      
      renderRDBDatasets <- function(data){
      
        output$tbl_rdb_datasets <- DT::renderDT(
          datatable(rdbTableHandler(data),
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
                list(width = '400px', targets = c(4),
                     render = JS("function(data, type, full, meta) {
                               var html = data;
                               if(data.startsWith(\"http://\") | data.startsWith(\"https://\")){
                                  html = '<a href=\"' + data + '\" target=\"_blank\">Download</a>';
                               }
                               return html;
                            }"))
              )
            )
          )
        )
      }
        
      #render tables
      observe({
        renderRDBDatasets(getRDBDatasets())
      })
      
      #-----------------------------------------------------------------------------------
    }
  )
}