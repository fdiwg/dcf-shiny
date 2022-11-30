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
          "id" = task_folders$name,
          "name" = sapply(task_folders$name, function(x){config$dcf$tasks[[x]]$name}),
          "creationTime" = as.POSIXct(task_folders$creationTime/1000, origin = "1970-01-01"),
          "lastModificationTime" = sapply(task_folders$id, function(x){
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
        tasks$lastModificationTime <- as.POSIXct(tasks$lastModificationTime/1000, origin = "1970-01-01")
        return(tasks)
      }
      
      output$tbl_rdb_datasets <- DT::renderDT(
        getRDBDatasets(),
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
      
      #-----------------------------------------------------------------------------------
    }
  )
}