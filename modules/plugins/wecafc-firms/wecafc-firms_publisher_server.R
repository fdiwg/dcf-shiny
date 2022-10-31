function(id, parent.session, config, profile, components){
  moduleServer(
    id,
    function(input, output, session) {
      #-----------------------------------------------------------------------------------
      ns <- session$ns
      
      #store
      store <- components$STORAGEHUB
      pool <- components$POOL
      
      output$task_wrapper<-renderUI({
        selectizeInput(ns("task"),
                       label="Task",
                       multiple = F,
                       choices = getTasks(config,withId=TRUE),
                       selected=NULL,
                       options = list(
                         placeholder = "Select a task",
                         onInitialize = I('function() { this.setValue(""); }')
                       )
        )
      })
      
      
      #-----------------------------------------------------------------------------------
    }
  )
}