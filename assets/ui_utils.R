footer <- function(id, version, date){
  tags$div(
    tags$p(sprintf("%s - v%s (%s)", id, version, date), style = "float:left;color:white;"),
    tags$p("This work has received funding from the European Union's Horizon 2020 research and innovation programme under the BlueCloud project (Grant agreement No 864209).", style = "float:right;")
  )
}