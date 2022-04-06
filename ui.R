ui <- shiny::tagList(
  dashboardPage(
    dashboardHeader(
      title = "DCF-SHINY"
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "dcf-tabs",
        menuItem(text = "Dashboard",tabName = "dashboard"),
        menuItem(text = "Monitor", tabName = "monitor")
      )
    ),
    dashboardBody(
      tabItems(
        dashboard_ui("dashboard"),
        monitor_ui("monitor")
      ),
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dcf-shiny.css")
      )
      
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)