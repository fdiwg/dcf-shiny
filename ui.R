ui <- dashboardPage(
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
    useShinyjs()
  )
)