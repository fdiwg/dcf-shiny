ui <- dashboardPage(
  dashboardHeader(
    title = "DCF-SHINY"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "dcf-tabs",
      menuItem(text = "Dashboard",tabName = "dashboard")
    )
  ),
  dashboardBody(
    tabItems(
      dashboard_ui("dashboard")
    ),
    useShinyjs()
  )
)