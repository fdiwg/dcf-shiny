ui <- shiny::tagList(
  dashboardPage(
    dashboardHeader(
      title = "DCF-SHINY"
    ),
    dashboardSidebar(
      sidebarMenuFromModules(config = NULL, profile = PROFILE)
    ),
    dashboardBody(
      loadModuleUIs(config = NULL, profile = PROFILE),
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dcf-shiny.css")
      )
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)