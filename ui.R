ui <- shiny::tagList(
  dashboardPage(
    dashboardHeader(
      title = "DCF-SHINY"
    ),
    dashboardSidebar(
      sidebarMenuFromModules(config = CONFIG, profile = PROFILE)
    ),
    dashboardBody(
      do.call("tabItems",c(loadModuleUIs(config = CONFIG, profile = PROFILE),loadPluginUIs(config = CONFIG, profile = PROFILE))),
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dcf-shiny.css")
      )
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)