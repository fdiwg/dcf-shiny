ui <- shiny::tagList(
  dashboardPage(
    dashboardHeader(
      title = CONFIG$dcf$name
    ),
    dashboardSidebar(
      sidebarMenuFromModules(config = CONFIG, profile = PROFILE)
    ),
    dashboardBody(
      do.call("tabItems",c(loadModuleUIs(config = CONFIG, profile = PROFILE),loadPluginUIs(config = CONFIG, profile = PROFILE))),
      useShinyjs(),
      useWaiter(),
      useHostess(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dcf-shiny.css")
      )
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)