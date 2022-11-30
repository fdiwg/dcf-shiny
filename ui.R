ui <- shiny::tagList(
  dashboardPage(
    dashboardHeader(
      title = shinycssloaders::withSpinner(uiOutput("header"))
    ),
    dashboardSidebar(
      shinycssloaders::withSpinner(uiOutput("sidebar"))
    ),
    dashboardBody(
      shinycssloaders::withSpinner(uiOutput("body")),
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dcf-shiny.css")
      )
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)