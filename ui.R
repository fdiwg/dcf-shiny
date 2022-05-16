css <- "
.nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"

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
      shinyjs::inlineCSS(css),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "dcf-shiny.css")
      )
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)