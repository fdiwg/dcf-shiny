footer <- function(id, version, date){
  tags$div(
    tags$p(sprintf("%s - v%s (%s)", id, version, date), style = "float:left;color:white;"),
    tags$p("This work has received funding from the European Union's Horizon 2020 research and innovation programme under the BlueCloud project (Grant agreement No 864209).", style = "float:right;")
  )
}

progressBar <- function(value = 0, label = FALSE, color = "aqua", size = NULL,height= NULL,
                        striped = FALSE, active = FALSE, vertical = FALSE) {
  stopifnot(is.numeric(value))
  if (value < 0 || value > 100)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)
  if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
    stop("'color' should be a valid status or color.", call. = FALSE)
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  if (vertical)
    style <- htmltools::css(height = text_value, `min-height` = "2em")
  else
    style <- htmltools::css(width = text_value, `min-width` = "0em")
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (vertical) "vertical",
    class = if (active) "active",
    style = if (!is.null(height)){htmltools::css(height = height)}else{NULL},
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}

progressGroup <- function(text, value, min = 0, max = value, color = "aqua") {
  stopifnot(is.character(text))
  stopifnot(is.numeric(value))
  if (value < min || value > max)
    stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)
  tags$div(
    class = "progress-group",
    tags$span(class = "progress-text", text),
    tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
    progressBar(round(value / max * 100), color = color, size = "sm")
  )
}

progressInfoBox <- function(title, value,text,description, max = value, icon = shiny::icon("bar-chart"), 
                            color = "aqua", width = 4, fill = FALSE,height="5px") {

colorClass <- paste0("bg-", color)
boxContent <- div(class = "info-box", class = if (fill) 
  colorClass, span(class = "info-box-icon", class = if (!fill) 
    colorClass, icon), div(class = "info-box-content", 
                           span(class = "info-box-text", title), if (!is.null(text)) 
                             span(class = "info-box-number", text),
                           progressBar(round(value / max * 100), color = color, size = "sm",height=height),
                           if(!is.null(description)){tags$span(class = "progress-description", description)}else{NULL}
    ))

div(class = if (!is.null(width)) 
  paste0("col-sm-", width), boxContent)
}