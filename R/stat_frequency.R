

#' Stat - Frequency
#' @param data data
#' @param xvar variable
#' @param show_code show/hide code
#' @import shiny
#' @importFrom dplyr count
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#' stat_frequency(mtcars)
#' }
#' @export

stat_frequency <- function(data, xvar, show_code) {

  if (missing(xvar)) {xvar = ""} else {xvar = deparse(substitute(xvar))}
  if (missing(show_code)) {show_code = FALSE}

  stat_frequency_ui <- function(id,
                                frequency_xvar = xvar,
                                frequency_showcode = show_code) {
    ns <- NS(id)
    tagList(
      div(id = ns('placeholder1'), class = "parent",
          div(class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
              div(class = "input-view well", style = "padding-right: 0; width: 350px;",
                  div(class = "custom-scroll",
                      div(class = "grid-container",
                          div("Frequency",  class = "module-name"),
                          div(class = "cont2",
                              switchInput(NS(id, "frequency_instant"),
                                          label = "",
                                          value = TRUE,
                                          size = "mini",
                                          onLabel = "",
                                          offLabel = "")),
                          div(class = "cont3", actionButton(NS(id, "frequency_run"), class = "btn-play",
                                                            label = icon(name = "fas fa-play", lib = "font-awesome")))

                      ),

                     selectizeInput(NS(id, "frequency_xvar"),
                                    label = "X",
                                    choices = c("", names(data)),
                                    selected = frequency_xvar
                     ),

                       prettyCheckbox(NS(id, "frequency_showcode"),
                                      label = "show/hide code",
                                      status = "info",
                                      value = frequency_showcode
                       )
                     )),

                  div(class = "result-view",

                  fluidRow(tableOutput(NS(id, "frequency_table"))),
                  fluidRow(verbatimTextOutput(NS(id, "frequency_text")) |>
                             tagAppendAttributes(class = "codeoutput"))
        ))
      ))
  }

  stat_frequency_se <- function(id) {
    moduleServer(id, function(input, output, session) {

      req(data)

      observeEvent(data, {
        updateSelectizeInput(
          session,
          "frequency_xvar",
          choices = c("", names(data))
        )
      })

      ns <- NS(id)

      observeEvent(input$frequency_instant, {
        if(input$frequency_instant == TRUE) {
          removeClass("frequency_run", "toggle-btnplay")
        } else {
          addClass("frequency_run", "toggle-btnplay")
        }
      })


      run <- reactive({
        input$frequency_run
      })


      code_text <- reactive({
        t <- paste0(
          "\n \n data |> ",
          "\n    count(", input$frequency_xvar, ")"
        )
      })

      code_text2 <- reactive({
        if(input$frequency_instant) {
        code_text()
        } else {
          req(run())
          isolate(code_text())
        }
      })

      output$frequency_table <- renderTable({
          req(input$frequency_xvar)
          eval(parse(text = code_text2()))
        })

      output$frequency_text <- renderText({
        paste(code_text())
      })


      mod_id <- paste0(id, "-frequency_")

      observeEvent(input$frequency_showcode, {
        if (input$frequency_showcode == "TRUE") {
          runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
        }
        if (input$frequency_showcode == "FALSE") {
          runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
        }
      })


    })

  }
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(
        HTML('
              .input-view .well { width: 350px; margin-left: -10px; }
              .well  { background-color: #ffffff !important;}
              .result-view { margin-left: 20px; width: 700px; }
              .toggle-btnplay { visibility: visible; background: none; }
              .cont2 .shiny-input-container:not(.shiny-input-container-inline) { width: auto; 	max-width: 100%; }
              .cont3 { margin-left: 10px; visibility: hidden; }
              .grid-container { display: flex; }
              #code { white-space: pre; margin: 20px; }
              .module-name {margin-top: 4px; font-style: italic;  width: 275px;}
              .shiny-text-output {  border: none;  margin-top: 20px;}
              .bootstrap-switch.bootstrap-switch-focused {	border-color: #d4d0d0;	outline: 0;	-webkit-box-shadow: none; box-shadow: none;}
              .bootstrap-switch.bootstrap-switch-mini .bootstrap-switch-handle-off, .bootstrap-switch.bootstrap-switch-mini .bootstrap-switch-handle-on, .bootstrap-switch.bootstrap-switch-mini .bootstrap-switch-label {padding: 1px 5px;font-size: 12px;line-height: 1;}
              .btn-play {padding: 0 !important;  margin-bottom: 10px;border: none;}
              .btn-play:hover {color: #000000; background-color:  #ffffff;border-color:  #ffffff;}
              .module-style { text-align: left; background-color: #faf9f7; border: 0; margin-bottom: 5px;}
              .parent .row .col-sm-3 {max-width: 400px !important;min-width: 300px !important;}
              .custom-scroll {max-height: 80vh;min-height: 30vh;overflow-y: auto;overflow-x: hidden;position: relative;scrollbar-width: thin;padding-right: 15px;}
              .custom-scroll::-webkit-scrollbar {width: 4px;background: #faf9f7;}
              .custom-scroll::-webkit-scrollbar-track {-webkit-border-radius: 2px;border-radius: 2px;}
              .custom-scroll::-webkit-scrollbar-thumb {-webkit-border-radius: 2px;border-radius: 2px;background:  #C0C0C0;}


             ')
      )
    ),
    theme = bslib::bs_theme(),
    stat_frequency_ui("module")
  )

  server <- function(input, output, session) {
    stat_frequency_se("module")
  }
  shinyApp(ui, server)
}


