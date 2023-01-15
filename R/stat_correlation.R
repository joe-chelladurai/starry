

#' Correlation
#' @param data data
#' @param xvar list of variables
#' @param show_code show/hide code
#' @param code additional code
#' @import shiny
#' @importFrom stats cor.test
#' @importFrom dplyr select
#' @importFrom corrr correlate fashion colpair_map shave
#' @importFrom shinyjs click delay
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#' stat_frequency(mtcars)
#' }
#' @export

stat_correlation <- function(data, xvar, code, show_code) {

  if (missing(xvar)) {xvar = ""} else {xvar = deparse(substitute(xvar))}
  if (missing(code)) {code = ""}
  if (missing(show_code)) {show_code = FALSE}


calc_p_value <- function(vec_a, vec_b){
  round(cor.test(vec_a, vec_b)$p.value, 3)
}




stat_correlation_ui <- function(id,
                   corr_xvar = xvar,
                   corr_code = code,
                   corr_showcode = show_code) {
  ns <- NS(id)
  tagList(
    div(id = ns('placeholder1'), class = "parent",
        div(class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
            div(class = "input-view well", style = "padding-right: 0; width: 350px;",
                div(class = "custom-scroll",
                    div(class = "grid-container",
                        div("Correlation",  class = "module-name"),
                        div(class = "cont2",
                            switchInput(NS(id, "instantlocal"),
                                        label = "",
                                        value = TRUE,
                                        size = "mini",
                                        onLabel = "",
                                        offLabel = ""
                                       )),
                        div(class = "cont3", actionButton(NS(id, "corr_run"), class = "btn-play",
                                                          label = icon(name = "fas fa-play", lib = "font-awesome")))

                                   ),
                    hidden(textInput(NS(id, "corr_vartext"),
                                     label = "Variables Text",
                                     value = ""),
                           actionButton(NS(id, "updatebtn"), "Update")),

                    selectizeInput(NS(id, "corr_xvar"),
                                   label = "Variables",
                                   choices = c(" ", names(data)),
                                   options = list(
                                     delimiter = ',',
                                     create = TRUE
                                     ),
                                   #    selected = unlist(strsplit(corr_vartext, "\\,")),
                                   selected = "",
                                   multiple = TRUE),

                    actionButton(NS(id, "toggle_corr_add_code"),
                                 width = "100%",
                                 class = "module-style",
                                 label = "Code",
                                 icon = icon("fas fa-caret-down")),

                    hidden(textAreaInput(NS(id, "corr_code"),
                                         value = corr_code,
                                         label = NULL),

                           prettyCheckbox(NS(id, "corr_showcode"),
                                          label = "show/hide code",
                                          status = "info",
                                          value = corr_showcode
                           )
                    )


        )),
        div(class = "result-view",



                  fluidRow(tableOutput(NS(id, "corr_table"))),

                  fluidRow(tableOutput(NS(id, "calc_table"))),

                  fluidRow(verbatimTextOutput(NS(id, "corr_text")) %>% tagAppendAttributes(class = "codeoutput"))
        )
        ))
  )



}

stat_correlation_se <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data, {
      updateSelectizeInput(session, "corr_xvar", choices = c(" ", names(data))) #, selected = unlist(strsplit(input$corr_vartext, "\\,"))))
      delay(1, click("updatebtn"))
    })

    observeEvent(input$updatebtn, {
      updateSelectizeInput(session, "corr_xvar", selected = unlist(strsplit(input$corr_vartext, "\\,")))#selected = c("x4", "x3", "x2", "x1"))
    })

    ns <- NS(id)



    observeEvent(input$toggle_corr_add_code, {

      toggle("corr_showcode")

      toggle("corr_code")

      if(input$toggle_corr_add_code %%2 == 1) {

        updateActionButton(
          session,
          "toggle_corr_add_code",
          icon = icon("fas fa-caret-up"))

      } else {

        updateActionButton(
          session,
          "toggle_corr_add_code",
          icon = icon("fas fa-caret-down"))
      }
    })


    selected <- reactive({ paste(input$corr_xvar, collapse = ", ") })


    observeEvent(input$corr_xvar,{
      req(input$corr_xvar)
      updateTextInput(session, "corr_vartext", value = input$corr_xvar)
    })

    code_text <- reactive({

      t <- paste0(

        "\n \n data %>% ",

        " \n    select(", input$corr_vartext, ") %>% ",

        " \n    correlate() %>% ",

        " \n    shave() %>% ",

        " \n    fashion(decimals = 3)"

      )


      t <- paste0(
        t,

        paste0(input$corr_code))

      t




    })


    calc_text <- reactive({

      paste0(

        "\n \n data %>% ",

        " \n    select(", input$corr_vartext, ") %>% ",

        " \n    colpair_map(calc_p_value) %>% ",

        " \n    shave() %>% ",

        " \n    fashion(decimals = 3)"

      )
    })



    observeEvent(input$instantlocal, {
      if(input$instantlocal == TRUE) {
        removeClass("corr_run", "toggle-btnplay")
      } else {
        addClass("corr_run", "toggle-btnplay")
      }
    })


    run <- reactive({
      input$corr_run
    })

    code_text2 <- reactive({
      if(input$instantlocal) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })


    calc_text2 <- reactive({
      if(input$instantlocal) {
        calc_text()
      } else {
        req(run())
        isolate(calc_text())
      }
    })



    output$corr_table  <- renderTable({

      req(isTruthy(input$corr_xvar != ""))
      eval(parse(text = code_text2()))

    }, caption = "Correlation Matrix", caption.placement = "top")



    output$calc_table  <- renderTable({

      req(isTruthy(input$corr_xvar != ""))

      eval(parse(text = calc_text2()))

    },  caption = "Probability Values", caption.placement = "top")



    mod_id <- paste0(id, "-corr_")

    observeEvent(input$corr_showcode, {
      if (input$corr_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible","font-size":"12px"})'))
      }
      if (input$corr_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden","font-size":"0px"})'))
      }
    })


     output$corr_text <- renderText({

        paste(code_text2(), "\n", "\n", calc_text2())


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
  stat_correlation_ui("module")
)
server <- function(input, output, session) {
  stat_correlation_se("module")
}


shinyApp(ui, server)



}


