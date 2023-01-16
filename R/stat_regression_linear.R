

#' Stat - Linear Regression
#' @param data data
#' @param xvar xvar
#' @param yvar yvar
#' @param show_code show/hide code
#' @param code additional code
#' @import shiny
#' @importFrom shinyjs hidden removeClass addClass toggle runjs
#' @importFrom shinyWidgets switchInput prettyCheckbox
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#'   stat_regression_linear(mtcars)
#' }
#' @export

stat_regression_linear <- function(data, xvar, yvar, code, show_code) {

  if (missing(xvar)) {xvar = ""} else {xvar = deparse(substitute(xvar))}
  if (missing(yvar)) {yvar = ""} else {yvar = deparse(substitute(yvar))}
  if (missing(code)) {code = ""}
  if (missing(show_code)) {show_code = FALSE}

stat_regression_linear_ui <- function(id,
                   linreg_xvar = xvar,
                   linreg_yvar = yvar,
                   linreg_code = code,
                   linreg_showcode = show_code) {
  ns <- NS(id)
  tagList(
    div(id = ns('placeholder1'), class = "parent",
        div(class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
            div(class = "input-view well", style = "padding-right: 0; width: 350px;",
                div(class = "custom-scroll",
                    div(class = "grid-container",
                        div("Linear Regression", class = "module-name"),
                        div(class = "cont2",
                            switchInput(NS(id, "linreg_instant"),
                                        label = "",
                                        value = TRUE,
                                        size = "mini",
                                        onLabel = "",
                                        offLabel = ""
                                        )),
                        div(class = "cont3", actionButton(NS(id, "linreg_run"), class = "btn-play",
                                                           label = icon(name = "fas fa-play", lib = "font-awesome")))

                                   ),


                                   #hidden(
                                   hidden(textInput(NS(id, "linreg_vartext"),
                                                    label = "Variables Text",
                                                    value = ""),
                                          actionButton(NS(id, "updatebtn"), "Update")),
                                   #),

                                   selectInput(NS(id, "linreg_yvar"),
                                               label = "Y",
                                               choices = c("", names(data)),
                                               selected = linreg_yvar),
                                   selectizeInput(NS(id, "linreg_xvar"),
                                                  label = "Variables",
                                                  choices = c("", names(data)),
                                                  options = list(
                                                    delimiter = ',',
                                                    create = TRUE
                                                  ),
                                                  #    selected = unlist(strsplit(linreg_vartext, "\\,")),
                                                  selected = "",
                                                  multiple = TRUE),

                                   actionButton(NS(id, "toggle_linreg_add_code"),
                                                width = "100%",
                                                class = "module-style",
                                                label = "Code",
                                                icon = icon("fas fa-caret-down")),

                                   hidden(
                                     textAreaInput(NS(id, "linreg_code"),
                                                   value = linreg_code,
                                                   label = NULL),



                                     prettyCheckbox(NS(id, "linreg_showcode"),
                                                    label = "show/hide code",
                                                    status = "info",
                                                    value = linreg_showcode
                                     )

                                   )
                )
        ),
        div(class = "result-view",


                  fluidRow(hidden(textOutput(NS(id, "something")))),

                  fluidRow(tableOutput(NS(id, "linreg_glance"))),

                  fluidRow(tableOutput(NS(id, "linreg_table"))),

                  fluidRow(verbatimTextOutput(NS(id, "linreg_text")) %>% tagAppendAttributes(class = "codeoutput"))
        )
        )))



}

stat_regression_linear_se <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data, {
      updateSelectizeInput(session, "linreg_xvar", choices = c("", names(data))) #, selected = unlist(strsplit(input$linreg_vartext, "\\,"))))
      delay(1, click("updatebtn"))
    })


    observeEvent(input$updatebtn, {
      updateSelectizeInput(session, "linreg_xvar", selected = unlist(strsplit(input$linreg_vartext, "\\,")))#selected = c("x4", "x3", "x2", "x1"))
    })

    ns <- NS(id)



    output$something <- renderText({
      paste(input$linreg_vartext)
    })

    observeEvent(input$toggle_linreg_add_code, {


      toggle("linreg_showcode")

      toggle("linreg_code")

      if(input$toggle_linreg_add_code %%2 == 1) {

        updateActionButton(
          session,
          "toggle_linreg_add_code",
          icon = icon("fas fa-caret-up"))

      } else {

        updateActionButton(
          session,
          "toggle_linreg_add_code",
          icon = icon("fas fa-caret-down"))
      }
    })


    selected <- reactive({ paste(input$linreg_xvar, collapse = ", ") })


    observeEvent(input$linreg_xvar,{
      req(input$linreg_xvar)
      updateTextInput(session, "linreg_vartext", value = input$linreg_xvar)
    })

    code_text <- reactive({

      req(isTruthy(input$linreg_xvar != ""))

      t <- paste0(

        "\n \n data %>% ",

        " \n    lm(data = .,", input$linreg_yvar, " ~ ", paste(input$linreg_xvar, collapse = ' + '), paste(") %>% "),

     #   " \n    summary() %>% ",

        " \n    tidy()"# %>% ",

    #    " \n    mutate(p.value = scales::pvalue(.$p.value))"

      )


      t <- paste0(
        t,

        paste0(input$linreg_code))

      t


    })


    code_text2 <- reactive({

      req(isTruthy(input$linreg_xvar != ""))

      t <- paste0(

        "\n \n data %>% ",

        " \n    lm(data = .,", input$linreg_yvar, " ~ ", paste(input$linreg_xvar, collapse = ' + '), paste(") %>% "),

     #   " \n    summary() %>% ",

        " \n    glance()"

      )
    })

    observeEvent(input$linreg_instant, {
      if(input$linreg_instant == TRUE) {
        removeClass("linreg_run", "toggle-btnplay")
      } else {
        addClass("linreg_run", "toggle-btnplay")
      }
    })


    run <- reactive({
      input$linreg_run
    })

    code_text0 <- reactive({
      if(input$linreg_instant) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })


    code_text3 <- reactive({
      if(input$linreg_instant) {
        code_text2()
      } else {
        req(run())
        isolate(code_text2())
      }
    })



    output$linreg_glance  <- renderTable({
      eval(parse(text = code_text3()))
    }, caption = "Model Fit Statistics", caption.placement = "top")


    output$linreg_table  <- renderTable({
      eval(parse(text = code_text0()))
    }, align = c("lrrrr"),caption = "Regression Table", na = "", caption.placement = "top")





    mod_id <- paste0(id, "-linreg_")

    observeEvent(input$linreg_showcode, {
      if (input$linreg_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible","font-size":"12px"})'))
      }
      if (input$linreg_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden","font-size":"0px"})'))
      }
    })


    output$linreg_text <- renderText({
        paste(code_text0(), "\n \n ", code_text3())
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
  stat_regression_linear_ui("module")
)
server <- function(input, output, session) {
  stat_regression_linear_se("module")
}


shinyApp(ui, server)



}


