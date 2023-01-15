
stat_t_test <- function(data, xvar, yvar, group, test, show_code) {

  if (missing(xvar)) {xvar = ""} else {xvar = deparse(substitute(xvar))}
  if (missing(yvar)) {yvar = ""} else {yvar = deparse(substitute(yvar))}
  if (missing(group)) {group = ""} else {group = deparse(substitute(group))}
  if (missing(test)) {test = "Welsch"} else {test = deparse(substitute(test))}
  if (missing(show_code)) {show_code = FALSE}


stat_t_test_ui <- function(id,
                    ttest_yvar = yvar,
                    ttest_xvar = xvar,
                    ttest_group = group,
                    ttest_type = test,
                    ttest_showcode = show_code) {
  ns <- NS(id)

  tagList(
    div(
      id = ns("placeholder1"), class = "parent",
      div(
        class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
        div(
          class = "input-view well", style = "padding-right: 0; width: 350px;",
          div(
            class = "custom-scroll",
            div(
              class = "grid-container",
              div("Independent Samples T-test", class = "module-name"),
              div(
                class = "cont2",
                switchInput(NS(id, "ttest_instant"),
                  label = "",
                  value = TRUE,
                  size = "mini",
                  onLabel = "",
                  offLabel = ""
                )
              ),
              div(class = "cont3", actionButton(NS(id, "ttest_run"),
                class = "btn-play",
                label = icon(name = "fas fa-play", lib = "font-awesome")
              ))
            ),
            selectInput(NS(id, "ttest_yvar"),
              label = "Y",
              choices = c(" ", names(data)),
              selected = ttest_yvar
            ),
            selectInput(NS(id, "ttest_xvar"),
              label = "X",
              choices = c(" ", names(data)),
              selected = ttest_xvar
            ),
            hidden(
              textInput(NS(id, "ttest_vartext"),
                label = "Variables Text",
                value = ""
              ),
              actionButton(NS(id, "updatebtn"), "Update")
            ),
            selectizeInput(NS(id, "ttest_group"),
              label = "Groups",
              choices = c(""),
              options = list(
                delimiter = ",",
                create = TRUE
              ),
              selected = "",
              multiple = TRUE
            ),
            checkboxGroupInput(NS(id, "ttest_type"),
              label = NULL,
              choices = c("Welsch", "Student's"),
              selected = ttest_type,
              inline = TRUE,
              width = "100%"
            ),
            actionButton(NS(id, "toggle_ttest_add_code"),
              width = "100%",
              class = "module-style",
              label = "Code",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              prettyCheckbox(NS(id, "ttest_showcode"),
                label = "show/hide code",
                status = "info",
                value = ttest_showcode
              )
            )
          )
        ),
        div(
          class = "result-view",

          fluidRow(tableOutput(NS(id, "ttest_table"))),
          fluidRow(tableOutput(NS(id, "ttest_table2"))),
          fluidRow(verbatimTextOutput(NS(id, "ttest_text")) %>%
            tagAppendAttributes(class = "codeoutput")),
          fluidRow(verbatimTextOutput(NS(id, "ttest_text2")) %>%
            tagAppendAttributes(class = "codeoutput"))
        )
      )
    )
  )
}

stat_t_test_se <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data, {
      delay(1000, click("updatebtn"))
    })

    observeEvent(input$updatebtn, {
      updateSelectizeInput(session, "ttest_group", selected = unlist(strsplit(input$ttest_vartext, "\\,")))
    })

    observeEvent(data, {
      updateSelectInput(
        session,
        "ttest_yvar",
        choices = c(" ", names(data))
      )

      updateSelectInput(
        session,
        "ttest_xvar",
        choices = c(" ", names(data))
      )
    })

    observeEvent(input$ttest_group, {
      req(input$ttest_group)
      updateTextInput(session, "ttest_vartext", value = input$ttest_group)
    })



    observeEvent(input$ttest_xvar, {
      updateSelectizeInput(session, "ttest_group", choices = c("", levels(as.factor(data[[input$ttest_xvar]]))))
    })



    observeEvent(input$ttest_instant, {
      if (input$ttest_instant == TRUE) {
        removeClass("ttest_run", "toggle-btnplay")
      } else {
        addClass("ttest_run", "toggle-btnplay")
      }
    })



    code_text <- reactive({
      paste0(
        "\n \n data %>% ", "filter(", input$ttest_xvar, "%in% c(", paste0("'", input$ttest_group, "'", collapse = ","), ")) %>% ",
        "\n    t.test(", input$ttest_yvar, " ~ ", input$ttest_xvar, ", data = . ", ") %>% \n    tidy()" # %>% \n    mutate(p.value = scales::pvalue(.$p.value))"
      )
    })



    observeEvent(input$toggle_ttest_add_code, {
      toggle("ttest_showcode")



      if (input$toggle_ttest_add_code %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_ttest_add_code",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_ttest_add_code",
          icon = icon("fas fa-caret-down")
        )
      }
    })

    code_text2 <- reactive({
      paste0(
        "\n \n data %>% ", "filter(", input$ttest_xvar, "%in% c(", paste0("'", input$ttest_group, "'", collapse = ","), ")) %>% ",
        "\n    t.test(", input$ttest_yvar, " ~ ", input$ttest_xvar, ", var.equal = TRUE, data = . ", ") %>% \n    tidy() " # %>% \n    mutate(p.value = scales::pvalue(.$p.value))"
      )
    })



    output$ttest_table <- renderTable(
      {
        req(isTruthy(input$ttest_yvar != " ") && isTruthy(input$ttest_xvar != " "))

        req(isTruthy(input$ttest_type == "Welsch"))

        req(code_text())

        eval(parse(text = code_text()))
      },
      spacing = "xs",
      caption = "Welsch",
      caption.placement = "top"
    )



    output$ttest_table2 <- renderTable(
      {
        req(isTruthy(input$ttest_yvar != " ") && isTruthy(input$ttest_xvar != " "))

        req(isTruthy(input$ttest_type == "Student's"))

        req(code_text2())

        eval(parse(text = code_text2()))
      },
      caption = "Student",
      caption.placement = "top"
    )

    output$ttest_text <- renderText({
      req(isTruthy(input$ttest_type == "Welsch"))

      code_text()
    })



    output$ttest_text2 <- renderText({
      req(isTruthy(input$ttest_type == "Student's"))

      code_text2()
    })



    ns <- NS(id)



    mod_id <- paste0(id, "-ttest_")

    observeEvent(input$ttest_showcode, {
      if (input$ttest_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
        runjs(paste0('$("#', mod_id, 'text2").css({"visibility":"visible"})'))
      }
      if (input$ttest_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
        runjs(paste0('$("#', mod_id, 'text2").css({"visibility":"hidden"})'))
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
  stat_t_test_ui("module", data)
)
server <- function(input, output, session) {
  stat_t_test_se("module")
}


shinyApp(ui, server)



}


stat_t_test(mtcars)



