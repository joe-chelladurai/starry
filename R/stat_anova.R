
stat_anova <- function(data, xvar, yvar, code, show_code) {

  if (missing(xvar)) {
    xvar <- ""
  } else {
    xvar <- deparse(substitute(xvar))
  }

  if (missing(yvar)) {
    yvar <- ""
  } else {
    yvar <- deparse(substitute(yvar))
  }
  if (missing(code)) {
    code <- ""
  }
  if (missing(show_code)) {
    show_code <- FALSE
  }


  Anova <- car::Anova

  stat_anova_ui <- function(id,
                      anova_xvar = xvar,
                      anova_yvar = yvar,
                      anova_code = code,
                      anova_showcode = show_code) {
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
                div("One way Anova", class = "module-name"),
                div(
                  class = "cont2",
                  switchInput(NS(id, "instantlocal"),
                    label = "",
                    value = TRUE,
                    size = "mini",
                    onLabel = "",
                    offLabel = ""
                  )
                ),
                div(class = "cont3", actionButton(NS(id, "anova_run"),
                  class = "btn-play",
                  label = icon(name = "fas fa-play", lib = "font-awesome")
                ))
              ),
              selectInput(NS(id, "anova_yvar"),
                label = "Y",
                choices = c("", names(data)),
                selected = anova_yvar
              ),
              selectInput(NS(id, "anova_xvar"),
                label = "X",
                choices = c("", names(data)),
                selected = anova_xvar
              ),
              actionButton(NS(id, "toggle_anova_add_code"),
                width = "100%",
                class = "module-style",
                label = "Code",
                icon = icon("fas fa-caret-down")
              ),
              hidden(
                textAreaInput(NS(id, "anova_code"),
                  value = anova_code,
                  label = NULL
                ),
                prettyCheckbox(NS(id, "anova_showcode"),
                  label = "show/hide code",
                  status = "info",
                  value = anova_showcode
                )
              )
            )
          ),
          div(
            class = "result-view",
            fluidRow(tableOutput(NS(id, "anova_glance"))),
            fluidRow(tableOutput(NS(id, "anova_table"))),
            fluidRow(verbatimTextOutput(NS(id, "anova_text")) %>% tagAppendAttributes(class = "codeoutput"))
          )
        )
      )
    )
  }

  stat_anova_se <- function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- NS(id)


      observeEvent(input$toggle_anova_add_code, {
        toggle("anova_showcode")
        toggle("anova_code")

        if (input$toggle_anova_add_code %% 2 == 1) {
          updateActionButton(
            session,
            "toggle_anova_add_code",
            icon = icon("fas fa-caret-up")
          )
        } else {
          updateActionButton(
            session,
            "toggle_anova_add_code",
            icon = icon("fas fa-caret-down")
          )
        }
      })


      selected <- reactive({
        paste(input$anova_xvar, collapse = ", ")
      })


      code_text <- reactive({
        req(isTruthy(input$anova_xvar != ""))

        t <- paste0(
          "\n \n data %>% ",
          " \n    lm(", input$anova_yvar, " ~ ", "as.factor(", input$anova_xvar, "), data = . ) %>% ",
          " \n    Anova(type = 3) %>% ",
          #     " \n    summary() %>% ",
          " \n    tidy()" # %>% ",
          #" \n    mutate(p.value = scales::pvalue(.$p.value))"
        )

        t <- paste0(
          t,
          paste0(input$anova_code)
        )

        t
      })


      code_text2 <- reactive({
        t <- paste0(
          "\n \n data %>% ",
          " \n    lm(", input$anova_yvar, " ~ ", "as.factor(", input$anova_xvar, "), data = . ) %>% ",
          #     " \n    Anova(type = 3) %>% ",
          #     " \n    summary() %>% ",

          " \n    glance()"
        )
      })


      observeEvent(input$instantlocal, {
        if (input$instantlocal == TRUE) {
          removeClass("anova_run", "toggle-btnplay")
        } else {
          addClass("anova_run", "toggle-btnplay")
        }
      })


      run <- reactive({
        input$anova_run
      })

      code_text0 <- reactive({
        if (input$instantlocal) {
          code_text()
        } else {
          req(run())
          isolate(code_text())
        }
      })


      code_text3 <- reactive({
        if (input$instantlocal) {
          code_text2()
        } else {
          req(run())
          isolate(code_text2())
        }
      })



      output$anova_glance <- renderTable(
        {
          req(input$anova_xvar)
          req(input$anova_yvar)

          eval(parse(text = code_text2()))
        },
        caption = "Model Fit Statistics",
        caption.placement = "top"
      )


      output$anova_table <- renderTable(
        {
          req(input$anova_xvar)
          req(input$anova_yvar)

          eval(parse(text = code_text()))
        },
        caption = "Anova Table",
        na = "",
        caption.placement = "top"
      )



      mod_id <- paste0(id, "-anova_")

      observeEvent(input$anova_showcode, {
        if (input$anova_showcode == "TRUE") {
          runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
        }
        if (input$anova_showcode == "FALSE") {
          runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
        }
      })


      output$anova_text <- renderText({
        paste(code_text(), "\n \n", code_text2())
      })
    })
  }


  ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(
        HTML("
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

             ")
      )
    ),
    theme = bslib::bs_theme(),
    stat_anova_ui("module")
  )
  server <- function(input, output, session) {
    stat_anova_se("module")
  }


  shinyApp(ui, server)
}
