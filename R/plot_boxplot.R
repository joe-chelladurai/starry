
plot_boxplot <- function(data, xvar, yvar, theme, group, code, shape, size, row, column, width, height, title, subtitle, xlab, ylab, caption, show_code){

  if (missing(xvar)) {xvar = ""} else {yvar = deparse(substitute(xvar))}
  if (missing(yvar)) {yvar = ""} else {yvar = deparse(substitute(yvar))}
  if (missing(shape)) {shape = ""} else {shape = deparse(substitute(shape))}
  if (missing(group)) {group = ""} else {group = deparse(substitute(group))}
  if (missing(size)) {size = ""} else {size = deparse(substitute(size))}
  if (missing(row)) {row = ""} else {row = deparse(substitute(row))}
  if (missing(column)) {column = ""} else {column = deparse(substitute(column))}
  if (missing(height)) {height = NA}
  if (missing(width)) {width = NA}
  if (missing(title)) {title = ""}
  if (missing(subtitle)) {subtitle = ""}
  if (missing(caption)) {caption = ""}
  if (missing(xlab)) {xlab = ""}
  if (missing(ylab)) {ylab = ""}
  if (missing(theme)) {theme = "theme_bw"} else {theme = deparse(substitute(theme))}
  if (missing(code)) {code = ""}
  if (missing(show_code)) {show_code = FALSE}






plot_boxplot_ui <- function(id,
                            data,
                            boxplot_xvar = xvar,
                            boxplot_yvar = yvar,
                            boxplot_theme = theme,
                            boxplot_group = group,
                            boxplot_code = code,
                            boxplot_shape = shape,
                            boxplot_size = size,
                            boxplot_wraprow = row,
                            boxplot_wrapcol = column,
                            boxplot_width = width,
                            boxplot_height = height,
                            boxplot_title = title,
                            boxplot_subtitle = subtitle,
                            boxplot_xlab = xlab,
                            boxplot_ylab = ylab,
                            boxplot_caption = caption,
                            boxplot_showcode = show_code) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("placeholder1"), class = "parent",
      div(
        class = "inputresultview", style = "display:flex;  margin-top:10px;  margin-bottom:10px;",
        div(
          class = "input-view well", style = "padding-right: 0; width: 350px;",
          div(
           class = "custom-scroll",
            div(
              class = "grid-container",
              div("Box", class = "module-name"),
              div(class = "cont2", switchInput(NS(id, "instantlocal"),
                label = "",
                value = TRUE,
                size = "mini",
                onLabel = "",
                offLabel = "",
              )),
              div(class = "cont3", actionButton(NS(id, "boxplot_run"),
                class = "btn-play",
                label = icon(name = "fas fa-play", lib = "font-awesome")
              ))
            ),
            selectizeInput(NS(id, "boxplot_yvar"),
              label = "Y",
              choices = c("", names(data)),
              selected = boxplot_yvar
            ),
            selectInput(NS(id, "boxplot_xvar"),
              label = "X",
              choices = c("", names(data)),
              selected = boxplot_xvar
            ),
            selectInput(NS(id, "boxplot_shape"),
              label = "Fill",
              choices = c(" ", names(data)),
              selected = boxplot_shape
            ),
            selectizeInput(NS(id, "boxplot_size"),
              label = "Outline",
              choices = c(" ", names(data)),
              selected = boxplot_size,
              options = list(create = TRUE)
            ),
            numericInput(NS(id, "boxplot_group"),
              label = "Box width",
              step = 0.1,
              value = boxplot_group
            ),
            actionButton(NS(id, "toggle_boxplot_facet"),
              width = "100%",
              class = "module-style",
              label = "Facet",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(NS(id, "boxplot_wraprow"),
                label = "Row",
                choices = c(" ", names(data)),
                selected = boxplot_wraprow
              ),
              selectInput(NS(id, "boxplot_wrapcol"),
                label = "Column",
                choices = c(" ", names(data)),
                selected = boxplot_wrapcol
              )
            ),
            actionButton(NS(id, "toggle_boxplot_text"),
              width = "100%",
              class = "module-style",
              label = "Text",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textInput(NS(id, "boxplot_title"),
                label = "Title",
                value = boxplot_title
              ),
              textInput(NS(id, "boxplot_subtitle"),
                label = "Subtitle",
                value = boxplot_subtitle
              ),
              textInput(NS(id, "boxplot_caption"),
                label = "Caption",
                value = boxplot_caption
              ),
              textInput(NS(id, "boxplot_xlab"),
                label = "X-axis label",
                value = boxplot_xlab
              ),
              textInput(NS(id, "boxplot_ylab"),
                label = "Y-axis label",
                value = boxplot_ylab
              )
            ),
            actionButton(NS(id, "toggle_plot_options"),
              width = "100%",
              class = "module-style",
              label = "Size",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              numericInput(NS(id, "boxplot_width"),
                label = "Width",
                step = 10,
                width = "100%",
                value = boxplot_width
              ),
              numericInput(NS(id, "boxplot_height"),
                label = "Height",
                step = 10,
                width = "100%",
                value = boxplot_height
              )
            ),
            actionButton(NS(id, "toggle_theme_options"),
              width = "100%",
              class = "module-style",
              label = "Theme",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(NS(id, "boxplot_theme"),
                label = "Theme",
                selected = boxplot_theme,
                choices = c(" ",
                  `Black & White` = "theme_bw",
                  `Minimal` = "theme_minimal",
                  `Grey` = "theme_grey",
                  `Line Draw` = "theme_linedraw",
                  `Light` = "theme_light",
                  `Dark` = "theme_dark",
                  `Classic` = "theme_classic",
                  `Void` = "theme_void"
                )
              )
            ),
            actionButton(NS(id, "toggle_boxplot_add_code"),
              width = "100%",
              class = "module-style",
              label = "Code",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textAreaInput(NS(id, "boxplot_code"),
                value = boxplot_code,
                label = NULL
              ),
              prettyCheckbox(NS(id, "boxplot_showcode"),
                label = "show/hide",
                status = "info",
                value = boxplot_showcode
              )
            )
          )
        ),
        div(
          class = "result-view",

          fluidRow(plotOutput(NS(id, "boxplot_plot"), width = "auto", height = "auto")),
          fluidRow(verbatimTextOutput(NS(id, "boxplot_text")) %>%
            tagAppendAttributes(class = "codeoutput"))
        )
      )
    )
  )
}

plot_boxplot_se <- function(id) {
  moduleServer(id, function(input, output, session) {
    req(data)

    observeEvent(data, {
      updateSelectizeInput(
        session,
        "boxplot_xvar",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "boxplot_yvar",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "boxplot_shape",
        choices = c(" ", names(data))
      )

      updateSelectizeInput(
        session,
        "boxplot_size",
        choices = c(" ", names(data))
      )

      updateSelectInput(
        session,
        "boxplot_wraprow",
        choices = c(" ", names(data))
      )

      updateSelectInput(
        session,
        "boxplot_wrapcol",
        choices = c(" ", names(data))
      )

      if (!is.null(input$boxplot_showcode)) {
        updatePrettyCheckbox(
          session,
          "boxplot_showcode",
          value = TRUE
        )
      }
    })

    observeEvent(input$instantlocal, {
      if (input$instantlocal == TRUE) {
        removeClass("boxplot_run", "toggle-btnplay")
      } else {
        addClass("boxplot_run", "toggle-btnplay")
      }
    })


    observeEvent(input$toggle_boxplot_facet, {
      toggle("boxplot_wraprow")
      toggle("boxplot_wrapcol")

      if (input$toggle_boxplot_facet %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_boxplot_facet",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_boxplot_facet",
          icon = icon("fas fa-caret-down")
        )
      }
    })

    observeEvent(input$toggle_boxplot_text, {
      toggle("boxplot_title")
      toggle("boxplot_subtitle")
      toggle("boxplot_caption")
      toggle("boxplot_xlab")
      toggle("boxplot_ylab")

      if (input$toggle_boxplot_text %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_boxplot_text",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_boxplot_text",
          icon = icon("fas fa-caret-down")
        )
      }
    })


    observeEvent(input$toggle_theme_options, {
      toggle("boxplot_theme")

      if (input$toggle_theme_options %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_theme_options",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_theme_options",
          icon = icon("fas fa-caret-down")
        )
      }
    })


    width <- reactive({
      if (is.na(input$boxplot_width)) {
        return(600)
      } else {
        input$boxplot_width
      }
    })


    height <- reactive({
      if (is.na(input$boxplot_height)) {
        return(400)
      } else {
        input$boxplot_height
      }
    })


    boxwidth <- reactive({
      if (is.na(input$boxplot_group)) {
        return(1)
      } else {
        input$boxplot_width
      }
    })

    ns <- NS(id)


    observeEvent(input$toggle_plot_options,
      {
        toggle("boxplot_width")
        toggle("boxplot_height")

        if (input$toggle_plot_options %% 2 == 1) {
          updateActionButton(
            session,
            "toggle_plot_options",
            icon = icon("fas fa-caret-up")
          )
        } else {
          updateActionButton(
            session,
            "toggle_plot_options",
            icon = icon("fas fa-caret-down")
          )
        }
      },
      ignoreInit = TRUE
    )




    observeEvent(input$toggle_boxplot_add_code, {
      toggle("boxplot_showcode")

      toggle("boxplot_code")

      if (input$toggle_boxplot_add_code %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_boxplot_add_code",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_boxplot_add_code",
          icon = icon("fas fa-caret-down")
        )
      }
    })


    code_text <- reactive({
      req(
        isTruthy(input$boxplot_xvar != "") |
          isTruthy(input$boxplot_yvar != "")
      )

      t <- paste0(
        "\n \n ggplot(data, aes(",
        if (input$boxplot_xvar != "") {
          paste0("factor(", input$boxplot_xvar, ")")
        },
        if (input$boxplot_yvar != "" && input$boxplot_xvar != "") {
          paste0(", ", input$boxplot_yvar)
        } else if (input$boxplot_yvar != "") {
          paste0("y = ", input$boxplot_yvar)
        } else {

        },
        if (input$boxplot_shape != " ") {
          paste0(", fill = factor(", input$boxplot_shape, ")")
        } else {

        },
        if (input$boxplot_size != " ") {
          paste0(", color = factor(", input$boxplot_size, ")")
        } else {

        },
        paste0(
          "))"
        ),
        paste0(
          " + \n    geom_boxplot(",
          if (!is.na(input$boxplot_group)) {
            paste0("width = ", input$boxplot_group)
          } else {

          },
          ")"
        )
      )

      t <- paste(
        t,
        if (input$boxplot_yvar != "" &&
          input$boxplot_xvar == "" &&
          input$boxplot_shape == " ") {
          paste(
            "+ \n    scale_x_discrete()"
          )
        }
      )


      t <- paste(
        t,
        if (input$boxplot_wraprow != " " &&
          input$boxplot_wrapcol != " ") {
          paste0(
            "+ \n    facet_grid(",
            input$boxplot_wraprow, " ~ ", input$boxplot_wrapcol, ")"
          )
        } else if (input$boxplot_wrapcol != " ") {
          paste0(
            "+ \n    facet_grid(. ~ ",
            input$boxplot_wrapcol, ")"
          )
        } else if (input$boxplot_wraprow != " ") {
          paste0(
            "+ \n    facet_grid(",
            input$boxplot_wraprow,
            " ~ . )"
          )
        }
      )


      t <- paste(
        t,
        if (input$boxplot_title != "") {
          paste0(
            "+ \n    labs(title = '",
            input$boxplot_title,
            "')"
          )
        }
      )



      t <- paste(
        t,
        if (input$boxplot_subtitle != "") {
          paste0(
            "+ \n    labs(subtitle = '",
            input$boxplot_subtitle, "')"
          )
        }
      )


      t <- paste(
        t,
        if (input$boxplot_caption != "") {
          paste0(
            "+ \n    labs(caption = '",
            input$boxplot_caption, "')"
          )
        }
      )

      t <- paste(
        t,
        if (input$boxplot_xlab != "") {
          paste0(
            "+ \n    labs(x = '",
            input$boxplot_xlab, "')"
          )
        }
      )


      t <- paste(
        t,
        if (input$boxplot_ylab != "") {
          paste0(
            "+ \n    labs(y = '",
            input$boxplot_ylab, "')"
          )
        }
      )


      t <- paste(
        t,
        if (input$boxplot_theme != " ") {
          paste0(
            "+ \n    ",
            input$boxplot_theme, "()"
          )
        }
      )


      t <- paste0(
        t,
        paste0(input$boxplot_code)
      )

      t <- paste0(
        t,
        paste0("\n \n")
      )

      t
    })


    run <- reactive({
      input$boxplot_run
    })

    #  global <- reactive({
    #    instantglobal()
    #  })


    code_text2 <- reactive({
      #    req(instantglobal())
      #   if(global() == TRUE) {
      #    code_text()

      # } else {
      if (input$instantlocal) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
      #  }
    })



    output$boxplot_plot <- renderPlot(
      {
        eval(parse(text = code_text2()))
      },
      width = width,
      height = height,
    )

    mod_id <- paste0(id, "-boxplot_")

    observeEvent(input$boxplot_showcode, {
      if (input$boxplot_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$boxplot_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })
    output$boxplot_text <- renderText({
      code_text2()
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
    plot_boxplot_ui("module", data)
  )
  server <- function(input, output, session) {
    plot_boxplot_se("module")
  }


  shinyApp(ui, server)



}


plot_boxplot(mtcars)

