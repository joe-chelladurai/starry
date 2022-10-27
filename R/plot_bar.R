

#' Plot - bar
#' @param data data
#' @import shiny
#' @import ggplot2
#' @importFrom shinyjs hidden removeClass addClass toggle runjs
#' @importFrom shinyWidgets switchInput prettyCheckbox
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#'   plot_bar(mtcars)
#' }
#' @export



plot_bar <- function(data, yvar, theme, group, code, shape, size, position, labels, row, prop, column, title, subtitle, xlab, ylab, caption) {



  if (missing(yvar)) {yvar = ""} else {yvar = deparse(substitute(yvar))}
  if (missing(shape)) {shape = ""} else {shape = deparse(substitute(shape))}
  if (missing(group)) {group = ""} else {group = deparse(substitute(group))}
  if (missing(size)) {size = ""} else {size = deparse(substitute(size))}
  if (missing(row)) {row = ""} else {row = deparse(substitute(row))}
  if (missing(column)) {column = ""} else {column = deparse(substitute(column))}
  if (missing(title)) {title = ""}
  if (missing(subtitle)) {subtitle = ""}
  if (missing(caption)) {caption = ""}
  if (missing(xlab)) {xlab = ""}
  if (missing(ylab)) {ylab = ""}
  if (missing(theme)) {theme = "theme_bw"} else {theme = deparse(substitute(theme))}
  if (missing(code)) {code = ""}
  if (missing(position)) {position = "none"} else {position = deparse(substitute(position))}
  if (missing(labels)) {labels = "none"} else {labels = deparse(substitute(labels))}
  if (missing(prop)) {prop = ""} else {prop = deparse(substitute(prop))}




plot_bar_UI <- function(id,
                             data,
                             bar_yvar = yvar,
                             bar_theme = theme,
                             bar_group = group,
                             bar_code = code,
                             bar_shape = shape,
                             bar_size = size,
                             bar_position = position,
                             bar_labels = labels,
                             bar_wraprow = row,
                             bar_prop = prop,
                             bar_wrapcol = column,
                             bar_title = title,
                             bar_subtitle = subtitle,
                             bar_xlab = xlab,
                             bar_ylab = ylab,
                             bar_caption = caption
                          ) {
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
              div("Bar", class = "module-name"),
              div(
                class = "cont2",
                switchInput(NS(id, "bar_instantlocal"),
                  label = "",
                  value = TRUE,
                  size = "mini",
                  onLabel = "",
                  offLabel = ""
                )
              ),
              div(
                class = "cont3",
                actionButton(NS(id, "bar_run"),
                  class = "btn-play",
                  label = icon(name = "fas fa-play", lib = "font-awesome")
                )
              ),
            ),
            selectInput(NS(id, "bar_yvar"),
              label = "Y",
              choices = c("", names(data)),
              selected = bar_yvar
            ),
            selectInput(NS(id, "bar_shape"),
              label = "Fill",
              choices = c("", names(data)),
              selected = bar_shape
            ),
            selectizeInput(NS(id, "bar_size"),
              label = "Outline",
              choices = c("", names(data)),
              selected = bar_size,
              options = list(create = TRUE)
            ),
            radioButtons(NS(id, "bar_position"),
              label = "Position",
              choices = c("none", "stack", "dodge", "dodge2", "fill"),
              selected = bar_position,
              inline = TRUE # )
            ),
            radioButtons(NS(id, "bar_labels"),
              label = "Data Labels",
              choices = c("none", "count", "percent"),
              selected = bar_labels,
              inline = TRUE
            ),
            selectInput(NS(id, "bar_prop"),
              label = "Proportion by: ",
              choices = "",
              selected = bar_prop,
            ),
            numericInput(NS(id, "bar_group"),
              label = "Bar width",
              step = 0.1,
              value = bar_group
            ),
            actionButton(NS(id, "toggle_bar_facet"),
              width = "100%",
              class = "module-style",
              label = "Facet",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(NS(id, "bar_wraprow"),
                label = "Row",
                choices = c("", names(data)),
                selected = bar_wraprow
              ),
              selectInput(NS(id, "bar_wrapcol"),
                label = "Column",
                choices = c("", names(data)),
                selected = bar_wrapcol
              )
            ),
            actionButton(NS(id, "toggle_bar_text"),
              width = "100%",
              class = "module-style",
              label = "Text",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textInput(NS(id, "bar_title"),
                label = "Title",
                value = bar_title
              ),
              textInput(NS(id, "bar_subtitle"),
                label = "Subtitle",
                value = bar_subtitle
              ),
              textInput(NS(id, "bar_caption"),
                label = "Caption",
                value = bar_caption
              ),
              textInput(NS(id, "bar_xlab"),
                label = "X-axis label",
                value = bar_xlab
              ),
              textInput(NS(id, "bar_ylab"),
                label = "Y-axis label",
                value = bar_ylab
              )
            ),
            actionButton(NS(id, "toggle_plot_options"),
              width = "100%",
              class = "module-style",
              label = "Size",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              numericInput(NS(id, "bar_width"),
                label = "Width",
                step = 10,
                width = "100%",
                value = ""
              ),
              numericInput(NS(id, "bar_height"),
                label = "Height",
                step = 10,
                width = "100%",
                value = ""
              )
            ),
            actionButton(NS(id, "toggle_theme_options"),
              width = "100%",
              class = "module-style",
              label = "Theme",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(NS(id, "bar_theme"),
                label = "Theme",
                selected = bar_theme,
                choices = c("",
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
            actionButton(NS(id, "toggle_bar_add_code"),
              width = "100%",
              class = "module-style",
              label = "Code",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textAreaInput(NS(id, "bar_code"),
                value = bar_code,
                label = NULL
              ),
              prettyCheckbox(NS(id, "bar_showcode"),
                label = "show/hide",
                status = "info",
                value = FALSE
              )
            )
          )
        ),
        div(
          class = "result-view",
          fluidRow(plotOutput(NS(id, "bar_plot"), width = "auto", height = "auto")),
          fluidRow(verbatimTextOutput(NS(id, "bar_text")) %>%
            tagAppendAttributes(class = "codeoutput"))
        )
      )
    )
  )
}

plot_bar_SE <- function(id) {
  moduleServer(id, function(input, output, session) {
    req(data)

    observeEvent(data, {
      updateSelectInput(
        session,
        "bar_yvar",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "bar_shape",
        choices = c("", names(data))
      )

      updateSelectizeInput(
        session,
        "bar_size",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "bar_wraprow",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "bar_wrapcol",
        choices = c("", names(data))
      )


    }, ignoreInit = TRUE)

    observe({
      updateSelectInput(
        session,
        "bar_prop",
        choices = c(input$bar_shape, input$bar_yvar, "1")
      )
    })


    observeEvent(input$bar_instantlocal, {
      if (input$bar_instantlocal == TRUE) {
        removeClass("bar_run", "toggle-btnplay")
      } else {
        addClass("bar_run", "toggle-btnplay")
      }
    })


    observeEvent(input$toggle_bar_facet, {
      toggle("bar_wraprow")
      toggle("bar_wrapcol")

      if (input$toggle_bar_facet %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_bar_facet",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_bar_facet",
          icon = icon("fas fa-caret-down")
        )
      }
    })

    observeEvent(input$toggle_bar_text, {
      toggle("bar_title")
      toggle("bar_subtitle")
      toggle("bar_caption")
      toggle("bar_xlab")
      toggle("bar_ylab")

      if (input$toggle_bar_text %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_bar_text",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_bar_text",
          icon = icon("fas fa-caret-down")
        )
      }
    })


    observeEvent(input$toggle_theme_options, {
      toggle("bar_theme")

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
      if (is.na(input$bar_width)) {
        return(600)
      } else {
        input$bar_width
      }
    })


    height <- reactive({
      if (is.na(input$bar_height)) {
        return(400)
      } else {
        input$bar_height
      }
    })


    barwidth <- reactive({
      if (is.na(input$bar_group)) {
        return(1)
      } else {
        input$bar_width
      }
    })

    ns <- NS(id)


    observeEvent(input$toggle_plot_options,
      {
        toggle("bar_width")
        toggle("bar_height")

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


    observeEvent(input$toggle_bar_add_code, {

      toggle("bar_showcode")

      toggle("bar_code")

      if (input$toggle_bar_add_code %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_bar_add_code",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_bar_add_code",
          icon = icon("fas fa-caret-down")
        )
      }
    })





    code_text <- reactive({
      req(isTruthy(input$bar_yvar != ""))

      code <- paste0(
        "\n \n ggplot(data, aes(",
        if (input$bar_yvar != "") {
          paste0("y = factor(", input$bar_yvar, ")")
        },
        if (input$bar_shape != "") {
          paste0(", fill = factor(", input$bar_shape, ")")
        } else {

        },
        if ((input$bar_shape != "" && input$bar_position == "fill") || input$bar_labels == "percent") {
          if (input$bar_shape == "") {
            paste0(", by = 1")
          } else {
            paste0(", by = ", input$bar_prop)
          }
        } else {

        },
        if (input$bar_size != "") {
          paste0(", color = factor(", input$bar_size, ")")
        } else {

        },
        paste0(
          "))"
        ),
        paste0(
          if (input$bar_labels == "percent") {
            " + \n    geom_bar(aes(x = ..prop..), stat = 'prop'"
          } else {
            " + \n    geom_bar(aes(x = ..count..), stat = 'count'"
          },
          if (input$bar_position != "None") {
            paste0(
              ", position = ", # "'",

              if (input$bar_position == "dodge") {
                "position_dodge(preserve = 'single')"
              } else if (input$bar_position == "dodge2") {
                "position_dodge2(preserve = 'single')"
              } else if (input$bar_position == "fill") {
                "'fill'"
              } else if (input$bar_position == "stack") {
                "'stack'"
              } else {

              } # ,
              #   "'"
            )
          },
          if (!is.na(input$bar_group)) {
            paste0(", width = ", input$bar_group)
          } else {

          },
          ")"
        )
      )

      code <- paste(
        code,
        if (input$bar_yvar != "" &&
          input$bar_shape == "") {
          paste()
        }
      )


      code <- paste(
        code,
        if (input$bar_wraprow != "" &&
          input$bar_wrapcol != "") {
          paste0(
            "+ \n    facet_grid(",
            input$bar_wraprow, " ~ ", input$bar_wrapcol, ")"
          )
        } else if (input$bar_wrapcol != "") {
          paste0(
            "+ \n    facet_grid(. ~ ",
            input$bar_wrapcol, ")"
          )
        } else if (input$bar_wraprow != "") {
          paste0(
            "+ \n    facet_grid(",
            input$bar_wraprow,
            " ~ . )"
          )
        }
      )


      code <- paste(
        code,
        if (input$bar_title != "") {
          paste0(
            "+ \n    labs(title = '",
            input$bar_title,
            "')"
          )
        }
      )



      code <- paste(
        code,
        if (input$bar_subtitle != "") {
          paste0(
            "+ \n    labs(subtitle = '",
            input$bar_subtitle, "')"
          )
        }
      )


      code <- paste(
        code,
        if (input$bar_caption != "") {
          paste0(
            "+ \n    labs(caption = '",
            input$bar_caption, "')"
          )
        }
      )

      code <- paste(
        code,
        if (input$bar_xlab != "") {
          paste0(
            "+ \n    labs(x = '",
            input$bar_xlab, "')"
          )
        }
      )


      code <- paste(
        code,
        if (input$bar_ylab != "") {
          paste0(
            "+ \n    labs(y = '",
            input$bar_ylab, "')"
          )
        }
      )


      code <- paste(
        code,
        if (input$bar_position != "fill") {
          if (input$bar_labels == "count") {
            paste0(
              "+ \n    geom_text(aes(",
              "label = ..count..), stat = 'count'",
              if (input$bar_position == "stack") {
                ", position = 'stack'"
              } else if (input$bar_position == "dodge") {
                paste0(
                  ", position = position_dodge(",
                  if (!is.na(input$bar_group)) {
                    paste0("width = ", input$bar_group)
                  } else {
                    paste0("width = 1")
                  },
                  ")"
                )
              } else if (input$bar_position == "dodge2") {
                paste0(
                  ", position = position_dodge2(",
                  if (!is.na(input$bar_group)) {
                    paste0("width = ", input$bar_group)
                  } else {
                    paste0("width = 1")
                  },
                  ")"
                )
              } else if (input$bar_position == "fill") {
                ", position = 'fill'"
              } else {
                #      ")"
              }, ")"
            )
          } else if (input$bar_labels == "percent") {
            paste0(
              "+ \n    geom_text(aes(",
              "x = ..prop..), stat = 'prop'",
              if (input$bar_position == "stack") {
                ", position = 'stack'"
              } else if (input$bar_position == "dodge") {
                paste0(
                  ", position = position_dodge(",
                  if (!is.na(input$bar_group)) {
                    paste0("width = ", input$bar_group)
                  } else {
                    paste0("width = 1")
                  },
                  ")"
                )
              } else if (input$bar_position == "dodge2") {
                paste0(
                  ", position = position_dodge2(",
                  if (!is.na(input$bar_group)) {
                    paste0("width = ", input$bar_group)
                  } else {
                    paste0("width = 1")
                  },
                  ")"
                )
              } else if (input$bar_position == "fill") {
                ", position = 'fill'"
              } else {
                #      ")"
              }, ")"
            )
          } else {

          }
        } else {

        }
      )


      code <- paste(
        code,
        if (input$bar_position == "fill") {
          if (input$bar_labels == "percent") {
            paste0(
              "+ \n    geom_text(stat = 'prop', position = position_fill()) "
            )
          }
        }
      )




      code <- paste(
        code,
        if (input$bar_theme != "") {
          paste0(
            "+ \n    ",
            input$bar_theme, "()"
          )
        }
      )


      code <- paste0(
        code,
        paste0(input$bar_code)
      )

      code
    })

    run <- reactive({
      input$bar_run
    })

    code_text2 <- reactive({
      if (input$bar_instantlocal) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })


    output$bar_plot <- renderPlot(
      {
        eval(parse(text = code_text2()))
      },
      width = width,
      height = height
    )


    mod_id <- paste0(id, "-bar_")

    observeEvent(input$bar_showcode, {
      if (input$bar_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$bar_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })

    output$bar_text <- renderText({
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
  plot_bar_UI("module", data)
)
server <- function(input, output, session) {
  plot_bar_SE("module")
}


shinyApp(ui, server)


}

plot_bar(mtcars, yvar = cyl)
