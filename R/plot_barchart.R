

#' Plot - Barchart
#' @param data data
#' @import shiny
#' @import ggplot2
#' @importFrom shinyjs hidden removeClass addClass toggle runjs
#' @importFrom shinyWidgets switchInput prettyCheckbox
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#'   plot_barchart(mtcars)
#' }
#' @export



plot_barchart <- function(data, yvar, num, theme, group, code, shape, size, position, labels, row, prop, column, title, subtitle, xlab, ylab, caption) {



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
  if (missing(num)) {num = ""} else {num = deparse(substitute(num))}
  if (missing(code)) {code = ""}
  if (missing(position)) {position = ""}
  if (missing(labels)) {labels = ""} else {labels = deparse(substitute(labels))}
  if (missing(prop)) {prop = ""} else {prop = deparse(substitute(prop))}




plot_barchart_UI <- function(id,
                             data,
                             barchart_yvar = yvar) {
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
                switchInput(NS(id, "barchart_instantlocal"),
                  label = "",
                  value = TRUE,
                  size = "mini",
                  onLabel = "",
                  offLabel = ""
                )
              ),
              div(
                class = "cont3",
                actionButton(NS(id, "barchart_run"),
                  class = "btn-play",
                  label = icon(name = "fas fa-play", lib = "font-awesome")
                )
              ),
            ),
            selectInput(NS(id, "barchart_yvar"),
              label = "Y",
              choices = c("", names(data)),
              selected = barchart_yvar
            ),
            selectInput(NS(id, "barchart_shape"),
              label = "Fill",
              choices = c("", names(data)),
           #   selected = barchart_shape
            ),
            selectizeInput(NS(id, "barchart_size"),
              label = "Outline",
              choices = c("", names(data)),
      #        selected = barchart_size,
              options = list(create = TRUE)
            ),
            radioButtons(NS(id, "barchart_position"),
              label = "Position",
              choices = c("None", "stack", "dodge", "dodge2", "fill"),
      #        selected = barchart_position,
              inline = TRUE # )
            ),
            radioButtons(NS(id, "barchart_labels"),
              label = "Data Labels",
              choices = c("None", "Count", "Percent"),
       #       selected = barchart_labels,
              inline = TRUE
            ),
            selectInput(NS(id, "barchart_prop"),
              label = "Proportion by: ",
              choices = "",
      #        selected = barchart_prop,
            ),
            numericInput(NS(id, "barchart_group"),
              label = "Bar width",
              step = 0.1,
              value = 0.1 #barchart_group
            ),
            actionButton(NS(id, "toggle_barchart_facet"),
              width = "100%",
              class = "module-style",
              label = "Facet",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(NS(id, "barchart_wraprow"),
                label = "Row",
                choices = c("", names(data)),
            #    selected = barchart_wraprow
              ),
              selectInput(NS(id, "barchart_wrapcol"),
                label = "Column",
                choices = c("", names(data)),
            #    selected = barchart_wrapcol
              )
            ),
            actionButton(NS(id, "toggle_barchart_text"),
              width = "100%",
              class = "module-style",
              label = "Text",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textInput(NS(id, "barchart_title"),
                label = "Title",
          #      value = barchart_title
              ),
              textInput(NS(id, "barchart_subtitle"),
                label = "Subtitle",
        #        value = barchart_subtitle
              ),
              textInput(NS(id, "barchart_caption"),
                label = "Caption",
        #        value = barchart_caption
              ),
              textInput(NS(id, "barchart_xlab"),
                label = "X-axis label",
       #         value = barchart_xlab
              ),
              textInput(NS(id, "barchart_ylab"),
                label = "Y-axis label",
        #        value = barchart_ylab
              )
            ),
            actionButton(NS(id, "toggle_plot_options"),
              width = "100%",
              class = "module-style",
              label = "Size",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              numericInput(NS(id, "barchart_width"),
                label = "Width",
                step = 10,
                width = "100%",
                value = ""
              ),
              numericInput(NS(id, "barchart_height"),
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
              selectInput(NS(id, "barchart_theme"),
                label = "Theme",
         #       selected = barchart_theme,
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
            actionButton(NS(id, "toggle_barchart_add_code"),
              width = "100%",
              class = "module-style",
              label = "Code",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textAreaInput(NS(id, "barchart_code"),
          #      value = barchart_code,
                label = NULL
              ),
              prettyCheckbox(NS(id, "barchart_showcode"),
                label = "show/hide",
                status = "info",
                value = FALSE
              )
            )
          )
        ),
        div(
          class = "result-view",
       #   fluidRow(actionButton(NS(id, "barchart_rmv"),
       #     class = "child",
       #     label = NULL,
       #     icon = icon("fas fa-times"),
       #     value = barchart_rmv
       #   )),
          fluidRow(plotOutput(NS(id, "barchart_plot"), width = "auto", height = "auto")),
          fluidRow(verbatimTextOutput(NS(id, "barchart_text")) %>%
            tagAppendAttributes(class = "codeoutput"))
        )
      )
    )
  )
}

plot_barchart_SE <- function(id) {
  moduleServer(id, function(input, output, session) {
    req(data)

    observeEvent(data, {
      updateSelectInput(
        session,
        "barchart_yvar",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "barchart_shape",
        choices = c("", names(data))
      )

      updateSelectizeInput(
        session,
        "barchart_size",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "barchart_wraprow",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "barchart_wrapcol",
        choices = c("", names(data))
      )


    }, ignoreInit = TRUE)

    observe({
      updateSelectInput(
        session,
        "barchart_prop",
        choices = c(input$barchart_shape, input$barchart_yvar, "1")
      )
    })


    observeEvent(input$barchart_instantlocal, {
      if (input$barchart_instantlocal == TRUE) {
        removeClass("barchart_run", "toggle-btnplay")
      } else {
        addClass("barchart_run", "toggle-btnplay")
      }
    })


    observeEvent(input$toggle_barchart_facet, {
      toggle("barchart_wraprow")
      toggle("barchart_wrapcol")

      if (input$toggle_barchart_facet %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_barchart_facet",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_barchart_facet",
          icon = icon("fas fa-caret-down")
        )
      }
    })

    observeEvent(input$toggle_barchart_text, {
      toggle("barchart_title")
      toggle("barchart_subtitle")
      toggle("barchart_caption")
      toggle("barchart_xlab")
      toggle("barchart_ylab")

      if (input$toggle_barchart_text %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_barchart_text",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_barchart_text",
          icon = icon("fas fa-caret-down")
        )
      }
    })


    observeEvent(input$toggle_theme_options, {
      toggle("barchart_theme")

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
      if (is.na(input$barchart_width)) {
        return(600)
      } else {
        input$barchart_width
      }
    })


    height <- reactive({
      if (is.na(input$barchart_height)) {
        return(400)
      } else {
        input$barchart_height
      }
    })


    barwidth <- reactive({
      if (is.na(input$barchart_group)) {
        return(1)
      } else {
        input$barchart_width
      }
    })

    ns <- NS(id)



  #  observeEvent(input$barchart_rmv, {
  #    removeUI(
  #      selector = paste0("#", ns("placeholder1"))
  #    )
  #  })


    observeEvent(input$toggle_plot_options,
      {
        toggle("barchart_width")
        toggle("barchart_height")

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


    observeEvent(input$toggle_barchart_add_code, {

      toggle("barchart_showcode")

      toggle("barchart_code")

      if (input$toggle_barchart_add_code %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_barchart_add_code",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_barchart_add_code",
          icon = icon("fas fa-caret-down")
        )
      }
    })





    code_text <- reactive({
      req(isTruthy(input$barchart_yvar != ""))

      t <- paste0(
        "\n \n ggplot(data, aes(",
        if (input$barchart_yvar != "") {
          paste0("y = factor(", input$barchart_yvar, ")")
        },
        if (input$barchart_shape != "") {
          paste0(", fill = factor(", input$barchart_shape, ")")
        } else {

        },
        if ((input$barchart_shape != "" && input$barchart_position == "fill") || input$barchart_labels == "Percent") {
          if (input$barchart_shape == "") {
            paste0(", by = 1")
          } else {
            paste0(", by = ", input$barchart_prop)
          }
        } else {

        },
        if (input$barchart_size != "") {
          paste0(", color = factor(", input$barchart_size, ")")
        } else {

        },
        paste0(
          "))"
        ),
        paste0(
          if (input$barchart_labels == "Percent") {
            " + \n    geom_bar(aes(x = ..prop..), stat = 'prop'"
          } else {
            " + \n    geom_bar(aes(x = ..count..), stat = 'count'"
          },
          if (input$barchart_position != "None") {
            paste0(
              ", position = ", # "'",

              if (input$barchart_position == "dodge") {
                "position_dodge(preserve = 'single')"
              } else if (input$barchart_position == "dodge2") {
                "position_dodge2(preserve = 'single')"
              } else if (input$barchart_position == "fill") {
                "'fill'"
              } else if (input$barchart_position == "stack") {
                "'stack'"
              } else {

              } # ,
              #   "'"
            )
          },
          if (!is.na(input$barchart_group)) {
            paste0(", width = ", input$barchart_group)
          } else {

          },
          ")"
        )
      )

      t <- paste(
        t,
        if (input$barchart_yvar != "" &&
          input$barchart_shape == "") {
          paste()
        }
      )


      t <- paste(
        t,
        if (input$barchart_wraprow != "" &&
          input$barchart_wrapcol != "") {
          paste0(
            "+ \n    facet_grid(",
            input$barchart_wraprow, " ~ ", input$barchart_wrapcol, ")"
          )
        } else if (input$barchart_wrapcol != "") {
          paste0(
            "+ \n    facet_grid(. ~ ",
            input$barchart_wrapcol, ")"
          )
        } else if (input$barchart_wraprow != "") {
          paste0(
            "+ \n    facet_grid(",
            input$barchart_wraprow,
            " ~ . )"
          )
        }
      )


      t <- paste(
        t,
        if (input$barchart_title != "") {
          paste0(
            "+ \n    labs(title = '",
            input$barchart_title,
            "')"
          )
        }
      )



      t <- paste(
        t,
        if (input$barchart_subtitle != "") {
          paste0(
            "+ \n    labs(subtitle = '",
            input$barchart_subtitle, "')"
          )
        }
      )


      t <- paste(
        t,
        if (input$barchart_caption != "") {
          paste0(
            "+ \n    labs(caption = '",
            input$barchart_caption, "')"
          )
        }
      )

      t <- paste(
        t,
        if (input$barchart_xlab != "") {
          paste0(
            "+ \n    labs(x = '",
            input$barchart_xlab, "')"
          )
        }
      )


      t <- paste(
        t,
        if (input$barchart_ylab != "") {
          paste0(
            "+ \n    labs(y = '",
            input$barchart_ylab, "')"
          )
        }
      )


      t <- paste(
        t,
        if (input$barchart_position != "fill") {
          if (input$barchart_labels == "Count") {
            paste0(
              "+ \n    geom_text(aes(",
              "label = ..count..), stat = 'count'",
              if (input$barchart_position == "stack") {
                ", position = 'stack'"
              } else if (input$barchart_position == "dodge") {
                paste0(
                  ", position = position_dodge(",
                  if (!is.na(input$barchart_group)) {
                    paste0("width = ", input$barchart_group)
                  } else {
                    paste0("width = 1")
                  },
                  ")"
                )
              } else if (input$barchart_position == "dodge2") {
                paste0(
                  ", position = position_dodge2(",
                  if (!is.na(input$barchart_group)) {
                    paste0("width = ", input$barchart_group)
                  } else {
                    paste0("width = 1")
                  },
                  ")"
                )
              } else if (input$barchart_position == "fill") {
                ", position = 'fill'"
              } else {
                #      ")"
              }, ")"
            )
          } else if (input$barchart_labels == "Percent") {
            paste0(
              "+ \n    geom_text(aes(",
              "x = ..prop..), stat = 'prop'",
              if (input$barchart_position == "stack") {
                ", position = 'stack'"
              } else if (input$barchart_position == "dodge") {
                paste0(
                  ", position = position_dodge(",
                  if (!is.na(input$barchart_group)) {
                    paste0("width = ", input$barchart_group)
                  } else {
                    paste0("width = 1")
                  },
                  ")"
                )
              } else if (input$barchart_position == "dodge2") {
                paste0(
                  ", position = position_dodge2(",
                  if (!is.na(input$barchart_group)) {
                    paste0("width = ", input$barchart_group)
                  } else {
                    paste0("width = 1")
                  },
                  ")"
                )
              } else if (input$barchart_position == "fill") {
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


      t <- paste(
        t,
        if (input$barchart_position == "fill") {
          if (input$barchart_labels == "Percent") {
            paste0(
              "+ \n    geom_text(stat = 'prop', position = position_fill()) "
            )
          }
        }
      )




      t <- paste(
        t,
        if (input$barchart_theme != "") {
          paste0(
            "+ \n    ",
            input$barchart_theme, "()"
          )
        }
      )


      t <- paste0(
        t,
        paste0(input$barchart_code)
      )

      t
    })

    run <- reactive({
      input$barchart_run
    })

    code_text2 <- reactive({
      if (input$barchart_instantlocal) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })


    output$barchart_plot <- renderPlot(
      {
        eval(parse(text = code_text2()))
      },
      width = width,
      height = height
    )


    mod_id <- paste0(id, "-barchart_")

    observeEvent(input$barchart_showcode, {
      if (input$barchart_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$barchart_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })

    output$barchart_text <- renderText({
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
  plot_barchart_UI("module", data)
)
server <- function(input, output, session) {
  plot_barchart_SE("module")
}


shinyApp(ui, server)


}

plot_barchart(mtcars, yvar = cyl)
