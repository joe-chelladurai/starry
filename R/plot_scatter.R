

#' Plot - Scatter
#' @param data data
#' @import shiny
#' @import ggplot2
#' @importFrom shinyjs hidden removeClass addClass toggle runjs
#' @importFrom shinyWidgets switchInput prettyCheckbox
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#'   plot_scatter(mtcars)
#' }
#' @export

plot_scatter <- function(data) {


plot_scatter_UI <- function(id,
                           data) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("placeholder1"), class = "parent",
      div(
        class = "inputresultview", style = "display:flex; margin-top: 10px; margin-bottom: 10px;",
        div(
          class = "input-view well", style = "padding-right: 0; width: 350px;",
          div(
            class = "custom-scroll",
            div(
              class = "grid-container",
              div("Scatter", class = "module-name"),
              div(
                class = "cont2",
                switchInput(NS(id, "scatter_instant"),
                            label = "",
                            value = TRUE,
                            size = "mini",
                            onLabel = "",
                            offLabel = "",
                )
              ),
              div(
                class = "cont3",
                actionButton(NS(id, "scatter_run"),
                             class = "btn-play",
                             label = icon(name = "fas fa-play", lib = "font-awesome")
                )
              )
            ),
            selectInput(
              NS(id, "scatter_xvar"),
              label = "X",
              choices = c("", names(data)),
              selected = ""
            ),
            selectInput(
              NS(id, "scatter_yvar"),
              label = "Y",
              choices = c("", names(data)),
              selected = ""
            ),
            checkboxInput(
              NS(id, "scatter_attr"),
              label = "Line"
            ),
            actionButton(
              NS(id, "toggle_scatter_group"),
              width = "100%",
              class = "module-style",
              label = "Group",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(
                NS(id, "scatter_group"),
                label = "Color",
                choices = c("", names(data)),
                selected = ""
              ),
              selectInput(
                NS(id, "scatter_shape"),
                label = "Shape",
                choices = c("", names(data)),
                selected = ""
              ),
              selectInput(
                NS(id, "scatter_size"),
                label = "Size",
                choices = c("", names(data)),
                selected = ""
              )
            ),




            # FACET
            actionButton(
              NS(id, "toggle_scatter_facet"),
              width = "100%",
              class = "module-style",
              label = "Facet",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(
                NS(id, "scatter_wraprow"),
                label = "Row",
                choices = c("", names(data)),
                selected = ""
              ),
              selectInput(
                NS(id, "scatter_wrapcol"),
                label = "Column",
                choices = c("", names(data)),
                selected = ""
              )
            ),

            # TExT
            actionButton(NS(id, "toggle_scatter_text"),
                         width = "100%",
                         class = "module-style",
                         label = "Text",
                         icon = icon("fas fa-caret-down")
            ),
            hidden(
              textInput(NS(id, "scatter_title"),
                        label = "Title",
                        value = ""
              ),
              textInput(NS(id, "scatter_subtitle"),
                        label = "Subtitle",
                        value = ""
              ),
              textInput(NS(id, "scatter_caption"),
                        label = "Caption",
                        value = ""
              ),
              textInput(NS(id, "scatter_xlab"),
                        label = "X-axis label",
                        value = ""
              ),
              textInput(NS(id, "scatter_ylab"),
                        label = "Y-axis label",
                        value = ""
              )
            ),

            # SIZE
            actionButton(
              NS(id, "toggle_scatter_size"),
              width = "100%",
              class = "module-style",
              label = "Size",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              numericInput(
                NS(id, "scatter_width"),
                label = "Width",
                step = 10,
                width = "100%",
                value = ""
              ),
              numericInput(
                NS(id, "scatter_height"),
                label = "Height",
                step = 10,
                width = "100%",
                value = ""
              )
            ),
            # THEME
            actionButton(NS(id, "toggle_scatter_theme"),
                         width = "100%",
                         class = "module-style",
                         label = "Theme",
                         icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(NS(id, "scatter_theme"),
                          label = "Theme",
                          selected = "",
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
            actionButton(
              NS(id, "toggle_scatter_code"),
              width = "100%",
              class = "module-style",
              label = "Code",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textAreaInput(
                NS(id, "scatter_code"),
                value = "",
                label = NULL
              ),
              prettyCheckbox(NS(id, "scatter_showcode"),
                             label = "show/hide",
                             status = "info"
              )
            )
          )
        ),
        div(
          class = "result-view",

          fluidRow(
            plotOutput(
              NS(id, "scatter_plot"),
              width = "auto", height = "auto"
            )
          ),
          fluidRow(
            verbatimTextOutput(
              NS(id, "scatter_text")
            ) %>%
              tagAppendAttributes(class = "codeoutput")
          )
        )
      )
    )
  )
}

plot_scatter_SE <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data, {
      updateSelectInput(
        session,
        "scatter_xvar",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "scatter_yvar",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "scatter_group",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "scatter_shape",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "scatter_size",
        choices = c("", names(data))
      )
    })




    ns <- NS(id)







    observeEvent(input$toggle_scatter_group, {
      toggle("scatter_group")
      toggle("scatter_shape")
      toggle("scatter_size")
      if (input$toggle_scatter_group %% 2 == 1) {
        updateActionButton(session, "toggle_scatter_group", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_scatter_group", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_scatter_size,
                 {
                   toggle("scatter_height")
                   toggle("scatter_width")
                   if (input$toggle_scatter_size %% 2 == 1) {
                     updateActionButton(session, "toggle_scatter_size", icon = icon("fas fa-caret-up"))
                   } else {
                     updateActionButton(session, "toggle_scatter_size", icon = icon("fas fa-caret-down"))
                   }
                 },
                 ignoreInit = TRUE
    )

    observeEvent(input$toggle_scatter_code, {
      toggle("scatter_showcode")
      toggle("scatter_code")
      if (input$toggle_scatter_code %% 2 == 1) {
        updateActionButton(session, "toggle_scatter_code", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_scatter_code", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_scatter_facet, {
      toggle("scatter_wraprow")
      toggle("scatter_wrapcol")
      if (input$toggle_scatter_facet %% 2 == 1) {
        updateActionButton(session, "toggle_scatter_facet", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_scatter_facet", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_scatter_text, {
      toggle("scatter_title")
      toggle("scatter_subtitle")
      toggle("scatter_caption")
      toggle("scatter_xlab")
      toggle("scatter_ylab")
      if (input$toggle_scatter_text %% 2 == 1) {
        updateActionButton(session, "toggle_scatter_text", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_scatter_text", icon = icon("fas fa-caret-down"))
      }
    })


    observeEvent(input$toggle_scatter_theme, {
      toggle("scatter_theme")
      if (input$toggle_scatter_theme %% 2 == 1) {
        updateActionButton(session, "toggle_scatter_theme", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_scatter_theme", icon = icon("fas fa-caret-down"))
      }
    })






    code_text <- reactive({
      req(isTruthy(input$scatter_xvar != "") && isTruthy(input$scatter_yvar != ""))


      t <- paste0(
        "\n \n ggplot(data, aes(", input$scatter_xvar, ", ", input$scatter_yvar,
        if (input$scatter_group != "") {
          paste0(", color = factor(", input$scatter_group, ")")
        },
        if (input$scatter_shape != "") {
          paste0(", shape = factor(", input$scatter_shape, ")")
        },
        if (input$scatter_size != "") {
          paste0(", size = ", input$scatter_size)
        },
        ")) + \n    geom_point()"
      )



      t <- paste(
        t,
        if (input$scatter_wraprow != "" && input$scatter_wrapcol != "") {
          paste0("+ \n    facet_grid(", input$scatter_wraprow, " ~ ", input$scatter_wrapcol, ")")
        } else if (input$scatter_wrapcol != "") {
          paste0("+ \n    facet_grid(. ~ ", input$scatter_wrapcol, ")")
        } else if (input$scatter_wraprow != "") {
          paste0("+ \n    facet_grid(", input$scatter_wraprow, " ~ . )")
        }
      )

      t <- paste(
        t,
        if (as.numeric(input$scatter_attr) == "1") {
          paste0("+ \n     geom_smooth(method = 'lm', se = FALSE)")
        }
      )


      t <- paste(
        t,
        if (input$scatter_theme != "") {
          paste0("+ \n    ", input$scatter_theme, "()")
        }
      )

      t <- paste(
        t,
        if (input$scatter_title != "") {
          paste0("+ \n    labs(title = '", input$scatter_title, "')")
        }
      )

      t <- paste(
        t,
        if (input$scatter_subtitle != "") {
          paste0("+ \n    labs(subtitle = '", input$scatter_subtitle, "')")
        }
      )

      t <- paste(
        t,
        if (input$scatter_caption != "") {
          paste0("+ \n    labs(caption = '", input$scatter_caption, "')")
        }
      )

      t <- paste(
        t,
        if (input$scatter_xlab != "") {
          paste0("+ \n    labs(x = '", input$scatter_xlab, "')")
        }
      )

      t <- paste(
        t,
        if (input$scatter_ylab != "") {
          paste0("+ \n    labs(y = '", input$scatter_ylab, "')")
        }
      )


      t <- paste0(
        t,
        paste0(input$scatter_code)
      )

      t
    })


    width <- reactive({
      if (is.na(input$scatter_width)) {
        return(600)
      } else {
        input$scatter_width
      }
    })



    height <- reactive({
      if (is.na(input$scatter_height)) {
        return(400)
      } else {
        input$scatter_height
      }
    })


    observeEvent(input$scatter_instant, {
      if (input$scatter_instant == TRUE) {
        removeClass("scatter_run", "toggle-btnplay")
      } else {
        addClass("scatter_run", "toggle-btnplay")
      }
    })


    run <- reactive({
      input$scatter_run
    })

    code_text2 <- reactive({
      if (input$scatter_instant) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })


    output$scatter_plot <- renderPlot(
      {
        eval(parse(text = code_text2()))
      },
      width = width,
      height = height
    )




    mod_id <- paste0(id, "-scatter_")

    observeEvent(input$scatter_showcode, {
      if (input$scatter_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$scatter_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })

    output$scatter_text <- renderText({
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
              .btn-play:hover {color: #ffffff;background-color:  #ffffff;border-color:  #ffffff;}
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
  plot_scatter_UI("module", data)
)
server <- function(input, output, session) {
  plot_scatter_SE("module")
}


shinyApp(ui, server)


}



