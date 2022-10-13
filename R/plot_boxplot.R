

#' Plot - Boxplot
#' @param data data
#' @import shiny
#' @import ggplot2
#' @importFrom shinyjs hidden removeClass addClass toggle
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#'   plot_boxplot(mtcars)
#' }
#' @export


plot_boxplot <- function(data) {
  plot_boxplot_UI <- function(id, data) {
    ns <- NS(id)
    fluidPage(
      titlePanel("Boxplot"),
      sidebarLayout(
        sidebarPanel(
          selectizeInput(NS(id, "boxplot_yvar"),
            label = "Y",
            choices = c("", names(data)),
            selected = ""
          ),
          selectInput(NS(id, "boxplot_xvar"),
            label = "X",
            choices = c("", names(data)),
            selected = ""
          ),
          selectInput(NS(id, "boxplot_shape"),
            label = "Fill",
            choices = c(" ", names(data)),
            selected = ""
          ),
          selectizeInput(NS(id, "boxplot_size"),
            label = "Outline",
            choices = c(" ", names(data)),
            selected = "",
            options = list(create = TRUE)
          ),
          numericInput(NS(id, "boxplot_boxwidth"),
            label = "Box width",
            step = 0.1,
            value = 1
          ),
          actionButton(NS(id, "toggle_theme_options"),
            width = "100%",
            class = "module-style",
            label = "Theme",
            icon = icon("fas fa-caret-down")
          ),
          hidden(selectInput(NS(id, "boxplot_theme"),
            label = "Theme",
            selected = "",
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
          )),
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
              selected = ""
            ),
            selectInput(NS(id, "boxplot_wrapcol"),
              label = "Column",
              choices = c(" ", names(data)),
              selected = ""
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
              value = ""
            ),
            textInput(NS(id, "boxplot_subtitle"),
              label = "Subtitle",
              value = ""
            ),
            textInput(NS(id, "boxplot_caption"),
              label = "Caption",
              value = ""
            ),
            textInput(NS(id, "boxplot_xlab"),
              label = "X-axis label",
              value = ""
            ),
            textInput(NS(id, "boxplot_ylab"),
              label = "Y-axis label",
              value = ""
            )
          )
        ),
        mainPanel(
          fluidRow(plotOutput(NS(id, "boxplot_plot"))),
        )
      )
    )
  }

  plot_boxplot_SE <- function(id) {
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

      boxwidth <- reactive({
        if (is.na(input$boxplot_boxwidth)) {
          return(1)
        } else {
          input$boxplot_boxwidth
        }
      })


      ns <- NS(id)

      code_text <- reactive({
        req(isTruthy(input$boxplot_xvar != "") | isTruthy(input$boxplot_yvar != ""))

        t <- paste0(
          "\n \n ggplot(data, aes(",
          if (input$boxplot_xvar != "") {
            paste0("factor(", input$boxplot_xvar, ")")
          },
          if (input$boxplot_yvar != "" && input$boxplot_xvar != "") {
            paste0(", ", input$boxplot_yvar)
          } else if (input$boxplot_yvar != " ") {
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
          "))",
          paste0(
            " + \n    geom_boxplot(",
            if (!is.na(input$boxplot_boxwidth)) {
              paste0("width = ", input$boxplot_boxwidth)
            } else {

            },
            ")"
          )
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

        t
      })


      code_text2 <- reactive({
        code_text()
      })


      output$boxplot_plot <- renderPlot(
        {
          eval(parse(text = code_text2()))
        },
        width = 500,
        height = 500
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
        HTML("
             .module-style { text-align: left;
                              background-color: #faf9f7;
                              border: 0;
                              margin-bottom: 5px;
          }
            .well  { background-color: #ffffff !important;}'
             ")
      )
    ),
    theme = bslib::bs_theme(),
    plot_boxplot_UI("module", data)
  )

  server <- function(input, output, session) {
    plot_boxplot_SE("module")
  }


  shinyApp(ui, server)
}

