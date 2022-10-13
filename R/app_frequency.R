

#' Frequency App
#' @param df data
#' @import shiny
#' @importFrom dplyr count
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#' app_frequency(mtcars)
#' }
#' @export

app_frequency <- function(df) {
  frequency_ui <- function(id,
                           df) {
    ns <- NS(id)

    fluidPage(
      titlePanel("Frequency"),
      sidebarLayout(
        sidebarPanel(width = 3,
                     selectizeInput(NS(id, "frequency_xvar"),
                                    label = "X",
                                    choices = c("", names(df)),
                     )),
        mainPanel(width = 9,
                  fluidRow(tableOutput(NS(id, "frequency_table")))
        )
      ))
  }

  frequency_se <- function(id, df) {
    moduleServer(id, function(input, output, session) {
      observeEvent(df, {
        updateSelectizeInput(
          session,
          "frequency_xvar",
          choices = c("", names(df))
        )
      })

      ns <- NS(id)

      code_text <- reactive({
        t <- paste0(
          "\n \n df |> ",
          "\n    count(", input$frequency_xvar, ")"
        )
      })

      code_text2 <- reactive({
        code_text()
      })

      output$frequency_table <- renderTable(
        {
          req(input$frequency_xvar)
          eval(parse(text = code_text2()))
        }

      )
    })
  }
  ui <- fluidPage(

    theme = bslib::bs_theme(enable_rounded = FALSE),
    tags$style(' .well  { background-color: "#ffffff" !important;}'),
    frequency_ui("module", df)
  )

  server <- function(input, output, session) {
    frequency_se("module", df)
  }
  shinyApp(ui, server)
}


