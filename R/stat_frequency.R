

#' Frequency App
#' @param data data
#' @import shiny
#' @importFrom dplyr count
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#' stat_frequency(mtcars)
#' }
#' @export

stat_frequency <- function(data) {

  frequency_ui <- function(id,
                           data) {
    ns <- NS(id)

    fluidPage(
      titlePanel("Frequency"),
      sidebarLayout(
        sidebarPanel(width = 3,
                     selectizeInput(NS(id, "frequency_xvar"),
                                    label = "X",
                                    choices = c("", names(data)),
                     )),
        mainPanel(width = 9,
                  fluidRow(tableOutput(NS(id, "frequency_table")))
        )
      ))
  }

  frequency_se <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      observeEvent(data, {
        updateSelectizeInput(
          session,
          "frequency_xvar",
          choices = c("", names(data))
        )
      })

      ns <- NS(id)

      code_text <- reactive({
        t <- paste0(
          "\n \n data |> ",
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
    frequency_ui("module", data)
  )

  server <- function(input, output, session) {
    frequency_se("module", data)
  }
  shinyApp(ui, server)
}


