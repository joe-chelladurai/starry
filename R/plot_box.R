

#' Plot - Box
#' @param data data
#' @param x variable on x axis
#' @param y variable on y axis
#' @param fill fill
#' @param outline outline
#' @param box_width width of box
#' @param row facet row in grid
#' @param column facet column in grid
#' @param theme theme of plot
#' @param title title of plot
#' @param subtitle subtitle of plot
#' @param xlab x-axis
#' @param ylab y-axis label
#' @param caption caption
#' @param width width of plot
#' @param height height of plot
#' @param show_code show the generated code TRUE or FALSE, (default TRUE)
#' @param code add custom code
#' @import shiny
#' @import ggplot2
#' @importFrom shinyjs hidden removeClass addClass toggle runjs
#' @importFrom shinyWidgets switchInput prettyCheckbox updatePrettyCheckbox
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#'   plot_box(mtcars)
#' }
#' @export



plot_box <- function(data, x, y, theme, box_width, fill, outline, row, column, width, height, title, subtitle, xlab, ylab, caption, show_code, code){

  if (missing(x)) {x = ""} else {x = deparse(substitute(x))}
  if (missing(y)) {y = ""} else {y = deparse(substitute(y))}
  if (missing(fill)) {fill = ""} else {fill = deparse(substitute(fill))}
  if (missing(box_width)) {box_width = ""} else {box_width = deparse(substitute(box_width))}
  if (missing(outline)) {outline = ""} else {outline = deparse(substitute(outline))}
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
  if (missing(show_code)) {show_code = TRUE}


plot_box_ui <- function(id,
                            data,
                            box_x = x,
                            box_y = y,
                            box_theme = theme,
                            box_boxwidth = box_width,
                            box_code = code,
                            box_fill = fill,
                            box_outline = outline,
                            box_wraprow = row,
                            box_wrapcol = column,
                            box_width = width,
                            box_height = height,
                            box_title = title,
                            box_subtitle = subtitle,
                            box_xlab = xlab,
                            box_ylab = ylab,
                            box_caption = caption,
                            box_showcode = show_code) {
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
              div(class = "cont3", actionButton(NS(id, "box_run"),
                class = "btn-play",
                label = icon(name = "fas fa-play", lib = "font-awesome")
              ))
            ),
            selectizeInput(NS(id, "box_y"),
              label = "Y",
              choices = c("", names(data)),
              selected = box_y
            ),
            selectInput(NS(id, "box_x"),
              label = "X",
              choices = c("", names(data)),
              selected = box_x
            ),
            selectInput(NS(id, "box_fill"),
              label = "Fill",
              choices = c(" ", names(data)),
              selected = box_fill
            ),
            selectizeInput(NS(id, "box_outline"),
              label = "Outline",
              choices = c(" ", names(data)),
              selected = box_outline,
              options = list(create = TRUE)
            ),
            numericInput(NS(id, "box_boxwidth"),
              label = "Box width",
              step = 0.1,
              value = box_boxwidth
            ),
            actionButton(NS(id, "toggle_box_facet"),
              width = "100%",
              class = "module-style",
              label = "Facet",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(NS(id, "box_wraprow"),
                label = "Row",
                choices = c(" ", names(data)),
                selected = box_wraprow
              ),
              selectInput(NS(id, "box_wrapcol"),
                label = "Column",
                choices = c(" ", names(data)),
                selected = box_wrapcol
              )
            ),
            actionButton(NS(id, "toggle_box_text"),
              width = "100%",
              class = "module-style",
              label = "Text",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textInput(NS(id, "box_title"),
                label = "Title",
                value = box_title
              ),
              textInput(NS(id, "box_subtitle"),
                label = "Subtitle",
                value = box_subtitle
              ),
              textInput(NS(id, "box_caption"),
                label = "Caption",
                value = box_caption
              ),
              textInput(NS(id, "box_xlab"),
                label = "X-axis label",
                value = box_xlab
              ),
              textInput(NS(id, "box_ylab"),
                label = "Y-axis label",
                value = box_ylab
              )
            ),
            actionButton(NS(id, "toggle_plot_options"),
              width = "100%",
              class = "module-style",
              label = "Size",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              numericInput(NS(id, "box_width"),
                label = "Width",
                step = 10,
                width = "100%",
                value = box_width
              ),
              numericInput(NS(id, "box_height"),
                label = "Height",
                step = 10,
                width = "100%",
                value = box_height
              )
            ),
            actionButton(NS(id, "toggle_theme_options"),
              width = "100%",
              class = "module-style",
              label = "Theme",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(NS(id, "box_theme"),
                label = "Theme",
                selected = box_theme,
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
            actionButton(NS(id, "toggle_box_add_code"),
              width = "100%",
              class = "module-style",
              label = "Code",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textAreaInput(NS(id, "box_code"),
                value = box_code,
                label = NULL
              ),
              prettyCheckbox(NS(id, "box_showcode"),
                label = "show/hide",
                status = "info",
                value = box_showcode
              )
            )
          )
        ),
        div(
          class = "result-view",

          fluidRow(plotOutput(NS(id, "box_plot"), width = "auto", height = "auto")),
          fluidRow(verbatimTextOutput(NS(id, "box_text")) %>%
            tagAppendAttributes(class = "codeoutput"))
        )
      )
    )
  )
}

plot_box_se <- function(id) {
  moduleServer(id, function(input, output, session) {
    req(data)

    observeEvent(data, {
      updateSelectizeInput(
        session,
        "box_x",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "box_y",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "box_fill",
        choices = c(" ", names(data))
      )

      updateSelectizeInput(
        session,
        "box_outline",
        choices = c(" ", names(data))
      )

      updateSelectInput(
        session,
        "box_wraprow",
        choices = c(" ", names(data))
      )

      updateSelectInput(
        session,
        "box_wrapcol",
        choices = c(" ", names(data))
      )

      if (!is.null(input$box_showcode)) {
        updatePrettyCheckbox(
          session,
          "box_showcode",
          value = TRUE
        )
      }
    })

    observeEvent(input$instantlocal, {
      if (input$instantlocal == TRUE) {
        removeClass("box_run", "toggle-btnplay")
      } else {
        addClass("box_run", "toggle-btnplay")
      }
    })


    observeEvent(input$toggle_box_facet, {
      toggle("box_wraprow")
      toggle("box_wrapcol")

      if (input$toggle_box_facet %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_box_facet",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_box_facet",
          icon = icon("fas fa-caret-down")
        )
      }
    })

    observeEvent(input$toggle_box_text, {
      toggle("box_title")
      toggle("box_subtitle")
      toggle("box_caption")
      toggle("box_xlab")
      toggle("box_ylab")

      if (input$toggle_box_text %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_box_text",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_box_text",
          icon = icon("fas fa-caret-down")
        )
      }
    })


    observeEvent(input$toggle_theme_options, {
      toggle("box_theme")

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
      if (is.na(input$box_width)) {
        return(600)
      } else {
        input$box_width
      }
    })


    height <- reactive({
      if (is.na(input$box_height)) {
        return(400)
      } else {
        input$box_height
      }
    })


    boxwidth <- reactive({
      if (is.na(input$box_boxwidth)) {
        return(1)
      } else {
        input$box_boxwidth
      }
    })

    ns <- NS(id)


    observeEvent(input$toggle_plot_options,
      {
        toggle("box_width")
        toggle("box_height")

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




    observeEvent(input$toggle_box_add_code, {
      toggle("box_showcode")

      toggle("box_code")

      if (input$toggle_box_add_code %% 2 == 1) {
        updateActionButton(
          session,
          "toggle_box_add_code",
          icon = icon("fas fa-caret-up")
        )
      } else {
        updateActionButton(
          session,
          "toggle_box_add_code",
          icon = icon("fas fa-caret-down")
        )
      }
    })


    code_text <- reactive({
      req(
        isTruthy(input$box_x != "") |
          isTruthy(input$box_y != "")
      )

      t <- paste0(
        "\n \n ggplot(data, aes(",
        if (input$box_x != "") {
          paste0("factor(", input$box_x, ")")
        },
        if (input$box_y != "" && input$box_x != "") {
          paste0(", ", input$box_y)
        } else if (input$box_y != "") {
          paste0("y = ", input$box_y)
        } else {

        },
        if (input$box_fill != " ") {
          paste0(", fill = factor(", input$box_fill, ")")
        } else {

        },
        if (input$box_outline != " ") {
          paste0(", color = factor(", input$box_outline, ")")
        } else {

        },
        paste0(
          "))"
        ),
        paste0(
          " + \n    geom_boxplot(",
          if (!is.na(input$box_boxwidth)) {
            paste0("width = ", input$box_boxwidth)
          } else {

          },
          ")"
        )
      )

      t <- paste(
        t,
        if (input$box_y != "" &&
          input$box_x == "" &&
          input$box_fill == " ") {
          paste(
            "+ \n    scale_x_discrete()"
          )
        }
      )


      t <- paste(
        t,
        if (input$box_wraprow != " " &&
          input$box_wrapcol != " ") {
          paste0(
            "+ \n    facet_grid(",
            input$box_wraprow, " ~ ", input$box_wrapcol, ")"
          )
        } else if (input$box_wrapcol != " ") {
          paste0(
            "+ \n    facet_grid(. ~ ",
            input$box_wrapcol, ")"
          )
        } else if (input$box_wraprow != " ") {
          paste0(
            "+ \n    facet_grid(",
            input$box_wraprow,
            " ~ . )"
          )
        }
      )


      t <- paste(
        t,
        if (input$box_title != "") {
          paste0(
            "+ \n    labs(title = '",
            input$box_title,
            "')"
          )
        }
      )



      t <- paste(
        t,
        if (input$box_subtitle != "") {
          paste0(
            "+ \n    labs(subtitle = '",
            input$box_subtitle, "')"
          )
        }
      )


      t <- paste(
        t,
        if (input$box_caption != "") {
          paste0(
            "+ \n    labs(caption = '",
            input$box_caption, "')"
          )
        }
      )

      t <- paste(
        t,
        if (input$box_xlab != "") {
          paste0(
            "+ \n    labs(x = '",
            input$box_xlab, "')"
          )
        }
      )


      t <- paste(
        t,
        if (input$box_ylab != "") {
          paste0(
            "+ \n    labs(y = '",
            input$box_ylab, "')"
          )
        }
      )


      t <- paste(
        t,
        if (input$box_theme != " ") {
          paste0(
            "+ \n    ",
            input$box_theme, "()"
          )
        }
      )


      t <- paste0(
        t,
        paste0(input$box_code)
      )

      t <- paste0(
        t,
        paste0("\n \n")
      )

      t
    })


    run <- reactive({
      input$box_run
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



    output$box_plot <- renderPlot(
      {
        eval(parse(text = code_text2()))
      },
      width = width,
      height = height,
    )

    mod_id <- paste0(id, "-box_")

    observeEvent(input$box_showcode, {
      if (input$box_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$box_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })
    output$box_text <- renderText({
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
    plot_box_ui("module", data)
  )
  server <- function(input, output, session) {
    plot_box_se("module")
  }


  shinyApp(ui, server)



}

