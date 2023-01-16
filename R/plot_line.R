

#' Plot - Line
#' @param data data
#' @param xvar xvar
#' @param yvar yvar
#' @param group group
#' @param attr attr
#' @param row facet row in grid
#' @param column facet column in grid
#' @param height height of plot
#' @param width width of plot
#' @param theme theme
#' @param title title of plot
#' @param subtitle subtitle of plot
#' @param xlab x-axis
#' @param ylab y-axis label
#' @param caption caption
#' @param show_code show/hide code
#' @param code additional code
#' @import shiny
#' @import ggplot2
#' @importFrom shinyjs hidden removeClass addClass toggle runjs
#' @importFrom shinyWidgets switchInput prettyCheckbox
#' @importFrom stringr str_detect
#' @return No return value. This function is called for the side effect of
#' launching a shiny application.
#' @examples
#' if (interactive()) {
#'   plot_line(mtcars)
#' }
#' @export

plot_line <- function(data, xvar, yvar, theme, group, attr, code, row, column, width, height, title, subtitle, xlab, ylab, caption, show_code){

  if (missing(xvar)) {xvar = ""} else {xvar = deparse(substitute(xvar))}
  if (missing(yvar)) {yvar = ""} else {yvar = deparse(substitute(yvar))}
  if (missing(attr)) {attr = FALSE}
  if (missing(group)) {group = ""} else {group = deparse(substitute(group))}
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





plot_line_ui <- function(id,
                       line_xvar = xvar,
                       line_yvar = yvar,
                       line_group = group,
                       line_attr = attr,
                       line_wraprow = row,
                       line_wrapcol = column,
                       line_height = height,
                       line_width = width,
                       line_theme = theme,
                       line_title = title,
                       line_subtitle = subtitle,
                       line_caption = caption,
                       line_xlab = xlab,
                       line_ylab = ylab,
                       line_code = code,
                       line_showcode = show_code) {
  ns <- NS(id)
  tagList(
    div(id = ns('placeholder1'), class = "parent",
        div(class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
            div(class = "input-view well", style = "padding-right: 0; width: 350px;",
                div(class = "custom-scroll",
                    div(class = "grid-container",
                        div("Line", class = "module-name"),
                        div(class = "cont2", switchInput(NS(id,
                                              "line_instantlocal"),
                                           label = "",
                                           value = TRUE,
                                           size = "mini",
                                           onLabel = "",
                                           offLabel = ""
                               )),
                        div(class = "cont3", actionButton(NS(id, "line_run"), class = "btn-play",
                                                   label = icon(name = "fas fa-play", lib = "font-awesome")))

                           ),

                       selectInput(NS(id, "line_xvar"),
                                   label = "X",
                                   choices = c("", names(data)),
                                   selected = line_xvar),

                       checkboxInput(NS(id, "line_attr"),
                                     label = "Categorical",
                                     value = line_attr),

                       selectInput(NS(id, "line_yvar"),
                                   label = "Y",
                                   choices = c("", names(data)),
                                   selected = line_yvar),

                       selectInput(NS(id, "line_group"),
                                   label = "Group",
                                   choices = c("", names(data)),
                                   selected = line_group),

                       # FACET
                       actionButton(
                         NS(id, "toggle_line_facet"),
                         width = "100%",
                         class = "module-style",
                         label = "Facet",
                         icon = icon("fas fa-caret-down")),


                       hidden(selectInput(
                         NS(id, "line_wraprow"),
                         label = "Row",
                         choices = c("", names(data)),
                         selected = line_wraprow),

                         selectInput(
                           NS(id, "line_wrapcol"),
                           label = "Column",
                           choices = c("", names(data)),
                           selected = line_wrapcol)),

                       # TExT
                       actionButton(NS(id, "toggle_line_text"),
                                    width = "100%",
                                    class = "module-style",
                                    label = "Text",
                                    icon = icon("fas fa-caret-down")
                       ),

                       hidden(
                         textInput(NS(id, "line_title"),
                                   label = "Title",
                                   value = line_title),

                         textInput(NS(id, "line_subtitle"),
                                   label = "Subtitle",
                                   value = line_subtitle),

                         textInput(NS(id, "line_caption"),
                                   label = "Caption",
                                   value = line_caption),

                         textInput(NS(id, "line_xlab"),
                                   label = "X-axis label",
                                   value = line_xlab),

                         textInput(NS(id, "line_ylab"),
                                   label = "Y-axis label",
                                   value = line_ylab)),

                       # SIZE
                       actionButton(
                         NS(id, "toggle_line_size"),
                         width = "100%",
                         class = "module-style",
                         label = "Size",
                         icon = icon("fas fa-caret-down")),



                       hidden(numericInput(
                         NS(id, "line_width"),
                         label = "Width",
                         step = 10,
                         width = "100%",
                         value = line_width),

                         numericInput(
                           NS(id, "line_height"),
                           label = "Height",
                           step = 10,
                           width = "100%",
                           value = line_height)),
                       # THEME
                       actionButton(NS(id, "toggle_line_theme"),
                                    width = "100%",
                                    class = "module-style",
                                    label = "Theme",
                                    icon = icon("fas fa-caret-down")),

                       hidden(
                         selectInput(NS(id, "line_theme"),
                                     label = "Theme",
                                     selected = line_theme,
                                     choices = c("",
                                                 `Black & White` = "theme_bw",
                                                 `Minimal` = "theme_minimal",
                                                 `Grey` = "theme_grey",
                                                 `Line Draw` = "theme_linedraw",
                                                 `Light` = "theme_light",
                                                 `Dark` = "theme_dark",
                                                 `Classic` = "theme_classic",
                                                 `Void` = "theme_void"
                                     ))),


                       actionButton(
                         NS(id, "toggle_line_code"),
                         width = "100%",
                         class = "module-style",
                         label = "Code",
                         icon = icon("fas fa-caret-down")),

                       hidden(
                         textAreaInput(
                           NS(id, "line_code"),
                           value = line_code,
                           label = NULL
                         ),
                         prettyCheckbox(NS(id, "line_showcode"),
                                        label = "show/hide",
                                        status = "info",
                                        value = line_showcode
                         )






                       )

                       )),
          div(class = "result-view",


                    fluidRow(
                      plotOutput(
                        NS(id, "line_plot"), width = "auto", height = "auto")),

                    fluidRow(
                      verbatimTextOutput(
                        NS(id, "line_text")) %>%
                        tagAppendAttributes(class = "codeoutput")
                    )
          )
        )))



}

plot_line_se <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data, {
      updateSelectInput(session, "line_xvar", choices = c("", names(data)))
      updateSelectInput(session, "line_yvar", choices = c("", names(data)))
      updateSelectInput(session, "line_group", choices = c("", names(data)))

      updateSelectInput(
        session,
        "line_wraprow",
        choices = c("", names(data)))

      updateSelectInput(
        session,
        "line_wrapcol",
        choices = c("", names(data)))


    })




    width <- reactive ({
      #   if(input$line_width == "") {

      if (is.na(input$line_width)) {
        return(600)
      } else {
        input$line_width
      }
    })



    height <- reactive ({
      if(is.na(input$line_height)) {
        return(400)
      } else {
        input$line_height
      }
    })






    ns <- NS(id)




    observeEvent(input$toggle_line_size, {
      toggle("line_height")
      toggle("line_width")
      if(input$toggle_line_size %%2 == 1) {
        updateActionButton(session, "toggle_line_size", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_line_size", icon = icon("fas fa-caret-down"))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$toggle_line_code, {
      toggle("line_showcode")
      toggle("line_code")
      if(input$toggle_line_code %%2 == 1) {
        updateActionButton(session, "toggle_line_code", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_line_code", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_line_facet, {
      toggle("line_wraprow")
      toggle("line_wrapcol")
      if(input$toggle_line_facet %%2 == 1) {
        updateActionButton(session, "toggle_line_facet", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_line_facet", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_line_text, {
      toggle("line_title")
      toggle("line_subtitle")
      toggle("line_caption")
      toggle("line_xlab")
      toggle("line_ylab")
      if(input$toggle_line_text %%2 == 1) {
        updateActionButton(session, "toggle_line_text", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_line_text", icon = icon("fas fa-caret-down"))
      }
    })


    observeEvent(input$toggle_line_theme, {
      toggle("line_theme")
      if(input$toggle_line_theme %%2 == 1) {
        updateActionButton(session, "toggle_line_theme", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_line_theme", icon = icon("fas fa-caret-down"))
      }
    })


    code_text <- reactive({

      req(isTruthy(input$line_xvar != "") && isTruthy(input$line_yvar != ""))


      t <- paste0(
        "\n \n ggplot(data, aes(",
        if (as.numeric(input$line_attr) == "1") {
          paste0("factor(", input$line_xvar, ")")
        } else {
          input$line_xvar
        }        , ", ", input$line_yvar,

        if(input$line_group != "") {
          paste0(", group = ", input$line_group)
        },

        ")) + \n    geom_line()"

      )


      t <- paste(
        t,

        if (input$line_wraprow != "" && input$line_wrapcol != "") {
          paste0("+ \n    facet_grid(", input$line_wraprow, " ~ ", input$line_wrapcol, ")")

        } else if (input$line_wrapcol != "") {
          paste0("+ \n    facet_grid(. ~ ", input$line_wrapcol, ")")
        } else if (input$line_wraprow != "") {
          paste0("+ \n    facet_grid(", input$line_wraprow, " ~ . )")
        }
      )



      t <- paste(
        t,
        if (input$line_theme != "") {
          paste0("+ \n    ", input$line_theme, "()")
        }
      )

      t <- paste(
        t,
        if (input$line_title != "") {
          paste0("+ \n    labs(title = '", input$linet_title, "')")
        }
      )

      t <- paste(
        t,
        if (input$line_subtitle != "") {
          paste0("+ \n    labs(subtitle = '", input$line_subtitle, "')")
        }
      )

      t <- paste(
        t,
        if (input$line_caption != "") {
          paste0("+ \n    labs(caption = '", input$line_caption, "')")
        }
      )

      t <- paste(
        t,
        if (input$line_xlab != "") {
          paste0("+ \n    labs(x = '", input$line_xlab, "')")
        }
      )

      t <- paste(
        t,
        if (input$line_ylab != "") {
          paste0("+ \n    labs(y = '", input$line_ylab, "')")
        }
      )


      t <- paste0(
        t,

        paste0(input$line_code)
      )

      t
    })


    observeEvent(input$line_instantlocal, {
      if(input$line_instantlocal == TRUE) {
        removeClass("line_run", "toggle-btnplay")
      } else {
        addClass("line_run", "toggle-btnplay")
      }
    })


    run <- reactive({
      input$line_run
    })

    code_text2 <- reactive({
      if(input$line_instantlocal) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })



    output$line_plot <- renderPlot({
      eval(parse(text = code_text2()))
    },
    width = width,
    height = height,
    )

    mod_id <- paste0(id, "-line_")

    observeEvent(input$line_showcode, {
      if (input$line_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$line_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })

    output$line_text <- renderText({
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
  plot_line_ui("module")
)
server <- function(input, output, session) {
  plot_line_se("module")
}


shinyApp(ui, server)



}

