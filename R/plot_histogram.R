
plot_histogram <- function(data, xvar, fill, outline, row, column, width, height, title, subtitle, xlab, ylab, caption, theme, code, show_code){

  if (missing(xvar)) {xvar = ""} else {xvar = deparse(substitute(xvar))}
  if (missing(fill)) {fill = ""} else {fill = deparse(substitute(fill))}
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
  if (missing(show_code)) {show_code = FALSE}




plot_histogram_ui <- function(id,
                            histogram_xvar = xvar,
                            histogram_fill = fill,
                            histogram_outline = outline,
                            histogram_wraprow = row,
                            histogram_wrapcol = column,
                            histogram_height = height,
                            histogram_width = width,
                            histogram_theme = theme,
                            histogram_title = title,
                            histogram_subtitle = subtitle,
                            histogram_caption = caption,
                            histogram_xlab = xlab,
                            histogram_ylab = ylab,
                            histogram_code = code,
                            histogram_showcode = show_code) {
  ns <- NS(id)
  tagList(
    div(id = ns('placeholder1'), class = "parent",
        div(class ="inputresultview", style = "display:flex; margin-top: 10px; margin-bottom: 10px;",
            div(class = "input-view well", style = "padding-right: 0; width: 350px;",
                div(class = "custom-scroll",
                    div(class = "grid-container",
                        div("Histogram", class = "module-name"),
                        div(class = "cont2",
                            switchInput(NS(id, "histogram_instantlocal"),
                                        label = "",
                                        value = TRUE,
                                        size = "mini",
                                        onLabel = "",
                                        offLabel = ""
                                        )),
                        div(class = "cont3",
                            actionButton(NS(id, "histogram_run"), class = "btn-play",
                                         label = icon(name = "fas fa-play", lib = "font-awesome")))

                           ),

                           # XVAR
                           selectInput(
                             NS(id, "histogram_xvar"),
                             label = "X",
                             choices = c("", names(data)),
                             selected = histogram_xvar),



                           # FILL
                           selectInput(
                             NS(id, "histogram_fill"),
                             label = "Fill",
                             choices = c(" ", names(data)),
                             selected = histogram_fill),
                           # OUTLINE
                           selectInput(
                             NS(id, "histogram_outline"),
                             label = "Outline",
                             choices = c(" ", names(data)),
                             selected = histogram_outline),

                           # FACET
                           actionButton(
                             NS(id, "toggle_histogram_facet"),
                             width = "100%",
                             class = "module-style",
                             label = "Facet",
                             icon = icon("fas fa-caret-down")),


                           hidden(selectInput(
                             NS(id, "histogram_wraprow"),
                             label = "Row",
                             choices = c(" ", names(data)),
                             selected = histogram_wraprow),

                             selectInput(
                               NS(id, "histogram_wrapcol"),
                               label = "Column",
                               choices = c(" ", names(data)),
                               selected = histogram_wrapcol)),

                           # TExT
                           actionButton(NS(id, "toggle_histogram_text"),
                                        width = "100%",
                                        class = "module-style",
                                        label = "Text",
                                        icon = icon("fas fa-caret-down")
                           ),

                           hidden(
                             textInput(NS(id, "histogram_title"),
                                       label = "Title",
                                       value = histogram_title),

                             textInput(NS(id, "histogram_subtitle"),
                                       label = "Subtitle",
                                       value = histogram_subtitle),

                             textInput(NS(id, "histogram_caption"),
                                       label = "Caption",
                                       value = histogram_caption),

                             textInput(NS(id, "histogram_xlab"),
                                       label = "X-axis label",
                                       value = histogram_xlab),

                             textInput(NS(id, "histogram_ylab"),
                                       label = "Y-axis label",
                                       value = histogram_ylab)),

                           # SIZE
                           actionButton(
                             NS(id, "toggle_histogram_size"),
                             width = "100%",
                             class = "module-style",
                             label = "Size",
                             icon = icon("fas fa-caret-down")),



                           hidden(numericInput(
                             NS(id, "histogram_width"),
                             label = "Width",
                             step = 10,
                             width = "100%",
                             value = histogram_width),

                             numericInput(
                               NS(id, "histogram_height"),
                               label = "Height",
                               step = 10,
                               width = "100%",
                               value = histogram_height)),
                           # THEME
                           actionButton(NS(id, "toggle_histogram_theme"),
                                        width = "100%",
                                        class = "module-style",
                                        label = "Theme",
                                        icon = icon("fas fa-caret-down")),

                           hidden(
                             selectInput(NS(id, "histogram_theme"),
                                         label = "Theme",
                                         selected = histogram_theme,
                                         choices = c(" ",
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
                             NS(id, "toggle_histogram_code"),
                             width = "100%",
                             class = "module-style",
                             label = "Code",
                             icon = icon("fas fa-caret-down")),

                           hidden(
                             textAreaInput(
                               NS(id, "histogram_code"),
                               value = histogram_code,
                               label = NULL
                             ),
                             prettyCheckbox(NS(id, "histogram_showcode"),
                                            label = "show/hide",
                                            status = "info",
                                            value = histogram_showcode
                             )






                           )

                       )),
          div(class = "result-view",


                    fluidRow(
                      plotOutput(
                        NS(id, "histogram_plot"), width = "auto", height = "auto")),
                    fluidRow(
                      verbatimTextOutput(
                        NS(id, "histogram_text")) %>%
                        tagAppendAttributes(class = "codeoutput"))

          )
        )))



}


plot_histogram_se <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data, {

      updateSelectInput(
        session,
        "histogram_xvar",
        choices = c("", names(data)))



      updateSelectInput(
        session,
        "histogram_outline",
        choices = c(" ", names(data))
      )

      updateSelectInput(
        session,
        "histogram_fill",
        choices = c(" ", names(data))
      )

      updateSelectInput(
        session,
        "histogram_wraprow",
        choices = c(" ", names(data))
      )

      updateSelectInput(
        session,
        "histogram_wrapcol",
        choices = c(" ", names(data))
      )


    })



    ns <- NS(id)



    observeEvent(input$toggle_histogram_size, {
      toggle("histogram_height")
      toggle("histogram_width")
      if(input$toggle_histogram_size %%2 == 1) {
        updateActionButton(session, "toggle_histogram_size", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_histogram_size", icon = icon("fas fa-caret-down"))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$toggle_histogram_code, {
      toggle("histogram_showcode")
      toggle("histogram_code")
      if(input$toggle_histogram_code %%2 == 1) {
        updateActionButton(session, "toggle_histogram_code", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_histogram_code", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_histogram_facet, {
      toggle("histogram_wraprow")
      toggle("histogram_wrapcol")
      if(input$toggle_histogram_facet %%2 == 1) {
        updateActionButton(session, "toggle_histogram_facet", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_histogram_facet", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_histogram_theme, {
      toggle("histogram_theme")
      if(input$toggle_histogram_facet %%2 == 1) {
        updateActionButton(session, "toggle_histogram_theme", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_histogram_theme", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_histogram_text, {
      toggle("histogram_title")
      toggle("histogram_subtitle")
      toggle("histogram_caption")
      toggle("histogram_xlab")
      toggle("histogram_ylab")
      if(input$toggle_histogram_text %%2 == 1) {
        updateActionButton(session, "toggle_histogram_text", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_histogram_text", icon = icon("fas fa-caret-down"))
      }
    })


    histogram_fill_reactive <- reactive({
      input$histogram_fill
    })

    histogram_xvar_reactive <- reactive({
      input$histogram_xvar
    })


    histogram_title_reactive <- reactive({
      input$histogram_title
    })



      code_text <- reactive({

      req(isTruthy(input$histogram_xvar != ""))

      t <- paste0(
        "\n \n ggplot(data, aes(x = ", input$histogram_xvar,

        if(input$histogram_fill != " ") {
          paste0(", fill = factor(", input$histogram_fill, ")")
        } else {

        },

        if(input$histogram_outline != " ") {
          paste0(", color = factor(", input$histogram_outline, ")")
        } else {

        },

        ")) + \n    geom_histogram()"


      )


      t <- paste(
        t,

        if (input$histogram_wraprow != " " && input$histogram_wrapcol != " ") {
          paste0("+ \n    facet_grid(", input$histogram_wraprow, " ~ ", input$histogram_wrapcol, ")")

        } else if (input$histogram_wrapcol != " ") {
          paste0("+ \n    facet_grid(. ~ ", input$histogram_wrapcol, ")")
        } else if (input$histogram_wraprow != " ") {
          paste0("+ \n    facet_grid(", input$histogram_wraprow, " ~ . )")
        }
      )


      t <- paste(
        t,
        if (input$histogram_theme != " ") {
          paste0("+ \n    ", input$histogram_theme, "()")
        }
      )

      t <- paste(
        t,
        if (histogram_title_reactive() != "") {
          paste0("+ \n    labs(title = '", histogram_title_reactive(), "')")
        }
      )

      t <- paste(
        t,
        if (input$histogram_subtitle != "") {
          paste0("+ \n    labs(subtitle = '", input$histogram_subtitle, "')")
        }
      )

      t <- paste(
        t,
        if (input$histogram_caption != "") {
          paste0("+ \n    labs(caption = '", input$histogram_caption, "')")
        }
      )

      t <- paste(
        t,
        if (input$histogram_xlab != "") {
          paste0("+ \n    labs(x = '", input$histogram_xlab, "')")
        }
      )

      t <- paste(
        t,
        if (input$histogram_ylab != "") {
          paste0("+ \n    labs(y = '", input$histogram_ylab, "')")
        }
      )


      t <- paste0(
        t,
        #      if(input$histogram_code != ""){
        paste0(input$histogram_code)
        #      }
      )

      t
    })


    width <- reactive ({

      if (is.na(input$histogram_width)) {
        return(600)
      } else {
        input$histogram_width
      }
    })



    height <- reactive ({

      if(is.na(input$histogram_height)) {
        return(400)
      } else {
        input$histogram_height
      }
    })



    observeEvent(input$histogram_instantlocal, {
      if(input$histogram_instantlocal == TRUE) {
        removeClass("histogram_run", "toggle-btnplay")
      } else {
        addClass("histogram_run", "toggle-btnplay")
      }
    })


    run <- reactive({
      input$histogram_run
    })

    code_text2 <- reactive({
      if(input$histogram_instantlocal) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })






    output$histogram_plot <- renderPlot({


        eval(parse(text = code_text2()))

    }, width = width,
    height = height)

    mod_id <- paste0(id, "-histogram_")

    observeEvent(input$histogram_showcode, {
      if (input$histogram_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$histogram_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })


    output$histogram_text <- renderText({

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
  plot_histogram_ui("module")
)
server <- function(input, output, session) {
  plot_histogram_se("module")
}


shinyApp(ui, server)



}


plot_histogram(mtcars)


