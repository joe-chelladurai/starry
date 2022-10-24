
plot_density_UI <- function(id,
                            density_xvar,
                            density_fill,
                            density_outline,
                            density_wraprow,
                            density_wrapcol,
                            density_height,
                            density_width,
                            density_theme,
                            density_title,
                            density_subtitle,
                            density_caption,
                            density_xlab,
                            density_ylab,
                            density_code,
                            density_showcode,
                            density_instantlocal,
                            density_run,
                            density_rmv) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("placeholder1"), class = "parent",
      div(
        class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
        div(
          class = "input-view well", style = "padding-right: 0; width: 350px;",
          div(
            class = "custom-scroll",
            div(
              class = "grid-container",
              div("Density", class = "module-name"),
              div(
                class = "cont2",
                switchInput(NS(id, "density_instantlocal"),
                  label = "",
                  value = TRUE,
                  size = "mini",
                  onLabel = "",
                  offLabel = ""
                )
              ),
              div(
                class = "cont3",
                actionButton(NS(id, "density_run"),
                  class = "btn-play",
                  label = icon(name = "fas fa-play", lib = "font-awesome")
                )
              )
            ),
            # XVAR
            selectInput(
              NS(id, "density_xvar"),
              label = "X",
              choices = c("", names(data)),
              selected = density_xvar
            ),



            # FILL
            selectInput(
              NS(id, "density_fill"),
              label = "Fill",
              choices = c("", names(data)),
              selected = density_fill
            ),
            # OUTLINE
            selectInput(
              NS(id, "density_outline"),
              label = "Outline",
              choices = c("", names(data)),
              selected = density_outline
            ),

            # FACET
            actionButton(
              NS(id, "toggle_density_facet"),
              width = "100%",
              class = "module-style",
              label = "Facet",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(
                NS(id, "density_wraprow"),
                label = "Row",
                choices = c("", names(data)),
                selected = density_wraprow
              ),
              selectInput(
                NS(id, "density_wrapcol"),
                label = "Column",
                choices = c("", names(data)),
                selected = density_wrapcol
              )
            ),

            # TExT
            actionButton(NS(id, "toggle_density_text"),
              width = "100%",
              class = "module-style",
              label = "Text",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textInput(NS(id, "density_title"),
                label = "Title",
                value = density_title
              ),
              textInput(NS(id, "density_subtitle"),
                label = "Subtitle",
                value = density_subtitle
              ),
              textInput(NS(id, "density_caption"),
                label = "Caption",
                value = density_caption
              ),
              textInput(NS(id, "density_xlab"),
                label = "X-axis label",
                value = density_xlab
              ),
              textInput(NS(id, "density_ylab"),
                label = "Y-axis label",
                value = density_ylab
              )
            ),

            # SIZE
            actionButton(
              NS(id, "toggle_density_size"),
              width = "100%",
              class = "module-style",
              label = "Size",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              numericInput(
                NS(id, "density_width"),
                label = "Width",
                step = 10,
                width = "100%",
                value = density_width
              ),
              numericInput(
                NS(id, "density_height"),
                label = "Height",
                step = 10,
                width = "100%",
                value = density_height
              )
            ),
            # THEME
            actionButton(NS(id, "toggle_density_theme"),
              width = "100%",
              class = "module-style",
              label = "Theme",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              selectInput(NS(id, "density_theme"),
                label = "Theme",
                selected = density_theme,
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
              NS(id, "toggle_density_code"),
              width = "100%",
              class = "module-style",
              label = "Code",
              icon = icon("fas fa-caret-down")
            ),
            hidden(
              textAreaInput(
                NS(id, "density_code"),
                value = density_code,
                label = NULL
              ),
              prettyCheckbox(NS(id, "density_showcode"),
                label = "show/hide",
                status = "info",
                value = density_showcode
              )
            )
          )
        ),
        div(
          class = "result-view",
          fluidRow(
            actionButton(
              NS(id, "density_rmv"),
              label = NULL,
              icon = icon("fas fa-times"),
              class = "child",
              value = density_rmv
            )
          ),
          fluidRow(
            plotOutput(
              NS(id, "density_plot"),
              width = "auto", height = "auto"
            )
          ),
          fluidRow(
            verbatimTextOutput(
              NS(id, "density_text")
            )
          ) %>% tagAppendAttributes(class = "codeoutput")
        )
      )
    )
  )
}

plot_density_SE <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data, {
      updateSelectInput(
        session,
        "density_xvar",
        choices = c("", names(data))
      )



      updateSelectInput(
        session,
        "density_outline",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "density_fill",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "density_wraprow",
        choices = c("", names(data))
      )

      updateSelectInput(
        session,
        "density_wrapcol",
        choices = c("", names(data))
      )
    })



    ns <- NS(id)
    observeEvent(input$density_rmv, {
      removeUI(
        selector = paste0("#", ns("placeholder1"))
      )
    })

    observeEvent(input$toggle_density_size,
      {
        toggle("density_height")
        toggle("density_width")
        if (input$toggle_density_size %% 2 == 1) {
          updateActionButton(session, "toggle_density_size", icon = icon("fas fa-caret-up"))
        } else {
          updateActionButton(session, "toggle_density_size", icon = icon("fas fa-caret-down"))
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$toggle_density_code, {
      toggle("density_showcode")
      toggle("density_code")
      if (input$toggle_density_code %% 2 == 1) {
        updateActionButton(session, "toggle_density_code", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_density_code", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_density_facet, {
      toggle("density_wraprow")
      toggle("density_wrapcol")
      if (input$toggle_density_facet %% 2 == 1) {
        updateActionButton(session, "toggle_density_facet", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_density_facet", icon = icon("fas fa-caret-down"))
      }
    })

    observeEvent(input$toggle_density_text, {
      toggle("density_title")
      toggle("density_subtitle")
      toggle("density_caption")
      toggle("density_xlab")
      toggle("density_ylab")
      if (input$toggle_density_text %% 2 == 1) {
        updateActionButton(session, "toggle_density_text", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_density_text", icon = icon("fas fa-caret-down"))
      }
    })


    observeEvent(input$toggle_density_theme, {
      toggle("density_theme")
      if (input$toggle_density_theme %% 2 == 1) {
        updateActionButton(session, "toggle_density_theme", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_density_theme", icon = icon("fas fa-caret-down"))
      }
    })

    code_text <- reactive({
      req(isTruthy(input$density_xvar != ""))

      t <- paste0(
        "\n \n ggplot(data, aes(x = ", input$density_xvar,
        if (input$density_fill != "") {
          paste0(", fill = factor(", input$density_fill, ")")
        } else {

        },
        if (input$density_outline != "") {
          paste0(", color = factor(", input$density_outline, ")")
        } else {

        },
        if (str_detect(input$density_code, "geom_density")) {
          "))"
        } else {
          ")) + \n    geom_density()"
        }
      )


      t <- paste(
        t,
        if (input$density_wraprow != "" && input$density_wrapcol != "") {
          paste0("+ \n    facet_grid(", input$density_wraprow, " ~ ", input$density_wrapcol, ")")
        } else if (input$density_wrapcol != "") {
          paste0("+ \n    facet_grid(. ~ ", input$density_wrapcol, ")")
        } else if (input$density_wraprow != "") {
          paste0("+ \n    facet_grid(", input$density_wraprow, " ~ . )")
        }
      )
      
      
      t <- paste(
        t,
        if (input$density_theme != "") {
          paste0("+ \n    ", input$density_theme, "()")
        }
      )

      t <- paste(
        t,
        if (input$density_title != "") {
          paste0("+ \n    labs(title = '", input$densityt_title, "')")
        }
      )

      t <- paste(
        t,
        if (input$density_subtitle != "") {
          paste0("+ \n    labs(subtitle = '", input$density_subtitle, "')")
        }
      )

      t <- paste(
        t,
        if (input$density_caption != "") {
          paste0("+ \n    labs(caption = '", input$density_caption, "')")
        }
      )

      t <- paste(
        t,
        if (input$density_xlab != "") {
          paste0("+ \n    labs(x = '", input$density_xlab, "')")
        }
      )

      t <- paste(
        t,
        if (input$density_ylab != "") {
          paste0("+ \n    labs(y = '", input$density_ylab, "')")
        }
      )


      t <- paste0(
        t,
        #      if(input$density_code != ""){
        paste0(input$density_code)
        #      }
      )

      t
    })


    width <- reactive({
      if (is.na(input$density_width)) {
        return(600)
      } else {
        input$density_width
      }
    })



    height <- reactive({
      if (is.na(input$density_height)) {
        return(400)
      } else {
        input$density_height
      }
    })

    observeEvent(input$density_instantlocal, {
      if (input$density_instantlocal == TRUE) {
        removeClass("density_run", "toggle-btnplay")
      } else {
        addClass("density_run", "toggle-btnplay")
      }
    })


    run <- reactive({
      input$density_run
    })

    code_text2 <- reactive({
      if (input$density_instantlocal) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })



    output$density_plot <- renderPlot(
      {
        eval(parse(text = code_text2()))
      },
      width = width,
      height = height
    )

    mod_id <- paste0(id, "-density_")

    observeEvent(input$density_showcode, {
      if (input$density_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$density_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })


    output$density_text <- renderText({
      code_text2()
    })
  })
}
