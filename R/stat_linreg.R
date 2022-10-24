

linregUI <- function(id,
                   linreg_xvar,
                   linreg_yvar,
                   linreg_vartext,
                   linreg_code,
                   linreg_showcode,
                   linreg_instant,
                   linreg_run,
                   linreg_rmv) {
  ns <- NS(id)
  tagList(
    div(id = ns('placeholder1'), class = "parent",
        div(class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
            div(class = "input-view well", style = "padding-right: 0; width: 350px;",
                div(class = "custom-scroll",
                    div(class = "grid-container",
                        div("Linear Regression", class = "module-name"),
                        div(class = "cont2",
                            switchInput(NS(id, "linreg_instant"),
                                        label = "",
                                        value = TRUE,
                                        size = "mini",
                                        onLabel = "",
                                        offLabel = ""
                                        )),
                        div(class = "cont3", actionButton(NS(id, "linreg_run"), class = "btn-play",
                                                           label = icon(name = "fas fa-play", lib = "font-awesome")))

                                   ),


                                   #hidden(
                                   hidden(textInput(NS(id, "linreg_vartext"),
                                                    label = "Variables Text",
                                                    value = linreg_vartext),
                                          actionButton(NS(id, "updatebtn"), "Update")),
                                   #),

                                   selectInput(NS(id, "linreg_yvar"),
                                               label = "Y",
                                               choices = c("", names(data)),
                                               selected = linreg_yvar),
                                   selectizeInput(NS(id, "linreg_xvar"),
                                                  label = "Variables",
                                                  choices = c("", names(data)),
                                                  options = list(
                                                    delimiter = ',',
                                                    create = TRUE
                                                  ),
                                                  #    selected = unlist(strsplit(linreg_vartext, "\\,")),
                                                  selected = linreg_vartext,
                                                  multiple = TRUE),

                                   actionButton(NS(id, "toggle_linreg_add_code"),
                                                width = "100%",
                                                class = "module-style",
                                                label = "Code",
                                                icon = icon("fas fa-caret-down")),

                                   hidden(
                                     textAreaInput(NS(id, "linreg_code"),
                                                   value = linreg_code,
                                                   label = NULL),



                                     prettyCheckbox(NS(id, "linreg_showcode"),
                                                    label = "show/hide code",
                                                    status = "info",
                                                    value = linreg_showcode
                                     )

                                   )
                )
        ),
        div(class = "result-view",

                  fluidRow(actionButton(NS(id, "linreg_rmv"),
                                        class = "child",
                                        label = NULL,
                                        icon = icon("fas fa-times"),
                                        value = linreg_rmv)),
                  fluidRow(hidden(textOutput(NS(id, "something")))),

                  fluidRow(tableOutput(NS(id, "linreg_glance"))),

                  fluidRow(tableOutput(NS(id, "linreg_table"))),

                  fluidRow(verbatimTextOutput(NS(id, "linreg_text")) %>% tagAppendAttributes(class = "codeoutput"))
        )
        )))



}

linregSE <- function(id) {
  moduleServer(id, function(input, output, session) {

  #  observeEvent(data, {
  #    updateSelectInput(session, "linreg_yvar", choices = c("", names(data)))
  #  })

    observeEvent(data, {
      #  req(input$linreg_vartext)
      updateSelectizeInput(session, "linreg_xvar", choices = c("", names(data))) #, selected = unlist(strsplit(input$linreg_vartext, "\\,"))))
      delay(1, click("updatebtn"))

    })

    #  observeEvent(input$linreg_vartext, {
    #   isolate(delay(1000, updateSelectizeInput(session, "linreg_xvar", choices = c("", names(data)), selected = unlist(strsplit(input$linreg_vartext, "\\,")))))
    #  })
    #  observeEvent(textvar,{
    #    if (!is.na(textvar) && textvar != "") {
    #   isolate(delay(1,
    #         updateTextInput(session, "linreg_vartext", value = textvar)
    #         ))
    #
    #   }
    #  })

    #  observeEvent(input$linreg_vartext,{
    #    delay(1, updateSelectizeInput(session, "linreg_xvar", selected = unlist(strsplit(input$linreg_vartext, "\\,"))))
    #  })

    #  textvar2 <- reactive({
    #    req(textvar)
    #    unlist(strsplit(textvar, "\\,"))
    #  })
    #  observeEvent(textvar,{
    #    #    if (!is.na(textvar) && textvar != "") {
    #
    #
    #    isolate(delay(1,
    #          updateSelectizeInput(session, "linreg_xvar", selected = textvar2())
    #    ))
    #
    #    #     }
    #  })




    observeEvent(input$updatebtn, {
      #      updateTextInput(session, "linreg_vartext", value = "x2, x3")
      updateSelectizeInput(session, "linreg_xvar", selected = unlist(strsplit(input$linreg_vartext, "\\,")))#selected = c("x4", "x3", "x2", "x1"))
    })

    ns <- NS(id)

    observeEvent(input$linreg_rmv, {
      removeUI(
        selector = paste0('#', ns('placeholder1'))
      )
    })

    output$something <- renderText({
      paste(input$linreg_vartext)
    })

    #  output$something <- renderText({
    #   req(textvar)
    #    req(textvar2())
    #    paste("this ", textvar, paste("that ", textvar2()))
    #  })

    observeEvent(input$toggle_linreg_add_code, {


      toggle("linreg_showcode")

      toggle("linreg_code")

      if(input$toggle_linreg_add_code %%2 == 1) {

        updateActionButton(
          session,
          "toggle_linreg_add_code",
          icon = icon("fas fa-caret-up"))

      } else {

        updateActionButton(
          session,
          "toggle_linreg_add_code",
          icon = icon("fas fa-caret-down"))
      }
    })


    selected <- reactive({ paste(input$linreg_xvar, collapse = ", ") })


    observeEvent(input$linreg_xvar,{
      #     if(is.na(input$linreg_vartext))
      req(input$linreg_xvar)
      updateTextInput(session, "linreg_vartext", value = input$linreg_xvar)
    })

    code_text <- reactive({

      req(isTruthy(input$linreg_xvar != ""))

      t <- paste0(

        "\n \n data %>% ",

        " \n    lm(data = .,", input$linreg_yvar, " ~ ", paste(input$linreg_xvar, collapse = ' + '), paste(") %>% "),

     #   " \n    summary() %>% ",

        " \n    tidy() %>% ",

        " \n    mutate(p.value = pvalue(.$p.value))"

      )


      t <- paste0(
        t,

        paste0(input$linreg_code))

      t




    })


    code_text2 <- reactive({

      req(isTruthy(input$linreg_xvar != ""))

      t <- paste0(

        "\n \n data %>% ",

        " \n    lm(data = .,", input$linreg_yvar, " ~ ", paste(input$linreg_xvar, collapse = ' + '), paste(") %>% "),

     #   " \n    summary() %>% ",

        " \n    glance()"

      )
    })

    observeEvent(input$linreg_instant, {
      if(input$linreg_instant == TRUE) {
        removeClass("linreg_run", "toggle-btnplay")
      } else {
        addClass("linreg_run", "toggle-btnplay")
      }
    })


    run <- reactive({
      input$linreg_run
    })

    code_text0 <- reactive({
      if(input$linreg_instant) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })


    code_text3 <- reactive({
      if(input$linreg_instant) {
        code_text2()
      } else {
        req(run())
        isolate(code_text2())
      }
    })




    output$linreg_glance  <- renderTable({


      eval(parse(text = code_text3()))

    }, caption = "Model Fit Statistics", caption.placement = "top")



    output$linreg_table  <- renderTable({


      eval(parse(text = code_text0()))

    }, align = c("lrrrr"),caption = "Regression Table", na = "", caption.placement = "top")



    
    
    mod_id <- paste0(id, "-linreg_")
    
    observeEvent(input$linreg_showcode, {
      if (input$linreg_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible","font-size":"12px"})'))
      }
      if (input$linreg_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden","font-size":"0px"})'))
      }
    })
    
    
    output$linreg_text <- renderText({

  
        paste(code_text0(), "\n \n ", code_text3())

  

    })






  })

}
