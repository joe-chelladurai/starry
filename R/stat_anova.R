
Anova <- car::Anova

anovaUI <- function(id,
                     anova_xvar,
                     anova_yvar,
                     anova_code,
                     anova_showcode,
                     anova_instant,
                     anova_run,
                     anova_rmv) {
  ns <- NS(id)
  tagList(
    div(id = ns('placeholder1'), class = "parent",
        div(class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
            div(class = "input-view well", style = "padding-right: 0; width: 350px;",
                div(class = "custom-scroll",
                    div(class = "grid-container",
                        div("One way Anova",  class = "module-name"),
                        div(class = "cont2",
                            switchInput(NS(id, "anova_instant"),
                                        label = "",
                                        value = TRUE,
                                        size = "mini",
                                        onLabel = "",
                                        offLabel = "")),
                        div(class = "cont3", actionButton(NS(id, "anova_run"), class = "btn-play",
                                                          label = icon(name = "fas fa-play", lib = "font-awesome")))

                                   ),
                             

                                   selectInput(NS(id, "anova_yvar"),
                                               label = "Y",
                                               choices = c("", names(data)),
                                               selected = anova_yvar),
                    
                                   selectInput(NS(id, "anova_xvar"),
                                               label = "X",
                                               choices = c("", names(data)),
                                               selected = anova_xvar),
                              
                                   actionButton(NS(id, "toggle_anova_add_code"),
                                                width = "100%",
                                                class = "module-style",
                                                label = "Code",
                                                icon = icon("fas fa-caret-down")),

                                   hidden(
                                     textAreaInput(NS(id, "anova_code"),
                                                   value = anova_code,
                                                   label = NULL),



                                     prettyCheckbox(NS(id, "anova_showcode"),
                                                    label = "show/hide code",
                                                    status = "info",
                                                    value = anova_showcode
                                     )

                                   )
        )),
        div(class = "result-view",

                  fluidRow(actionButton(NS(id, "anova_rmv"),
                                        class = "child",
                                        label = NULL,
                                        icon = icon("fas fa-times"),
                                        value = anova_rmv)),
            
                  fluidRow(tableOutput(NS(id, "anova_glance"))),

                  fluidRow(tableOutput(NS(id, "anova_table"))),

                  fluidRow(verbatimTextOutput(NS(id, "anova_text")) %>% tagAppendAttributes(class = "codeoutput"))
        )
        ))
  )



}

anovaSE <- function(id) {
  moduleServer(id, function(input, output, session) {

 


    ns <- NS(id)

    observeEvent(input$anova_rmv, {
      removeUI(
        selector = paste0('#', ns('placeholder1'))
      )
    })

   
    observeEvent(input$toggle_anova_add_code, {


      toggle("anova_showcode")

      toggle("anova_code")

      if(input$toggle_anova_add_code %%2 == 1) {

        updateActionButton(
          session,
          "toggle_anova_add_code",
          icon = icon("fas fa-caret-up"))

      } else {

        updateActionButton(
          session,
          "toggle_anova_add_code",
          icon = icon("fas fa-caret-down"))
      }
    })


    selected <- reactive({ paste(input$anova_xvar, collapse = ", ") })





    code_text <- reactive({

      req(isTruthy(input$anova_xvar != ""))


      t <- paste0(

        "\n \n data %>% ",

        " \n    lm(",input$anova_yvar, " ~ ", "as.factor(", input$anova_xvar,"), data = . ) %>% ",

        " \n    Anova(type = 3) %>% ",

   #     " \n    summary() %>% ",

        " \n    tidy() %>% ",

        " \n    mutate(p.value = pvalue(.$p.value))"

      )


      t <- paste0(
        t,

        paste0(input$anova_code))

      t




    })


    code_text2 <- reactive({

      t <- paste0(

        "\n \n data %>% ",

        " \n    lm(",input$anova_yvar, " ~ ", "as.factor(", input$anova_xvar,"), data = . ) %>% ",
        
   #     " \n    Anova(type = 3) %>% ",
        
    #    " \n    summary() %>% ",

        " \n    glance()"

      )
    })


    observeEvent(input$anova_instant, {
      if(input$anova_instant == TRUE) {
        removeClass("anova_run", "toggle-btnplay")
      } else {
        addClass("anova_run", "toggle-btnplay")
      }
    })


    run <- reactive({
      input$anova_run
    })

    code_text0 <- reactive({
      if(input$anova_instant) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })


    code_text3 <- reactive({
      if(input$anova_instant) {
        code_text()
      } else {
        req(run())
        isolate(code_text2())
      }
    })





    output$anova_glance  <- renderTable({
      
      req(input$anova_xvar)
      req(input$anova_yvar)

      eval(parse(text = code_text2()))

    }, caption = "Model Fit Statistics", caption.placement = "top")



    output$anova_table  <- renderTable({

      req(input$anova_xvar)
      req(input$anova_yvar)
      
      eval(parse(text = code_text()))

    }, caption = "Anova Table", na = "", caption.placement = "top")


    
    
    
    mod_id <- paste0(id, "-anova_")
    
    observeEvent(input$anova_showcode, {
      if (input$anova_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$anova_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })
    

    output$anova_text <- renderText({

           paste(code_text(), "\n \n", code_text2())

    

    })






  })

}
