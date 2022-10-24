


codeUI <- function(id, 
                   code_rmv,
                   code_text,
                   code_instant,
                   code_run) {
  ns <- NS(id)
  
  tagList(
    
    div(id = ns('placeholder1'), class = "parent",
        div(class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
            div(class = "input-view well", style = "padding-right: 0; width: 350px;"
           
            ),
            div(class = "result-view", 
                
                fluidRow(actionButton(NS(id, "code_rmv"),
                                      class = "child", 
                                      label = NULL,
                                      icon = icon("fas fa-times"),
                                      value = code_rmv)),
                
                div(class = "grid-container", 
                    div("", class = "module-name", style = "width: 80%"),
                    div(class = "cont2", 
                        switchInput(NS(id, "code_instant"),
                                    label = "",
                                    value = TRUE,
                                    size = "mini",
                                    onLabel = "",
                                    offLabel = "")),
                    div(class = "cont3", actionButton(NS(id, "code_run"), class = "btn-play",
                                                      label = icon(name = "fas fa-play", lib = "font-awesome")))),
                shinyAce::aceEditor(
                  ns("code_text"),
                  mode = "r",
                  theme = "chrome",
                  value = code_text,
                  wordWrap = TRUE,
                  debounce = 1,
                  showLineNumbers = FALSE,
                  highlightActiveLine = FALSE,
                  autoScrollEditorIntoView = TRUE,
                  autoComplete = "live",
                  minLines = 3,
                  maxLines = 6
                  
                ) ,
                tagList(htmlOutput(ns("code_output"))),
                hidden(verbatimTextOutput(ns("code_text")) %>% 
                         tagAppendAttributes(class = 'codeoutput'))
            #    textAreaInput(NS(id, "code_text"), value = code_text, label = NULL, width = '100%')
                
            
                
                
             #   fluidRow(verbatimTextOutput(NS(id, "code_output")))
                
            )
        ))
  )
  
}

codeSE <- function(id, env) {
  
  moduleServer(id, function(input, output, session) {
    
    run <- reactive({
      input$code_run
    }) 
    
    
  
    

    
    ns <- NS(id)
    
    observeEvent(input$code_rmv, {
      
      removeUI(
        selector = paste0('#', ns('placeholder1'))
      )
      
    })
    
    
    
    
    observeEvent(input$code_instant, {
      if(input$code_instant == TRUE) {
        removeClass("code_run", "toggle-btnplay")
      } else {
        addClass("code_run", "toggle-btnplay")
      }
    })
    
    
    
    code_text <- reactive({
      

        paste0(input$code_text)
      
      
    })
    
   
    code_text2 <- reactive({
   
      if(input$code_instant) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })
    
    
     
    output$code_output <- renderUI({
 
      eval_code <- paste0("\n```{r echo = FALSE, comment = NA}\n", code_text2(), "\n```\n")
      HTML(knitr::knit2html(text = eval_code, fragment.only = TRUE, quiet = TRUE, envir = env))
    })
    

    
    output$code_text <- renderText({
      paste("\n \n", code_text2())
    })
    
    
    
    
    
    
  })
  
}