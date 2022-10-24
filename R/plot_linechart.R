
plot_linechart_UI <- function(id, 
                       linechart_xvar,
                       linechart_yvar,
                       linechart_group,
                       linechart_attr,
                       linechart_wraprow,
                       linechart_wrapcol,
                       linechart_height,
                       linechart_width, 
                       linechart_theme,
                       linechart_title,
                       linechart_subtitle,
                       linechart_caption,
                       linechart_xlab,
                       linechart_ylab,
                       linechart_code,
                       linechart_showcode,
                       linechart_instantlocal,
                       linechart_run,
                       linechart_rmv) {
  ns <- NS(id)
  tagList(
    div(id = ns('placeholder1'), class = "parent",
        div(class = "inputresultview", style = "display: flex; margin-top: 10px; margin-bottom: 10px;",
            div(class = "input-view well", style = "padding-right: 0; width: 350px;",
                div(class = "custom-scroll",
                    div(class = "grid-container",
                        div("Line", class = "module-name"),
                        div(class = "cont2", switchInput(NS(id, 
                                              "linechart_instantlocal"),
                                           label = "",
                                           value = TRUE,
                                           size = "mini",
                                           onLabel = "",
                                           offLabel = ""
                               )),
                        div(class = "cont3", actionButton(NS(id, "linechart_run"), class = "btn-play",
                                                   label = icon(name = "fas fa-play", lib = "font-awesome")))
                               
                           ),
                       
                       selectInput(NS(id, "linechart_xvar"), 
                                   label = "X", 
                                   choices = c("", names(data)), 
                                   selected = linechart_xvar),
                       
                       checkboxInput(NS(id, "linechart_attr"),
                                     label = "Categorical", 
                                     value = linechart_attr),
                       
                       selectInput(NS(id, "linechart_yvar"), 
                                   label = "Y", 
                                   choices = c("", names(data)), 
                                   selected = linechart_yvar),
                       
                       selectInput(NS(id, "linechart_group"), 
                                   label = "Group", 
                                   choices = c("", names(data)), 
                                   selected = linechart_group),
                       
                       # FACET
                       actionButton(
                         NS(id, "toggle_linechart_facet"), 
                         width = "100%", 
                         class = "module-style",
                         label = "Facet", 
                         icon = icon("fas fa-caret-down")),
                       
                       
                       hidden(selectInput(
                         NS(id, "linechart_wraprow"),
                         label = "Row",
                         choices = c("", names(data)),
                         selected = linechart_wraprow),
                         
                         selectInput(
                           NS(id, "linechart_wrapcol"),
                           label = "Column",
                           choices = c("", names(data)),
                           selected = linechart_wrapcol)),
                       
                       # TExT
                       actionButton(NS(id, "toggle_linechart_text"),
                                    width = "100%",
                                    class = "module-style",
                                    label = "Text",
                                    icon = icon("fas fa-caret-down")
                       ),
                       
                       hidden(
                         textInput(NS(id, "linechart_title"),
                                   label = "Title", 
                                   value = linechart_title),
                         
                         textInput(NS(id, "linechart_subtitle"),
                                   label = "Subtitle",
                                   value = linechart_subtitle),
                         
                         textInput(NS(id, "linechart_caption"),
                                   label = "Caption",
                                   value = linechart_caption),
                         
                         textInput(NS(id, "linechart_xlab"),
                                   label = "X-axis label",
                                   value = linechart_xlab),
                         
                         textInput(NS(id, "linechart_ylab"),
                                   label = "Y-axis label",
                                   value = linechart_ylab)),
                       
                       # SIZE               
                       actionButton(
                         NS(id, "toggle_linechart_size"), 
                         width = "100%", 
                         class = "module-style",
                         label = "Size", 
                         icon = icon("fas fa-caret-down")),
                       
                       
                       
                       hidden(numericInput(
                         NS(id, "linechart_width"), 
                         label = "Width", 
                         step = 10, 
                         width = "100%", 
                         value = linechart_width),
                         
                         numericInput(
                           NS(id, "linechart_height"), 
                           label = "Height", 
                           step = 10, 
                           width = "100%", 
                           value = linechart_height)),
                       # THEME                 
                       actionButton(NS(id, "toggle_linechart_theme"),
                                    width = "100%", 
                                    class = "module-style",
                                    label = "Theme", 
                                    icon = icon("fas fa-caret-down")),      
                       
                       hidden(
                         selectInput(NS(id, "linechart_theme"), 
                                     label = "Theme", 
                                     selected = linechart_theme, 
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
                         NS(id, "toggle_linechart_code"), 
                         width = "100%", 
                         class = "module-style",
                         label = "Code", 
                         icon = icon("fas fa-caret-down")),
                       
                       hidden(
                         textAreaInput(
                           NS(id, "linechart_code"),
                           value = linechart_code,
                           label = NULL
                         ),
                         prettyCheckbox(NS(id, "linechart_showcode"),
                                        label = "show/hide",
                                        status = "info",
                                        value = linechart_showcode
                         )
                         
                         
                         
                         
                         
                         
                       )
                       
                       )),
          div(class = "result-view",
                    fluidRow(
                      actionButton(
                        NS(id, "linechart_rmv"),
                        label = NULL,
                        icon = icon("fas fa-times"),
                        class = "child", 
                        value = linechart_rmv)),
                    
                    fluidRow(
                      plotOutput(
                        NS(id, "linechart_plot"), width = "auto", height = "auto")),
                    
                    fluidRow( 
                      verbatimTextOutput(
                        NS(id, "linechart_text")) %>% 
                        tagAppendAttributes(class = "codeoutput")
                    )
          )
        )))
  
  
  
}

plot_linechart_SE <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data, {
      updateSelectInput(session, "linechart_xvar", choices = c("", names(data)))
      updateSelectInput(session, "linechart_yvar", choices = c("", names(data)))
      updateSelectInput(session, "linechart_group", choices = c("", names(data)))
      
      updateSelectInput(
        session,
        "linechart_wraprow",
        choices = c("", names(data)))
      
      updateSelectInput(
        session,
        "linechart_wrapcol",
        choices = c("", names(data)))
     
      
    })
    
    
    
    
    width <- reactive ({  
      #   if(input$linechart_width == "") {
      
      if (is.na(input$linechart_width)) { 
        return(600)
      } else {
        input$linechart_width
      }
    })
    
    
    
    height <- reactive ({  
      #  if(input$linechart_height == "") {
      if(is.na(input$linechart_height)) {
        return(400)
      } else {
        input$linechart_height
      }
    })
    
    
    
    
    

    ns <- NS(id)
    observeEvent(input$linechart_rmv, {
      
      removeUI(
        selector = paste0('#', ns('placeholder1'))
      )
    })
 
 
    
    observeEvent(input$toggle_linechart_size, {
      toggle("linechart_height")
      toggle("linechart_width")
      if(input$toggle_linechart_size %%2 == 1) {
        updateActionButton(session, "toggle_linechart_size", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_linechart_size", icon = icon("fas fa-caret-down"))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$toggle_linechart_code, {
      toggle("linechart_showcode")
      toggle("linechart_code")
      if(input$toggle_linechart_code %%2 == 1) {
        updateActionButton(session, "toggle_linechart_code", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_linechart_code", icon = icon("fas fa-caret-down"))
      }
    }) 
    
    observeEvent(input$toggle_linechart_facet, {
      toggle("linechart_wraprow")
      toggle("linechart_wrapcol")
      if(input$toggle_linechart_facet %%2 == 1) {
        updateActionButton(session, "toggle_linechart_facet", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_linechart_facet", icon = icon("fas fa-caret-down"))
      }
    }) 
    
    observeEvent(input$toggle_linechart_text, {
      toggle("linechart_title")
      toggle("linechart_subtitle")
      toggle("linechart_caption")
      toggle("linechart_xlab")
      toggle("linechart_ylab")
      if(input$toggle_linechart_text %%2 == 1) {
        updateActionButton(session, "toggle_linechart_text", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_linechart_text", icon = icon("fas fa-caret-down"))
      }
    }) 
    
    
    observeEvent(input$toggle_linechart_theme, {
      toggle("linechart_theme")
      if(input$toggle_linechart_theme %%2 == 1) {
        updateActionButton(session, "toggle_linechart_theme", icon = icon("fas fa-caret-up"))
      } else {
        updateActionButton(session, "toggle_linechart_theme", icon = icon("fas fa-caret-down"))
      }
    }) 
    

    code_text <- reactive({
      
      req(isTruthy(input$linechart_xvar != "") && isTruthy(input$linechart_yvar != ""))
      
      
      t <- paste0(
        "\n \n ggplot(data, aes(", 
        if (as.numeric(input$linechart_attr) == "1") {
          paste0("factor(", input$linechart_xvar, ")")
        } else {
          input$linechart_xvar
        }        , ", ", input$linechart_yvar, 
        
        if(input$linechart_group != "") {
          paste0(", group = ", input$linechart_group)
        },
        
        ")) + \n    geom_line()"
       
      )
      
      
      t <- paste(
        t, 
        
        if (input$linechart_wraprow != "" && input$linechart_wrapcol != "") {
          paste0("+ \n    facet_grid(", input$linechart_wraprow, " ~ ", input$linechart_wrapcol, ")")
          
        } else if (input$linechart_wrapcol != "") {
          paste0("+ \n    facet_grid(. ~ ", input$linechart_wrapcol, ")")
        } else if (input$linechart_wraprow != "") {
          paste0("+ \n    facet_grid(", input$linechart_wraprow, " ~ . )")
        }
      )
      
     
      
      t <- paste(
        t,
        if (input$linechart_theme != "") {
          paste0("+ \n    ", input$linechart_theme, "()")
        }
      )
      
      t <- paste(
        t,
        if (input$linechart_title != "") {
          paste0("+ \n    labs(title = '", input$linechartt_title, "')")
        }
      )
      
      t <- paste(
        t,
        if (input$linechart_subtitle != "") {
          paste0("+ \n    labs(subtitle = '", input$linechart_subtitle, "')")
        }
      )
      
      t <- paste(
        t,
        if (input$linechart_caption != "") {
          paste0("+ \n    labs(caption = '", input$linechart_caption, "')")
        }
      )
      
      t <- paste(
        t,
        if (input$linechart_xlab != "") {
          paste0("+ \n    labs(x = '", input$linechart_xlab, "')")
        }
      )
      
      t <- paste(
        t,
        if (input$linechart_ylab != "") {
          paste0("+ \n    labs(y = '", input$linechart_ylab, "')")
        }
      )
      
      
      t <- paste0(
        t,
     
        paste0(input$linechart_code)
      )
      
      t  
    })
    
    
    observeEvent(input$linechart_instantlocal, {
      if(input$linechart_instantlocal == TRUE) {
        removeClass("linechart_run", "toggle-btnplay")
      } else {
        addClass("linechart_run", "toggle-btnplay")
      }
    })
    
    
    run <- reactive({
      input$linechart_run
    }) 
    
    code_text2 <- reactive({
      if(input$linechart_instantlocal) {
        code_text()
      } else {
        req(run())
        isolate(code_text())
      }
    })
    
    
    
    output$linechart_plot <- renderPlot({
      eval(parse(text = code_text2()))
    },
    width = width,
    height = height,
    )
    
    mod_id <- paste0(id, "-linechart_")
    
    observeEvent(input$linechart_showcode, {
      if (input$linechart_showcode == "TRUE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"visible"})'))
      }
      if (input$linechart_showcode == "FALSE") {
        runjs(paste0('$("#', mod_id, 'text").css({"visibility":"hidden"})'))
      }
    })
    
    output$linechart_text <- renderText({
        code_text2()
    })
    
  })
  
}

