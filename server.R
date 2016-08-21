library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googleVis)
library(shinyBS)

# File-size limit to 9 MB
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input, output, session) {
  source("www/data-processing.R")
  
  #### MELT ####    
  data_upload <- reactive({
    infile <- input$cdf_file
    if (is.null(infile)) {
      error_message <- "Upload your ComboStudentAssessment.csv or download the example file to use this tool."
      createAlert(session, "ui_alert_graph", "server_alert_graph", title = "No Data :(",
                  content = error_message)
      createAlert(session, "ui_alert_table", "server_alert_table", title = "No Data :(",
                  content = error_message)
      createAlert(session, "ui_alert_table2", "server_alert_table2", title = "No Data :(",
                  content = error_message)
      createAlert(session, "ui_alert_table3", "server_alert_table3", title = "No Data :(",
                  content = error_message)
      return(NULL)
    }
    closeAlert(session, "server_alert_graph")
    closeAlert(session, "server_alert_table")    
    closeAlert(session, "server_alert_table2")
    closeAlert(session, "server_alert_table3")        
    read.csv(infile$datapath) %>% melt_cdf()
  })
  
  #### GENERATE ####  
  growth_all <- reactive({
    if (is.null(data_upload())) {
      return(NULL)
    }
    data_upload() %>% gen_growth()
  })

  growth <- reactive({
    if (is.null(growth_all())) {
      return(NULL)
    }
    growth_all() %>% filter(School == input$school)
  })
  
  status <- reactive({
    if (is.null(data_upload())) {
      return(NULL)
    }
    data_upload() %>% gen_status()
  })
  
  #### ROLLUP (RX) ####
  ## GROWTH
  rx_growth_overall <- reactive({
    if (is.null(growth_all())) {
      return(NULL)
    }
    growth() %>% rollup_growth_overall()
  })
  
  rx_growth_targets <- reactive({
    if (is.null(growth())) {
      return(NULL)
    }
    growth() %>% rollup_growth_targets()
  })
  
  rx_growth_targets_by_q <- reactive({
    if (is.null(growth())) {
      return(NULL)
    }
    growth() %>% rollup_growth_targets_by_q()
  })
  
  rx_growth_quartile <- reactive({
    if (is.null(growth())) {
      return(NULL)
    }
    growth() %>% rollup_growth_quartile()
  })
  
  rx_growth_quartile_sankey <- reactive({
    if (is.null(growth())) {
      return(NULL)
    }
    growth() %>% rollup_growth_quartile_sankey()
  })
  
  rx_growth_quartile_sankey_filtered <- reactive({
    if (is.null(rx_growth_quartile_sankey())) {
      return(NULL)
    }
    rx_growth_quartile_sankey() %>% 
      filter(School == input$school,
             Subject == input$subject,
             Growth_Season == input$growth_season)
  })
  
  
  ##   STATUS
  rx_status_overall <- reactive({
    if (is.null(growth())) {
      return(NULL)
    }
    status() %>% rollup_status_overall()
  })  
  
  
  ## Tables
  rx_table_tidy <- reactive({
    if(is.null(status())) {
      return(NULL)
    }
    status() %>% export_growth()
  })
  
  rx_table_molten <- reactive({
    if(is.null(status())) {
      return(NULL)
    }
    status() %>% export_molten()
  })
  
  #### INPUTS ####    
  observe({
    updateSelectInput(session, "school", 
                      choices = sort(unique(growth_all()$School),decreasing = FALSE))
  })
  
  observe({
    updateSelectInput(session, "subject",
                      choices = unique(growth()$Subject))
  })

  observe({
    updateSelectInput(session, "growth_season",
                      choices = sort(unique(growth()$Growth_Season),decreasing = TRUE))
  })

  observe({
    updateSelectInput(session, "season",
                      choices = sort(unique(status()$Season), decreasing = TRUE))
  })

  observe({
    updateSelectInput(session, "grade",
                      choices = sort(unique(rx_growth_quartile_sankey_filtered()$Grade), decreasing = FALSE))
  })
  
  #### PLOTS ####    
  output$graph_status_overall <- renderPlot(
    {
      if (is.null(rx_status_overall())) {
        return(NULL)
      }
      
      data <- rx_status_overall() %>%
        filter(School == input$school)
      
      ggplot(data = data, aes(x = Subject, y = percent, fill = factor(Quartile))) + 
        geom_bar(stat = "identity") +
        geom_text(data = data, aes(x = Subject, y = label_loc, 
                                   fill = factor(Quartile), label = label),
                  size = 6, fontface = "bold", color = "#F1F5FB") +
        scale_fill_manual(values = c("4" = "#255694","3" = "#60A2D7"), 
                          labels = c("College\nReady","3rd Quartile"),
                          breaks = c("4","3")) +
        scale_y_continuous(breaks = round(seq(0,1,.25),2),
                           labels = c("0%","25%","50%","75%","100%"),
                           limits = c(0,1)) +      
        xlab(unique(paste0("\n Grade ",as.character(data$Grade)))) +
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(color="#555555", size = 12),
              legend.background = element_blank(),
              legend.key = element_blank(),
              axis.text.y=element_blank(),
              axis.text.x=element_text(size = 12),
              axis.ticks=element_blank(),
              axis.title.y=element_blank(),
              axis.title.x=element_text(size = 16),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()
        )
    }, 
    bg = "transparent"
  )  
  
  output$graph_growth_overall <- renderPlot(
    {
      if (is.null(rx_growth_overall())) {
        return(NULL)
      }
      
      # Load RX data
      data <- rx_growth_overall() %>%
        filter(School == input$school)
      
      # Graph    
      ggplot(data = data, aes(x = factor(Subject), y = total, 
                              fill = factor(growth))) + 
        geom_bar(stat = 'identity') +
        geom_text(data = data, aes(x=factor(Subject), y = label_loc, 
                                   label = total_label), size = 6, 
                  fontface="bold", color="#ebf1f9") +
        
        # Scales      
        scale_fill_manual(values = c("Typ_not" = "#BCD07A","Tiered" = "#3C9250"),
                          labels = c("Typical", "Tiered"),
                          breaks = c("Typ_not", "Tiered")) +
        scale_y_continuous(breaks = round(seq(0,1,.25),2),
                           labels = c("0%","25%","50%","75%","100%"),
                           limits = c(0,1)) +
        
        # Labels
        ylab("% Meeting Target") +
        xlab("\n All Grades") +      
        
        # Formatting
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(color="#555555", size = 12),
              legend.background = element_blank(),
              legend.key = element_blank(),
              axis.text.y=element_blank(),
              axis.text.x=element_text(size = 12),
              axis.ticks=element_blank(),
              axis.title.y=element_blank(),
              axis.title.x=element_text(size = 16),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()
        )
    }, 
    bg = "transparent"
  )  
  
  output$graph_growth_targets <- renderPlot(
    {
      if (is.null(rx_growth_targets())) {
        return(NULL)
      }
      
      # Data
      data <- rx_growth_targets() %>%
        filter(School == input$school,
               Growth_Season == input$growth_season)
      
      # Plot
      ggplot(data = data, aes(x = factor(Grade), y = total, fill = factor(growth))) + 
        geom_bar(stat = 'identity') +
        geom_text(data = data, aes(x=factor(Grade), y = label_loc, label=total_label), 
                  size = 5.2, fontface="bold", color="#ebf1f9") +
        facet_grid(.~Subject) +
        
        # Scales
        scale_fill_manual(values = c("Typ_not" = "#BCD07A","Tiered" = "#3C9250"),
                          labels = c("Typical", "Tiered"),
                          breaks = c("Typ_not", "Tiered")) +
        scale_y_continuous(breaks = round(seq(0,1,.25),2),
                           labels = c("0%","25%","50%","75%","100%"),
                           limits = c(0,1)) +
        
        # Labels
        ylab("% of Matched Students") +
        xlab("\n Grade") +
        
        # Format
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(color="#555555", size = 12),
              legend.background = element_blank(),
              legend.key = element_blank(),
              axis.title.x = element_text(size=16),
              axis.title.y = element_text(size=12),
              axis.text=element_text(size=12),
              axis.ticks.x = element_blank(),              
              panel.grid.major.y = element_line(color = "#b3b3b3"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              strip.text.x = element_text(size=14),
              strip.background = element_rect(fill = "#f2f2f2")
        )
    }, 
    bg = "transparent"
  )  
  
  output$graph_growth_targets_by_q <- renderPlot(
    {
      if (is.null(rx_growth_targets_by_q())) {
        return(NULL)
      }
      
      # Data
      data <- rx_growth_targets_by_q() %>%
        filter(School == input$school,
               Subject == input$subject, 
               Growth_Season == input$growth_season)      
      
      # Plot
      ggplot() +
        geom_bar(data = data, aes(x = factor(Start_Q), y = value, 
                                  fill = factor(Start_Q)), stat = "identity") +
        facet_grid(.~Grade) +
        geom_text(data = data, aes(x = factor(Start_Q), y = label_loc, label=label), 
                  size = 4.5, fontface="bold") +
        scale_fill_manual(values = c("1" = "#8D8685", "2" = "#CFCCC1",
                                     "3" = "#60A2D7", "4" = "#255694"), 
                          labels = c("Bottom Quartile","2nd Quartile",
                                     "3rd Quartile","College Ready"),
                          breaks = c("1","2","3","4")) +
        
        # adding label
        ylab("% of Matched Students") +

        ylim(0,1.05) +
        scale_y_continuous(breaks = round(seq(0,1,.25),2),
                           labels = c("0%","25%","50%","75%","100%")) +
        
        # Formatting
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(color="#555555", size = 12),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.position = "bottom",
              legend.direction = "horizontal",
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=12),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              panel.grid.major.y = element_line(color = "#b3b3b3"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              strip.text.x = element_text(size=14),
              strip.background = element_rect(fill = "#f2f2f2")
        )
    }, 
    bg = "transparent"
  )  
  
  output$graph_growth_quartile <- renderPlot(
    {
      if (is.null(rx_growth_quartile())) {
        return(NULL)
      }
      
      # Data
      data <- rx_growth_quartile() %>%
        filter(School == input$school, 
               Subject == input$subject, 
               Growth_Season == input$growth_season) %>%
        arrange(display, Quartile)
      
      # subset low and high quartiles for graphs
      low  <- data %>% 
        filter(Quartile <= 0) %>%
        mutate(Quartile = factor(Quartile)) %>%
        group_by(display) %>%
        mutate(label_loc = cumsum(total) - (.5 * total))
      
      high <- data %>% 
        filter(Quartile >= 0) %>%
        mutate(Quartile = factor(Quartile)) %>%
        group_by(display) %>%
        mutate(label_loc = cumsum(total) - (.5 * total))      
      
      # Create graphs
      ggplot() +
        
        # Titles
        ylab(label = "% of Matched Students") +
        xlab("\n Grade") +
        
        # Graph Body
        geom_bar(data=low, aes(x=display, y=total, fill=Quartile), stat="identity") + 
        geom_bar(data=high, aes(x=display, y=total, fill=Quartile), stat="identity") + 
        geom_text(data=low, aes(x=display, y=label_loc, fill=Quartile, label=total_label),
                  size = 6, fontface="bold", color="#F1F5FB") +
        geom_text(data=high, aes(x=display, y=label_loc, fill=Quartile, label=total_label),
                  size = 6, fontface = "bold", color = "#F1F5FB") +
        
        # Legend
        scale_fill_manual(values = c("4" = "#255694","3" = "#60A2D7",
                                     "-2" = "#CFCCC1", "-1" = "#8D8685"), 
                          labels = c("College Ready","3rd Quartile",
                                     "2nd Quartile","Bottom Quartile"),
                          breaks = c("4","3","-2","-1")) +
        scale_y_continuous(breaks = round(seq(-1,1,.25),2),
                           labels = c("100%","75%","50%","25%","0%","25%",
                                      "50%","75%","100%")) +
        
        # Formatting
        theme(panel.background = element_blank(),
              plot.background = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(color="#555555", size = 12),
              legend.background = element_blank(),
              legend.key = element_blank(),
              axis.title.x = element_text(size=16),
              axis.title.y = element_text(size=12),
              axis.ticks.x = element_blank(),              
              panel.grid.major.y = element_line(color = "#b3b3b3"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank()
        )
    }, 
    bg = "transparent"
  )    
  
  output$graph_growth_quartile_sankey <- renderGvis(
    {
      if (is.null(rx_growth_quartile_sankey_filtered())) {
        return(NULL)
      }
      
      data <- rx_growth_quartile_sankey_filtered() %>%
        filter(Grade == input$grade) %>%
        arrange(School, desc(Start_Q), desc(End_Q), Growth_Season, Grade, Subject) %>%        
        select(Start_Q, End_Q, Total) 
      
      gvisSankey(data, from = "Start_Q", to = "End_Q", weight = "Total",
                 options = list(height=400, width=600,
                                sankey="{
                                link: {
                                colorMode: 'gradient',
                                colors: ['#255694','#60A2D7','#CFCCC1','#8D8685']
                                }, 
                                node: {
                                label: {
                                fontSize: 24,
                                color: '#000',
                                bold: true,
                                italic: false
                                },
                                interactivity: true, // Allows you to select nodes.
                                labelPadding: 6,     // Horizontal distance between the label and the node.
                                nodePadding: 10,     // Vertical distance between nodes.
                                width: 20,            // Thickness of the node.
                                colors: ['#255694','#60A2D7','#CFCCC1','#8D8685']
                                }
    }"
                )
                 )
}
                 )
  
  #### TABLES & EXPORTS ####
  output$display_table <- renderDataTable({
    multiplier_table()
  }, options = list('paging' = FALSE,'searching' = FALSE,'info' = FALSE,'wrap' = TRUE, 'sort' = FALSE))
  
  output$lookup_tidy <- renderDataTable({
    rx_table_tidy()
  })
  
  output$dl_lookup_tidy <- downloadHandler(
    filename = "student_summary.csv",
    content = function(file) {
      write.csv(rx_table_tidy(), file, row.names = FALSE)
    }
  )

  output$lookup_molten <- renderDataTable({
    rx_table_molten()
  })
  
  output$dl_lookup_molten <- downloadHandler(
    filename = "student_summary.csv",
    content = function(file) {
      write.csv(rx_table_molten(), file, row.names = FALSE)
    }
  )
  
  output$lookup_growth <- renderDataTable({
    growth()
  })  
  
  output$dl_lookup_growth <- downloadHandler(
    filename = "student_summary.csv",
    content = function(file) {
      write.csv(growth(), file, row.names = FALSE)
    }
  )  
    
  #### JS: SHOW/HIDE INPUTS ####
  observe({
    if(input$analysis_tab %in% c("Growth Summary","Growth by Starting Quartile",
                         "Performance Summary","Performance by Grade")){
      show(id = "js_input_growth_season", anim = TRUE, time = .3, animType = "fade")
    }
    else {
      hide(id = "js_input_growth_season", anim = FALSE)
    }
  })
  
  observe({
    if(input$analysis_tab %in% c("Growth by Starting Quartile","Performance Summary",
                                 "Performance by Grade")){
      show(id = "js_input_subject", anim = TRUE, time = .3, animType = "fade")
    }
    else {
      hide(id = "js_input_subject", anim = FALSE)
    }
  })
  
  observe({
    if(input$analysis_tab %in% c("Performance by Grade")){
      show(id = "js_input_grade", anim = TRUE, time = .3, animType = "fade")
    }
    else {
      hide(id = "js_input_grade", anim = FALSE)
    }
  })
  
  #### JS: SHOW/HIDE PAGES ####
})
