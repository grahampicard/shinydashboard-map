library(shiny)
library(plyr)
library(dplyr)
library(tidyr)
library(rCharts)
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
  output$graph_growth_overall <- renderChart2(
    {
      if (is.null(rx_growth_overall())) {
        return(NULL)
      }
      
      # Load RX data
      data <- rx_growth_overall() %>%
        filter(School == input$school) %>% 
        ungroup() %>% 
        select(Subject, growth, total) %>%
        spread(growth, total) %>% 
        select(Subject, Typical = Typ_not, Tiered)
      
      # Graph    
      chart_width <- session$clientData$output_plot_growth_overall_dim_width
      
      go <- rCharts:::Highcharts$new()
      go$chart(type = "column", width = chart_width, backgroundColor = 'rgba(255, 255, 255, 0.1)')
      go$title(text = "Growth Summary")
      go$plotOptions(column = list(stacking = "normal", 
                                   dataLabels=list(enabled=TRUE, 
                                                   formatter = "#! function () {return (Math.round(Math.abs(this.y *100)) >= 10 ?
                                                   Math.round(Math.abs(this.y *100))+ '%': '') }  !#",
                                                   style = list(textShadow = '0 0 0px black',
                                                                fontWeight = 'normal'))))
      go$xAxis(categories = unique(data$Subject))
      go$yAxis(title = list(text = "% Meeting Growth Targets"), 
               max = 1, 
               labels = list(formatter = "#! function() { return this.value * 100 + '%'; } !#"),
               stackLabels = list(enabled = TRUE, 
                                  formatter = "#! function() { return Math.round(this.total * 100) + '%'; } !#",
                                  style = list(textShadow = '0 0 0px black',
                                               fontWeight = 'normal',
                                               color = 'gray')))
      go$data(data)
      go$colors(c("#BCD631","#439539"))
      go$tooltip(formatter = "#! function() {return Math.round(this.y *100) + '%' } !#")
      go$legend(layout = "vertical")
      go$exporting(enabled = T)

      return(go)
    }
  )  

  output$graph_status_overall <- renderChart2(
    {
      if (is.null(rx_status_overall())) {
        return(NULL)
      }
      
      data <- rx_status_overall() %>%
        filter(School == input$school) %>%
        ungroup() %>%  
        select(Subject, Quartile, percent) %>%
        spread(Quartile, percent) %>%
        rename(`3rd Quartile` = `3`, `College Ready` = `4`) %>%
        select(Subject, `College Ready`, `3rd Quartile`)

      top_grade <- rx_status_overall() %>% 
        ungroup() %>%
        filter(School == input$school) %>% 
        select(Grade) %>%
        unique() 
        
      chart_width <- session$clientData$output_plot_growth_overall_dim_width
    
      so <- rCharts:::Highcharts$new()
      
      so$chart(type = "column", width = chart_width, backgroundColor = 'rgba(255, 255, 255, 0.1)')
      so$plotOptions(column = list(stacking = "normal", 
                                   dataLabels=list(enabled=TRUE, 
                                                   formatter = "#! function () {return (Math.round(Math.abs(this.y *100)) >= 10 ?
                                                   Math.round(Math.abs(this.y *100))+ '%': '') }  !#",
                                                   style = list(textShadow = '0 0 0px black',
                                                                fontWeight = 'normal'))))
      so$xAxis(categories = unique(data$Subject))
      so$data(data)
      so$title(text = paste("Grade",top_grade,"Exiting Status"))
      so$colors(c('#255694','#60A2D7'))
      so$legend(layout = "vertical")
      so$yAxis(max = 1, 
               labels = list(formatter = "#! function() { return this.value * 100 + '%'; } !#"),
               stackLabels = list(enabled = TRUE, 
                                  formatter = "#! function() { return Math.round(this.total * 100) + '%'; } !#",
                                  style = list(textShadow = '0 0 0px black',
                                               fontWeight = 'normal',
                                               color = 'gray')))
      so$tooltip(formatter = "#! function() {return Math.round(this.y *100) + '%' } !#")
      so$exporting(enabled = T)
      
      return(so)
    }
  )  
  
  output$graph_growth_targets <- renderChart2(
    {
      if (is.null(rx_growth_targets())) {
        return(NULL)
      }
      
      # Data
      data <- rx_growth_targets() %>%
        filter(School == input$school,
               Growth_Season == input$growth_season) %>%
        ungroup() %>%
        select(Subject, Grade, total, growth) %>%
        spread(growth, total) %>% 
        unite(New_Subject, Subject, Grade, sep = "<br>") %>%
        select(New_Subject, Typical = Typ_not, Tiered)
      
      chart_width <- session$clientData$output_plot_growth_targets_dim_width 
      
      # Chart      
      gg <- rCharts:::Highcharts$new()
      gg$chart(type = "column", width = chart_width, backgroundColor = 'rgba(255, 255, 255, 0.1)')
      gg$data(data)
      gg$title(text = "Growth by Grade")      
      gg$plotOptions(column = list(stacking = "normal", 
                                   dataLabels=list(enabled=TRUE, 
                                   formatter = "#! function () {return (Math.round(Math.abs(this.y *100)) >= 10 ?
                                                   Math.round(Math.abs(this.y *100))+ '%': '') }  !#",
                                   style = list(textShadow = '0 0 0px black',
                                                fontWeight = 'normal'))))
      gg$xAxis(categories = unique(data$New_Subject))
      gg$yAxis(title = list(text = "% Meeting Growth Target"), max = 1, 
               labels = list(formatter = "#! function() { return this.value * 100 + '%'; } !#"),
               stackLabels = list(enabled = TRUE, 
                                  formatter = "#! function() { return Math.round(this.total * 100) + '%'; } !#",
                                  style = list(textShadow = '0 0 0px black',
                                               fontWeight = 'normal',
                                               color = 'gray')))
      gg$colors(c("#BCD631","#439539"))
      gg$legend(layout = "vertical")
      gg$tooltip(formatter = "#! function() {return Math.round(this.y *100) + '%' } !#")
      gg$exporting(enabled = T)
      
      # Output
      return(gg) 
    }
  )  
  
  output$graph_growth_by_q <- renderChart2(
    {
      if (is.null(rx_growth_targets_by_q())) {
        return(NULL)
      }
      
      # Data
      data <- rx_growth_targets_by_q() %>%
        filter(School == input$school,
               Subject == input$subject, 
               Growth_Season == input$growth_season) %>%
        mutate(Grade = paste("Grade", Grade)) %>%
        select(Grade, Start_Q, value) %>%
        spread(Start_Q, value) %>%
        rename(`Bottom` = `1`, `2nd` = `2`, `3rd` = `3`, `Top` = `4`)

      if (length(data$Grade) == 1) {
        names  <- list(data$Grade)
      } else {
        names <- data$Grade  
      }
      
      chart_width <- session$clientData$output_plot_growth_by_q_dim_width
      
      # Create chart
      gsq <- rCharts:::Highcharts$new()
      gsq$data(data)
      gsq$plotOptions(column = list(
        dataLabels=list(enabled=TRUE, 
                        formatter = "#! function () {return (Math.round(Math.abs(this.y *100)) >= 10 ?
                                        Math.round(Math.abs(this.y *100))+ '%': '') }  !#",
                        style = list(textShadow = '0 0 0px black',
                        fontWeight = 'normal'))))
      gsq$chart(type = "column", width = chart_width, backgroundColor = 'rgba(255, 255, 255, 0.1)')
      gsq$title(text = "Typical Growth by Starting Quartile")
      gsq$xAxis(categories = names)
      gsq$yAxis(title = list(text = "% Meeting Typical Growth Target"), max = 1, labels = list(formatter = "#! function() { return this.value * 100 + '%'; } !#"))
      gsq$tooltip(formatter = "#! function() {return Math.round(this.y *100) + '%' } !#")
      gsq$colors(c('#8D8685','#CFCCC1','#60A2D7','#255694'))
      gsq$exporting(enabled = T)
      # Plot
      return(gsq)
      
    }
  )  
  
  output$graph_growth_quartile <- renderChart2(
    {
      if (is.null(rx_growth_quartile())) {
        return(NULL)
      }
      
      # Data
      data <- rx_growth_quartile() %>%
        filter(School == input$school, 
               Subject == input$subject, 
               Growth_Season == input$growth_season) %>%
        arrange(Grade, display) %>%
        select(display, Bottom = `1`, `2nd` = `2`, Top = `4`, `3rd` = `3`)

      # Formatting changes      
      chart_width <- chart_width <- session$clientData$output_plot_graph_growth_quartile_dim_width
      order <- list(Bottom = 4, `2nd` = 3, `3rd` = 2, Top = 1)
      
      # Plot
      qs <- rCharts:::Highcharts$new()
      qs$chart(type = "column", width = chart_width, backgroundColor = 'rgba(255, 255, 255, 0.1)', height = 450)
      qs$data(data)
      qs$title(text = "Quartile Shifts")
      qs$plotOptions(column = list(stacking = "normal", 
                                  dataLabels=list(enabled=TRUE, 
                                                  formatter = "#! function () {return (Math.round(Math.abs(this.y *100)) >= 10 ?
                                                  Math.round(Math.abs(this.y *100))+ '%': '') }  !#",
                                                  style = list(textShadow = '0 0 0px black',
                                                               fontWeight = 'normal')))) 
      qs$xAxis(categories = (unique(data$display)))
      qs$yAxis(title = list(text = "Size of Quartile"), max = 1, labels = list(formatter = "#! function() { return Math.abs(this.value) * 100 + '%'; } !#"))
      qs$tooltip(formatter = "#! function() {return Math.round(Math.abs(this.y *100)) + '%' } !#")
      qs$colors(c('#8D8685','#CFCCC1','#255694','#60A2D7'))
      qs$legend(layout = "vertical")
      qs$exporting(enabled = TRUE)
      qs$params$series <- lapply(qs$params$series, function(d){
        temp = order[d$name]
        names(temp) = NULL
        d$legendIndex = temp
        return(d)
      })
      
      return(qs)      
    }
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
