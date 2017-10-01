library(shiny)
library(rCharts)
library(shinydashboard)
library(shinyjs)
library(shinyBS)

sidebar  <- dashboardSidebar(
              hr(),
              sidebarMenu(id="tabs",
                menuItem("Getting Started", tabName = "start", icon = icon("database"), selected=TRUE),
                menuItem("Analyze Your Results", icon = icon("dashboard"),
                  menuSubItem("School-Level", tabName = "analysis_school", icon = icon("child"))),
                menuItem("Exports", icon=icon("download"),
                  menuSubItem("Student Summary", tabName = "table_summary", icon = icon("file-excel-o")),
                  menuSubItem("Single-Test Data", tabName = "table_molten", icon = icon("file-excel-o")),
                  menuSubItem("Growth Data", tabName = "table_growth", icon = icon("file-excel-o"))                  
                )
              )
            )

body     <- dashboardBody(useShinyjs(),
              tabItems(
                tabItem(tabName = "start",
                  fluidPage(
                    # tags$img( src = "logo-black.png", width = '150px'),
                    br(),
                    h2("Welcome to the NWEA MAP Analysis One-Stop!"),
                    p("This is a proof-of-concept, one-stop shop where you explore the NWEA MAP  
                      results for each school in your district. Dig into key growth and 
                      achievement metrics to assess the academic health 
                      of each school in your region, download graph image files to 
                      share your on your own, and get database-friendly exports with
                      2015 Growth and Percentile norms."),
                    p("This uses R Shiny, RCharts, Highcharts, and GoogleVis. See all
                      code at my ", tags$a(href = "https://github.com/grahampicard/shinydashboard-map","github page.")),
                    br(),
                    
                    ## Boxes 
                    box(title = "Prepare your Data", width = NULL, collapsible = TRUE,
                      p("Upload your Spring 'ComboStudentAssessment.csv' file found in the NWEA MAP online tool. To test out the tool, download an",
                        tags$a(href = "fake-ComboStudentAssessment.csv", "example NWEA export"),
                        "with fake data."),                 
                      fileInput("cdf_file",label = "",accept = c("text/csv",".csv")))
                    ,

                    box(title = "What is MAP?", width = NULL, collapsible = TRUE, collapsed = TRUE,
                      p("Measures of Academic Progress (MAP) is a personalized, adaptive
                        assessment created by NWEA. According to NWEA's", 
                      tags$a(href = "https://www.nwea.org/about/", "website:")),
                      p(tags$em("More than 7,400 partners in U.S. 
                        schools, districts, education agencies, and international schools 
                        trust us to offer pre-kindergarten through grade 12 assessments that 
                        accurately measure student growth and learning needs, professional 
                        development that fosters educators' ability to accelerate student 
                        learning, and research that supports assessment validity and data 
                        interpretation. To better inform instruction and maximize every 
                        learner's academic growth, educators currently use NWEA assessments 
                        and items with"), tags$b("nearly 10 million students.")),
                      p("Schools administer MAP assessments in any of the following subjects: Reading, 
                        Mathematics, Science, and Language. Students are commonly assessed
                        between 2-3 times during an academic year, though some schools test students
                        up to 4 times. Becuase MAP is computer-based and adaptive, no two MAP 
                        assessments are ever repeated."),
                      p("MAP asessments are scored on a RIT scale. RIT is ungraded, meaning
                        RIT scores can be compared across grades. For each grade, subject, and
                        testing season, NWEA releases norms that connect a student's 
                        RIT score to his/her national percentile ranking (NPR). Additionally, 
                        NWEA provides RIT growth goals (referred to as typical goal/target/growth) for 
                        each student. Typical growth is the amount of RIT growth needed for 
                        a student to maintain his/her percentile ranking across seasons.")
                    ),
                    
                    box(title = "What does the tool do?", width = NULL, collapsible = TRUE, collapsed = TRUE,
                        p("NWEA's CSV exports and PDF reports are designed for academic
                        directors and teachers to analyze student-level results to improve 
                        individual, targeted instruction. While these resources provide rich 
                        information, there currently are few reports that summarize overall 
                        academic health of a school, and none of these are interactive."),
                        p("This tool provides", tags$b("a high-level overview of school performance"), "and
                        focuses on key progress monitoring metrics that answer two important questions",
                          tags$ol(
                            tags$li("Are students leaving your school on track to", tags$b("college-readiness?")),
                            tags$li("Are students making", tags$b("enough growth"), "over the course of the 
                                  year to reach college-ready levels?")
                          )),
                        p("Additionally, you can download the results with 2015 standard norms that
                        allow you to compare results across schools (NWEA's CDFs use custom 
                        norms based on instructional weeks by default)."),
                        p("All visualizations are interactive. Select/Deselect any item from a legend to 
                          focus on the measures that are important.")
                    ),
                    
                    box(title = "Is it safe to upload my school/district's data?", width = NULL, collapsible = TRUE, collapsed = TRUE, 
                        p("Yep! Your data will never be stored on our servers. Once you close 
                          out this window, the tool automatically resets, and your data is gone. 
                          If you want to access these reports or exports again, just come back and 
                          upload your CDF file.")
                    ),
                    
                    box(title = "Where do I get my school/district's CDF file?", width = NULL, collapsible = TRUE, collapsed = TRUE, 
                        p("On the ", a(href = "https://sso.mapnwea.org/auth/login","NWEA admin site"),
                          "first navigate to 'MAP Reports' > 'Data Export Scheduler'. Select 'Combined Data
                        file', 'By District', and '2015 Norms'. Click save and wait for your NWEA to generate
                        your CDF report (this may take some time). Please reach out to the MAP NWEA testing
                        coordinator for your region in order to gain access.")
                    ),
                    
                    box(title = "Questions or Comments?", width = NULL, collapsible = TRUE, collapsed = TRUE, 
                        p("Reach out to ", tags$a(href = "mailto:gpicard@kipp.org", "Graham Picard"), "for
                        more information or more resources about R programming.")
                    )
                  )
                ),
                tabItem(tabName = "analysis_school",
                        fluidPage(
                          bsAlert("ui_alert_graph"),        
                          fluidRow(
                            column(3,
                                   div(id = "js_input_school", selectInput("school","School",""))
                            ),
                            column(3,
                                   div(id = "js_input_growth_season", selectInput("growth_season","Growth Season",""))
                            ),
                            column(3,
                                   hidden(div(id = "js_input_subject", selectInput("subject","Subject","")))
                            ),
                            column(3,
                                   hidden(div(id = "js_input_grade", selectInput("grade","Grade","")))
                            )
                          ),
                          
                          tabsetPanel(id = "analysis_tab",
                                      
                                      #### ANALYSIS > OVERALL ####
                                      tabPanel("Overall",
                                               fluidPage(
                                                 div(id = "js_tab_overall",                   
                                                     br(),
                                                     column(4,
                                                            h4("Explore Your Results"),
                                                            p("At schools that prepare students for college success, over half of 
                                                               all students are meeting their growth targets and over half of students
                                                               in the oldest grade are reaching the top quartile by the time they graduate 
                                                               in both subjects."),
                                                            p("When assessing the academic health of a school, it's crucial to look
                                                              at both of these measures to see if all students are on
                                                              track to college success.")
                                                     ),
                                                     column(4,
                                                            plotOutput("plot_growth_overall_dim", height = "1px"),
                                                            showOutput("graph_growth_overall", "highcharts"),
                                                            br(),
                                                            p("K, 1, 2, 5: Fall-to-Spring Growth Targets."),
                                                            p("3, 4, 6, 7, 8: Spring-to-Spring Growth Targets.")
                                                     ),
                                                     column(4, 
                                                            showOutput("graph_status_overall", "highcharts"),
                                                            br(),
                                                            p("Spring results for oldest grade in school. Includes all tested 
                                                              students.")
                          )                          
                        )
                      )
                    ),
                    
                    #### ANALYSIS > GROWTH TARGETS ####
                    tabPanel("Growth Summary",
                             fluidPage(
                               div(id = "js_tab_growth_targets",
                                   br(),
                                   column(4,
                                          h4("Are all grades meeting their annual growth targets?"),     
                                          p("For over half of students to reach the top quartile by the 
                                            time they graduate, students who start below grade level need 
                                            to make more than typical growth."),
                                          p(tags$b("Tiered Targets"), "are more aggressive goals that are scaled 
                                            by a student's starting national rank. Tiered targets are calculated
                                            by taking the RIT growth target and applying a multiplier based on 
                                            a student's quartile ranking. See the table below for multiplier amounts."),
                                          br(),
                                          div(align = 'center',
                                              h4("Multipliers by Grade"),
                                              dataTableOutput("display_table")
                                          ),
                                          p(tags$b("Spring-to-Spring"), "accounts for summer loss, 
                            providing a more accurate depiction of a student growth 
                              over the course of the year. Use Spring-to-spring to 
                              analyze growth in 3rd, 4th, 6th, 7th, and 8th grade.")                            
                                   ),
                                   
                                   mainPanel(
                                     plotOutput("plot_growth_targets_dim", height = "1px"),                                     
                                     chartOutput("graph_growth_targets", "highcharts"),                  
                                     p(tags$i("Matched students only. N < 15 suppressed"))                  
                                   )
                               )
                             )
                    ),
                    
                    #### ANALYSIS > GROWTH TARGETS BY Q  ####
                    tabPanel("Growth by Starting Quartile",
                             fluidPage(
                               div(id = "js_tab_growth_targets_by_q",            
                                   br(),
                                   column(4,
                                          h4("Which students are most likely to meet their growth target?"),
                                          p("At healthy schools, students who start the year at a low national
                              rank should be as likely to make typical growth as students who
                              start the year at a high rank."),
                                          p("To see if all students are making growth, we can disaggregate growth
                              by a student's starting quartile. Are students across all quartiles
                              likely to meet their annual growth target?"),
                                          p(tags$b("Spring-to-Spring "), "accounts for summer loss, 
                            providing a more accurate depiction of a student growth 
                              over the course of the year. Use Spring-to-spring to 
                              analyze growth in 3rd, 4th, 6th, 7th, and 8th grade.")                            
                                   ),
                           column(width = 8,
                                  plotOutput("plot_growth_by_q_dim", height = "1px"),                                
                                  chartOutput("graph_growth_by_q", "highcharts")
                           )

                        )
                      )
                    ),
                    
                    #### ANALYSIS > Quartile Distributions  ####
                    tabPanel("Performance Summary",
                      fluidPage(
                        div(id = "js_tab_growth_quartile",            
                          br(),
                          column(4,
                            h4("Is the number of students reaching the 
                               Top Quartile increasing over time?"),
                            p("A student whose National Percentile Ranking (NPR) is in
                            the", tags$b("Top Quartile"), "(at or above 75th percentile), 
                            is considered on-track to being ", tags$b("college ready"),
                            ". As students meet their tiered growth targets, the number
                            of students who are reaching the top quartile should increase."),
                            p("Choose a growth season and see if the grades are making increases
                              in the number of students finishing in the top quartile"),
                            p(tags$b("Spring-to-Spring "), "accounts for summer loss, 
                            providing a more accurate depiction of a student growth 
                            over the course of the year. Use Spring-to-spring to 
                            analyze growth in 3rd, 4th, 6th, 7th, and 8th grade.")
                          ),
                          mainPanel(
                            plotOutput("plot_graph_growth_quartile_dim", height = "1px"),
                            chartOutput("graph_growth_quartile", "highcharts"),
                            p("Matched Students Only")
                          )
                        )
                      )
                    ),
                    
                    #### ANALYSIS > Start/End Quartile Flow ####
                    tabPanel("Performance by Grade",
                      fluidPage(
                        div(id = "js_tab_growth_quartile_sankey",            
                          br(),
                          column(4,
                            h4("How does quartile movement look within a grade?"),
                            p("Dig deeper into the Performance Summary results. Choose 
                              a grade and zoom in on quartile movement to visualize 
                              how quartile distribution changing over the course of the year."),
                            p(tags$b("Click a node to get started"), "(It may 
                              take a few seconds to load)."),
                            p(tags$b("Spring-to-Spring "), "accounts for summer loss, 
                            providing a more accurate depiction of a student growth 
                              over the course of the year. Use Spring-to-spring to 
                              analyze growth in 3rd, 4th, 6th, 7th, and 8th grade.")                            
                          ),
                          mainPanel(align = 'center',
                            h4("Zoomed-in Quartile Shifts"),
                            htmlOutput("graph_growth_quartile_sankey")
                          )
                        )
                      )
                    )
                  )
                )
              ),
              
            tabItem(tabName = "table_summary",
                    fluidPage(                
                bsAlert("ui_alert_table"),
                fluidRow(
                  column(6, p("Summary of students performance on Prior Spring, Fall, and Spring MAP. 
                              NWEA default norms are used.")),
                  column(6, 
                         downloadButton("dl_lookup_tidy", label = "Download CSV")
                  )
                ),
                mainPanel(
                  br(),
                  dataTableOutput("lookup_tidy")
                )                
              )
            ),
            tabItem(tabName = "table_molten",
              fluidPage(                
                bsAlert("ui_alert_table2"),
                fluidRow(
                  column(6, p("Long file of students performance on Prior Spring, Fall, and Spring MAP. 
                              NWEA default norms are used.")),
                  column(6, 
                         downloadButton("dl_lookup_molten", label = "Download CSV")
                  )
                ),
                mainPanel(
                  br(),
                  dataTableOutput("lookup_molten")
                )                
              )
            ),
            tabItem(tabName = "table_growth",
                    fluidPage(                
                      bsAlert("ui_alert_table3"),
                      fluidRow(
                        column(6, p("Summary of student growth performance from Fall-Spring and Spring-Spring.  
                              NWEA default norms are used.")),
                        column(6, 
                               downloadButton("dl_lookup_growth", label = "Download CSV")
                        )
                      ),
                      mainPanel(
                        br(),
                        dataTableOutput("lookup_growth")
                      )                
                    )
            )            
          )
        )

dashboardPage( skin = 'black',
  dashboardHeader(title = "MAP One-Stop"),
  sidebar,
  body
)