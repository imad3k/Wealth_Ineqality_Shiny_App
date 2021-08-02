#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)

library(ggthemes)
library(plotly)

library(DT)



races <- list("White" = "White Alone",
              "Black" = "Black Alone",
              "Hispanic" = "Hispanic")

races2 <- c("White Alone" = 1,
            "Black Alone" = 2,
            "Hispanic" = 3)





# Define UI for app

ui <- fluidPage(
    
    theme = shinytheme("cosmo"),
    
    # Title Wealth Inequality in America
    
    titlePanel("Income Inequality in America"),
    
    navbarPage("DSBA 5122",
               
               tabPanel("About",
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                HTML('
           
            <center>
            <img src = "https://logos-world.net/wp-content/uploads/2020/06/Charlotte-49ers-Logo.png",
            height = 200, width = 300>
            </center>
                                    '),
                                
                                tags$h4("Group 7: Ahmad, Rios, Choksi, Verma")
                                
                            ), # Closeout sidebarPanel
                            
                            
                            mainPanel(
                                
                                HTML(
                                    '<center>
            <img src = "https://apps.urban.org/features/wealth-inequality-charts/img/AP173355447669.jpg",
                height = 400, width = 800>
              </center>'
                                ),
                                
                                tags$hr(),
                                
                                
                                tags$h1("Tidy Tuesday (Dataset: 02-09-2021)"),
                                
                                HTML('
          <h3 style = "color:tomato"> Charts About Wealth Inequality in America </h3>
              '),
                                
                                tags$p("In this project we took a look at the Tidy Tuesday data submitted by 
            Robert Walker on February, 9th, 2021.
            The goal of his study was to recreate some of the charts from an article 
            published by The Urban Institute"),
                                
                                tags$a(href="https://apps.urban.org/features/wealth-inequality-charts/", "(Link)."),
                                
                            ) # Close main Panel
                        ) #Close Sidebar Laout
               ), # Close tabPanel
               
               
               
               ############## Start of first Page #################
               
               tabPanel("Chart 1",
                        
                        # Sidebar with a slider input
                        
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput(inputId = "slide",
                                            label = "Select a Starting Population Percentile:",
                                            min = min(wealth_distribution$percentile),
                                            max = max(wealth_distribution$percentile),
                                            value = 1,
                                            step = 10),
                                
                                sliderInput(inputId = "slide1a",
                                            label = "Select an Ending Population Percentile:",
                                            min = min(wealth_distribution$percentile),
                                            max = max(wealth_distribution$percentile),
                                            value = 99,
                                            step = 10),
                                
                                tags$hr(),
                                
                                sliderInput(inputId = "wordfontsize1",
                                            label = "Change Font size",
                                            min = 8,
                                            max = 30,
                                            value = 14),
                                
                                
                                
                                tags$hr(),
                                
                                # If we want to add an update button use this
                                
                                actionButton(inputId = "go",
                                             label = "Update")
                                
                                
                            ), # Close sidebar Panel
                            
                            # Set aside a space for our chart
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Plot", plotOutput("graph1")), 
                                    tabPanel("Data Table", DT::dataTableOutput("my_table_chart1"))
                                ) # tabsetPanel Closeout
                                
                                
                            ) # Main panel closeout
                            
                        ), # Sidebar closeout
                        
               ), # Tabpanel closeout
               
               
               ################## End of First Page #######################              
               
               
               
               
               
               ################## Start of second page ####################
               
               tabPanel("Chart 2",
                        
                        # Sidebar with a slider input
                        
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput(inputId = "slide2",
                                            label = "Select a Starting Year:",
                                            min = min(all_h3_comb$year),
                                            max = max(all_h3_comb$year),
                                            value = c(min(all_h3_comb$year), max(all_h3_comb$year)),
                                            step = 1),
                                
                                
                                tags$hr(),
                                
                                tags$h4(tags$b("For the Data Table:")),
                                
                                checkboxGroupInput(inputId = "show_vars",
                                                   label = "Select a Variable to Show",
                                                   choices = names(all_h3_comb),
                                                   selected = names(all_h3_comb)),
                                
                                tags$hr(),
                                
                                sliderInput(inputId = "wordfontsize2",
                                            label = "Change Font size",
                                            min = 8,
                                            max = 30,
                                            value = 14
                                ),
                                
                                tags$hr(),
                                
                                # If we want to add an update button use this
                                
                                actionButton(inputId = "go2",
                                             label = "Update"),
                                
                            ), # Side panel closeout
                            
                            
                            # Set aside a space for our chart
                            mainPanel(
                                
                                tabsetPanel(
                                    tabPanel("Plot", plotOutput("graph2")), 
                                    tabPanel("Data Table", DT::dataTableOutput("my_table1"))
                                )
                                
                            ) # Main panel closeout
                            
                        ), # Sidebar closeout
                        
               ), # Tabpanel closeout
               
               
               ################## End of Second Page #######################
               
               
               
               
               
               
               ############## Start of Third Page #################
               
               tabPanel("Chart 3",
                        
                        # Sidebar with a slider input
                        
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput(inputId = "slide3",
                                            label = "Select a Starting Year:",
                                            min = 1980, 
                                            max = max(income_distribution$year),
                                            value = c(min(income_distribution$year), max(income_distribution$year)),
                                            step = 1),
                                
                                
                                
                                tags$hr(),
                                
                                
                                sliderInput(inputId = "wordfontsize3",
                                            label = "Change Font size",
                                            min = 8,
                                            max = 30,
                                            value = 14),
                                
                                tags$hr(),
                                
                                # If we want to add an update button use this
                                
                                actionButton(inputId = "go3",
                                             label = "Update"),
                                
                                
                            ), # Close sidebar Panel
                            
                            # Set aside a space for our chart
                            mainPanel(
                                
                                tabsetPanel(
                                    tabPanel("Plot", plotlyOutput("graph3")), 
                                    tabPanel("Data Table", DT::dataTableOutput("my_table_chart3"))
                                    
                                ) # tabsetPanel Closeout
                                
                                
                            ) # Main panel closeout
                        ), # Sidebar closeout
                        
               ) # Tabpanel closeout
               
               
               ################## End of Third Page #######################
               
               
               
    ) # Navbar page closeout
    
) # fluidpage closeout



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    #################### Render FIRST chart #################
    
    
    
    data <- reactive({req(input$slide)
        
        df <- wealth_distribution %>%
            filter(percentile >= input$slide) %>% 
            filter(percentile <= input$slide1a)
        
    }) # closeout Reactive
    
    
    output$graph1 <- renderPlot({
        
        ggplot(data(),
               aes(x = percentile,
                   y = wealth_family,
                   color = year)) +
            geom_point(size = 2.5, show.legend = FALSE) +
            geom_line(show.legend = FALSE) +
            facet_wrap(~year) +
            scale_x_continuous(breaks = c(25, 50, 75, 100)) +
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
            ggtitle("Wealth Distribution by Population Percentile Over Time") +
            xlab("Population Percentile") +
            ylab("Wealth") +
            theme_stata() +
            theme(axis.text.y = element_text(angle = 0),
                  text=element_text(size= input$wordfontsize1),
                  panel.grid.major.x = element_line(linetype = 4),
                  panel.grid.major.y = element_line(linetype = 4)) +
            scale_color_viridis_c()
        
    }) # Close out renderplot
    
    # Render DT Table part
    
    mytable1 <- wealth_distribution[sample(nrow(wealth_distribution), 1000), ]
    
    output$my_table_chart1 <- DT::renderDataTable({
        DT::datatable(mytable1)
    }) # Closeout renderTable
    
    
    #################### END Render FIRST chart #################    
    
    
    
    
    
    #################### Render second chart #################
    
    
    data2 <- reactive({req(input$slide2)
        
        years <- reactive({
            seq(input$slide2[1], input$slide2[2], by = 1)
        })
        
        df2 <- all_h3_comb %>%
            filter(income_quintile != "Top 5%") %>%
            mutate(income_quintile = factor(income_quintile, levels = c("Lowest", "Second", "Middle", "Fourth", "Highest"))) %>%
            filter(dollar_type == "2019 Dollars") %>%
            filter(race %in% c("White Alone", "Black Alone", "Hispanic")) %>%
            filter(year %in% years())
        
        
    }) # Closeout Reactive
    
    
    output$graph2 <- renderPlot({
        
        ggplot(data2(),
               aes(x = year, y = income_dollars, color = race, fill = race, group = race)) +
            geom_point(size = 0.15) +
            geom_smooth(method = lm) +
            geom_line() +
            facet_wrap(~income_quintile, scales = "free_y") +
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
            theme(strip.text = element_text(color = "white",
                                            face = "bold",
                                            size = 14),
                  strip.background = element_rect(fill = "grey50")) +
            labs(title = "Change in Family Income by Earning Brackets") +
            xlab("Year") +
            ylab("Family Income (Dollars)") +
            theme_stata() +
            theme(axis.text.y = element_text(angle = 0),
                  text=element_text(size= input$wordfontsize2),
                  panel.grid.major.x = element_line(linetype = 4),
                  panel.grid.major.y = element_line(linetype = 4))
        
    }) # Close out renderplot
    
    # Display DT Table
    
    mytable <- all_h3_comb[sample(nrow(all_h3_comb), 1000), ]
    
    output$my_table1 <- DT::renderDataTable({
        
        DT::datatable(mytable[, input$show_vars, drop = FALSE])
        
    }) # Closeout renderTable
    
    
    
    #################### END Render SECOND chart #################    
    
    
    
    
    
    #################### Render THIRD chart #################    
    
    
    data3 <- reactive({req(input$slide3)
        
        years3 <- reactive({
            seq(input$slide3[1], input$slide3[2], by = 1)
        })    
        
        df3 <- income_distribution %>%
            filter(race %in% c("Black Alone", "White Alone", "Hispanic (Any Race)")) %>%
            filter(year >= 1980) %>% 
            filter(year %in% years3())
        
    }) # Closeout Reactive
    
    
    output$graph3 <- renderPlotly({
        
        p <- ggplot(data3(),
                    aes(x= year, y = income_mean, color= race, fill= race)) +
            geom_bar(stat= "identity", width = 0.2) + 
            facet_wrap(~race) +
            scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
            theme_stata() +
            theme(legend.position = "none",
                  text=element_text(size= input$wordfontsize3),
                  axis.text.x = element_text(angle = 0),
                  axis.text.y = element_text(angle = 0)) +
            labs(title = "Distribution of Mean Income by Race") +
            xlab("Year") +
            ylab(" Mean Income")
        
        ggplotly(p)
        
    }) # Close out renderplot  
    
    # DT Data table part
    
    mytable3 <- income_distribution[sample(nrow(income_distribution), 1000), ]
    
    output$my_table_chart3 <- DT::renderDataTable({
        
        DT::datatable(mytable3)
        
    }) # Closeout renderTable
    
    
    #################### END Render THIRD chart #################      
    
    
} # Closeout Server



# Run the application
shinyApp(ui = ui, server = server)