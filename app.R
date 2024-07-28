# Building a shiny dashboard to show statistics of several car owners

#Loading the necessary libraries


library(shiny)

library(tidyverse)

library(ggplot2)

library(readr)

# loading the data set & preparation

car_owners <- read_csv("shinydeveloper/car_owners.csv")

car_owners$income_fct <- factor(car_owners$income, levels = c("Under $25", "$25 - $49",
                                                      "$50 - $74", "$75+"))

ui <- fluidPage(
    
    ## title row
    
    fluidRow(
        
        column(1),
        
        column(5, titlePanel("Car Owners")),
        
        column(6,titlePanel("By Reuben"))
        
        
    ),
    
    ## inputs row
    
    fluidRow(
        
        column(1),
        
        column(4,
               
               br(), br(),
               
               selectInput("gender", "Choose a gender group", choices = c("Female", "Male"))
               
               
        ),
        
        column(4,
               
               br(), br(),
               
               sliderInput("age", "Select an age range", min = 18, max = 77,
                           value = c(30, 50))
               
        ),
        
        column(3,
               
               br(), br(), br(),
               
               actionButton("go", "Get Results")
               
        )
        
        
    ),
    
    ## horizontal line
    
    fluidRow(
        
        column(12, hr())
        
    ),
    
    ## text row
    
    fluidRow(
        
        column(1),
        
        column(11,
               
               br(), br(),
               
               h4(strong(textOutput("entries_text")))
               
               
        )
        
        
    ),
    
    ## table row
    
    fluidRow(
        
        column(1),
        
        column(11,
               
               br(), br(),
               
               dataTableOutput("entries_table")
               
               
        )
        
        
    ),
    
    ## charts row
    
    fluidRow(
        
        column(1),
        
        column(5,
               
               br(), br(),
               
               plotOutput("bar_chart")
               
               
        ),
        
        column(6,
               
               br(), br(),
               
               plotOutput("pie_chart")
               
               
               
        )
        
        
        
    )
    
)

server <- function(input, output) {
    
    ##### DATA PREPARATION ##################
    
    ### create the filtered data set
    
    owners_filtered <- eventReactive(input$go, {
        
        car_owners %>% filter(gender == input$gender,
                          age >= input$age[1],
                          age <= input$age[2]) %>%
            select(car_price, income, education, marital, income_fct)
        
        
    })
    
    ### compute the number of entries (owners) in the filtered data set
    
    entries <- eventReactive(input$go, {
        
        owners_filtered() %>% count()
        
    })
    
    ### create the data frame with the frequencies of the education variable
    ### (needed for the pie chart)
    
    frequencies <- eventReactive(input$go, {
        
        owners_filtered() %>% count(education)
        
    })
    
    frequencies_df <- eventReactive(input$go, {
        
        as.data.frame(frequencies())
        
    })
    
    ##### CREATE THE OUTPUT OBJECTS ##################
    
    ### print the text
    
    output$entries_text <- renderText({
        
        paste0("Number of owners: ", entries())
        
    })
    
    ### display the table
    
    output$entries_table <- renderDataTable(
        
        options = list(pageLength = 5),
        
        {
            
            owners_filtered()[,-5]
            
        } )
    
    ### plot the bar chart
    
    output$bar_chart <- renderPlot(
        
        {
            
            ggplot(owners_filtered(), aes(income_fct, car_price))+
                geom_bar(fill = "#985917", stat = "summary", fun = mean)+
                xlab("Income level")+
                ylab("Average car price")+
                labs(title = "Average Car Price by Income Level",
                     subtitle = paste0("Gender: ", input$gender))+
                theme(plot.title = element_text(size = 17, hjust = 0.5, face = "bold"),
                      plot.subtitle = element_text(size = 14, hjust = 0.5),
                      panel.background = element_rect(fill = "white", colour = "black"))
            
            
        })
    
    
    ### plot the pie chart
    
    output$pie_chart <- renderPlot({
        
        ggplot(frequencies_df(), aes(x = "", y = n, fill = education))+
            geom_bar(stat = "identity")+
            coord_polar(theta = "y", start = 0)+
            labs(title = "Structure by Education Level",
                 subtitle = paste0("Gender: ", input$gender))+
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  plot.title = element_text(size = 17, hjust = 0.5, face = "bold"),
                  plot.subtitle = element_text(size = 14, hjust = 0.5))+
            scale_fill_brewer(palette = "Greens")+
            labs(fill = "Education")
        
        
    })
    
    
    
}

shinyApp(ui = ui, server = server)