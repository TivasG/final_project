#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Inequality in the World Today",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Inequality vs. Populism Data"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        
        
        # We will use this R chunk to load and clean the data that I will use for the
        # project. First I will load the inequality data.
        
        library(readr)
        library(janitor)
        library(tidyverse)
        library(readxl)
        
        inequality <- read_csv("Income Inequality Data by Country.csv") %>%
            clean_names() 
        
        
        #We need to tidy it so we can add populism data.
        
        tidy_inequality <- inequality %>%
            pivot_longer(cols = -year, 
                         names_to = "Country Name", 
                         values_to = "Inequality Data") 
        
        #View(tidy_inequality)
        
        
        #Now we can load the populist 
        
        populist <- read_csv("speech_populist.csv") %>%
            clean_names()
        
        location <- tidy_inequality %>%
            mutate(`Country Name` = `Country Name` %>%
                       substring(21)) %>%
            mutate(`Country Name` = gsub( "_.*$", "_", `Country Name`)) %>%
            mutate(`Country Name` = substr(`Country Name`, 
                                           1, 
                                           nchar(`Country Name`)-1)) %>%
            mutate(`Country Name` = paste(toupper(substring(`Country Name`, 1,1)),
                                          (substring(`Country Name`, 2,nchar(`Country Name`))),
                                          sep="")) %>%
            rename(country = `Country Name`)
        
        new_populist = populist %>%
            rename(year = yearbegin)
        #location
        #?sub
        joined_data = full_join(new_populist, location, by = c("country", "year"))
        #View(populist)
        #View(joined_data)
        
        plot1 <- ggplot(joined_data, aes(x = `Inequality Data`, y = totalaverage)) + geom_smooth() + labs(title = "Inequality vs Populist Rhetoric in Speeches", 
                                                                                                 subtitle =  "How increased inequality affects populism in political discourse") + ylab("Populism Score")
        # Draw the histogram with the specified number of bins
        
        #hist(x, col = 'darkgray', border = 'white')
        ifelse(
            input$plot_type == "a",
            
            # If input$plot_type is "a", plot histogram of "waiting" column 
            # from the faithful dataframe
            
            x   <- plot1,
            
            # If input$plot_type is "b", plot histogram of "eruptions" column
            # from the faithful dataframe
            
            x   <- plot2
        )
        x
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
