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
    tabPanel("Populism",
             fluidPage(
                 titlePanel("Inequality vs. Populism"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Graphs of Inequality vs Populism",
                             c("Graph 1" = "a", "Graph 2" = "b")
                         )),
                     mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Social Spending",
                titlePanel("Inequality vs. Social Spending"),
             mainPanel(
                 img(src='social_spending.png', align = "left"),
                 ### the rest of later code
             )
             ),
    tabPanel("Happiness",
             titlePanel("Inequality vs. Happiness"),
             mainPanel(
                 img(src='happiness.png', align = "left"),
                 ### the rest of later code
             )
    ),
    tabPanel("About Project",
            titlePanel("About Project"),
    h3("Project Background and Motivations"),
    p("I was inspired to undergo this project after reading 
      Capital in the 21st Century by Thomas Piketty. This 
      influential economics book on global wealth and income
      inequality quickly became one of my favorite books, 
      and I wanted to explore the issue further. One of the 
      main questions I had after reading this book was 'what 
      are some of the marginal impacts of inequality?' This
      question served as the inspiration for this project."),
    h3("Project Data"),
    p("The inequality data used throughout this proejct is
      from Piketty and his fellow researcher's own database,
      the World Inequality Database. I decided to look at three
      indicators, and their relationship with inequality: populism,
      social spending, and happiness. For the populism indicator, I 
      used two data sources, one which looked at populist
      stances taken by elected leaders, and another which looked
      at the populist rhetoric used by leaders in speeches. For 
      the social spending datasource, I used data from the OECD,
      which has a wide variety of the level of social spending
      within a country. Finally, my happiness data is from the
      World Happiness Report.")),
    tabPanel("About Author", 
             titlePanel("About Tivas"),
             p("My name is Tivas Gupta and I study Economics at Harvard.
             I am passionate about data science and interested in 
             the role it plays in economics and in forming public
             policy.
             You can reach me at tivasgupta@college.harvard.edu.")))

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
        data_log <- joined_data %>%
            mutate(log_inequality= log(`Inequality Data`), 
                   log_populism = log(totalaverage)) %>%
            filter(log_populism > -4)
        
        Populist_Data <- read_excel("Populist Data.xlsx")
        #View(Populist_Data)
        
        new_populist <- Populist_Data %>%
            rename(year = `year begin`)
        
        new_populist <- new_populist %>%
            select(country, `average score`, year)
        
        #view(new_populist)
        
        inequality <- read_csv("Income Inequality Data by Country.csv")
        
        
        tidy_inequality <- inequality %>%
            pivot_longer(cols = -Year, 
                         names_to = "Country Name", 
                         values_to = "Inequality Data") 
        
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
            rename(country = `Country Name`) %>%
            rename(year = Year)
        
        inequality <- location %>%
            filter(!is.na(`Inequality Data`))
        
        named_inequality <- inequality %>%
            mutate(`Country name` = (substr(country, 1, nchar(`country`) - 40))) #%>%
        #mutate(`Nation` = ifelse(`Country name` == "New", 
        #"New Zealand", 
        #(substr(`Country name`, 1, nchar(`Country name`) - 8)))) 
        #view(named_inequality)
        
        final_inequality <- named_inequality %>%
            select(year, `Country name`, `Inequality Data`) %>%
            rename(country2 = `Country name`) %>%
            mutate(country = ifelse(country2 == "USA", "United States", 
                                    (ifelse(country2 == "Russian Federation", "Russia", country2)))) %>% 
            select(-country2)
        
        joined_data = full_join(new_populist, final_inequality, by = c("country", "year"))
        
        #View(new_populist)
        #View(final_inequality)
        #View(joined_data)
        
        true_data <- joined_data %>%
            filter(!is.na(`Inequality Data`)) %>%
            filter(!is.na(`average score`))
        
        true_data <- true_data %>%
            filter(`average score` != 0.00000000)
        
        
        #true_data
        plot_notlogged <- ggplot(true_data, aes(x = `Inequality Data`, y = `average score`)) + geom_point() 
        
        plot_notlogged
        
        
        data_log2 <- true_data %>%
            mutate(log_inequality2= log(`Inequality Data`), 
                   log_populism2 = log(`average score`))
        
        plot1 <- ggplot(data_log, aes(x = log_inequality, y = log_populism))+
            geom_point() +
            geom_smooth(method = lm, se = FALSE) + 
            labs(title = "Inequality vs Populist Levels of Leaders", 
                 subtitle =  "Both Inequality and the Populism Variables are Logged") + ylab("Populism Score") + xlab("Inequality Score") + theme_classic()
        
        plot2 <-  ggplot(data_log2, aes(x = log_inequality2, y = log_populism2))+
            geom_point() +
            geom_smooth(method = lm, se = FALSE) + 
            labs(title = "Inequality vs Populist Rhetoric in Speeches", 
                 subtitle =  "Both Inequality and the Populism Variables are Logged") + ylab("Populism Score") + xlab("Inequality Score") + theme_classic()
        
        # plot2
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
    output$myImage <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = 'social_spending.png')
        
        # Generate the PNG
        png(outfile, width = 400, height = 300)
        hist(rnorm(input$obs), main = "Generated in renderImage()")
        dev.off()
        
        # Return a list containing the filename
        list(src = outfile,
             contentType = 'image/png',
             width = 400,
             height = 300,
             alt = "This is alternate text")
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
