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
    title = span( "Inequality in the World Today", style = "background-color: #DEEBF7", style="color:red"),
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
                     mainPanel(plotOutput("line_plot"))),
                 p("In this graph, we see the relationship between the log of inequality,
                   and the log of two different populism-related variables: populism of 
                   nation leaders, and populism present in the rhetoric of their speeches.
                   \n \nIn both of these graphs, points are clustered at the origin, with 
                   some outliers spread across the graphs. With this type of relationship,
                   I decided to analyze the log of each of the variables instead. The 
                   coefficient of the log variable is the elasticity. In other words, how
                   a percent change in inequality leads to a percent change in each of the
                   populism variables.
                   \n\nAs we can see in the graph, there is no evidence of a causal relationship
                   between inequality and either of the populism variables we have chosen.
                   The models demonstrating this more clearly are discussed on the Models
                   page. There are several explanations for why there isn't a relationship
                   here. On one hand, it is possible that inequality simply has no effect
                   on populism. We cannot prove otherwise with our modeling of this data.
                   It is also possible that there is some effect, and we have failed to
                   properly isolate it with either this data or this model. Perhaps in 
                   highly unequal countries, there is no populist rise; only in nations with
                   recent increases in populism experience such an effect. Perhaps there is
                   some time lag between a nation becoming more unequal and the rise of
                   populism. Perhaps there is some other relationship, or confounding factor
                   that affects inequality and populism in our world.")
             )),
    tabPanel("Social Spending",
             titlePanel("Inequality vs. Social Spending"),
             mainPanel(
                 img(src='social_spending.png', align = "left"),
                 
             ),
             p("In this graph we can see the relationship between inequality in countries
                and each countries level of social spending as a percentage of their GDP. 
                As one can see in the scatterplot above, there appears to be a strong 
                relationship between social spending and GDP. More evidence to support
                this assertion can be found on the Models page. We see a high level of
                inequality in a cluster of countries with low social spending on the left
                hand side of the graph, and then another, larger cluster of countries with 
                relatively low (compared to the first cluster) and slightly decreasing 
                inequality on the right hand side of the graph. As the trend line demonstrates
                as the level of social spending increses, the inequality within that country
                tends to go down. Now, this graph doesn't prove this relationship. There
                could be some other factor which impacts the countries in the first cluster;
                the relationship would not be as strong without their influence. Additionally,
                social spenidng could be tied to another variable which decreases inequality,
                such as higher tax rates among the rich. But we can observe that there is
                evidence for some relationship. Finally, compared to the other two variables
                in this project, the direction of causality in my hypothesis is reversed.
                I don't really believe that higher inequality causes lower levels of social
                spending. Rather, I believe that higher social spending can reduce inequality.
                This is different than the other two variables, where I hypothesize that
                inequality has some impact on those factors."
             )
             ),
    tabPanel("Happiness",
             titlePanel("Inequality vs. Happiness"),
             mainPanel(
                 img(src='happiness.png', align = "left"),
                 ### the rest of later code
             ),
             mainPanel(
             p(
               "In this graph we see the relationship between Inequality and Happiness
               as defined by the World Happiness Report. At first glance, there does
               seem to be a negative relationship between inequality and happiness, 
               meaning as inequality increases, happiness decreases. This relationship
               does seem to exist, and more evidence to support it is presented in the
               Models section of this project website. It is also worth noting that this
               relationship could be considered partly tautological, as one of the inputs
               in the 'Happiness equation' utilized by the World Happiness Report is the
               Gini index score for each country a seperate measure of inequality. However
               given the strength of the relationship, it is a fair hypothesis that 
               inequality affects the other factors utilized by the World Inequality Report
               as well. Additionally, I introduced regional considerations into this graphic.
               I was somewhat worried about the Simpson's Paradox undermining this analysis.
               This would mean that there is actually a positive relationship between inequality
               and happiness, and this would be exposed by the introduction of a third variable,
               reigon. However, this negative trend seems to hold for each of the 'colors', or
               regions, in the graph."
             ))
    ),
    tabPanel("Models",
    titlePanel("Ineqluality and Populism"),
    verticalLayout(
    h3("Linear Models"),
    p("I began my exploration of the inequality and populism datasets by attempting
      to construct linear models for each specific dataset. After clearning the data
      and joining the data by country and year, I created a linear regression model
      looking at the relationship between inequality and the populism of a nation's 
      leaders. The results of the model can be seen in the table below."),
    mainPanel(
      img(src='model_1_populism.png', align = "center"),
      ### the rest of later code
      h3(""),
      p("")
    ),
    h3(""),
    p(" Although we do have a positive coefficient of .28 with inequality on popluism, meaning
      an increase in inequality leads to an increase in populism, this coefficient
      is not statistically signficant. This fact can be seen by the fact that the upper
      and lower bounds of the confidence interval include zero. This means that we fail
      to say with 95% confidence that the true coefficient of this relationship is not zero,
      and we cannot prove a causal relationship. We can see a similar relationship between the 
      inequality of countries and the rhetoric of their leader's speeches. The model is presented
      in the table below."),
    mainPanel(
      img(src='model_2_populism.png', align = "center"),
      ### the rest of later code
      h3(""),
      p("")
    ),
    p("Here the coefficient is even slightly negative, and the confidence interval once again
      contains zero. With both sets of data, there is just not enough evidence to suggest a causal
      relationship with inequality and populism utilizing a linear model."),
    h3("Log Models"),
    p("Since the data was bunched around the orgin of the scatterplot with outliers, I thought
      graphing the logs of the data could provide a better sense of the elasticity between the
      two variables. As explained on the first page, this means the percent change in one
      variable caused by a percent change in the other variable. In creating the log model
      with inequality and the populism of leaders, we get the below output."),
    mainPanel(
      img(src='model_1_populism_log.png', align = "center"),
      ### the rest of later code
      h3(""),
      p("")
    ),
    p("Once again, we see that the confidence interval contains zero, futher proof
      that there is not a causal relationship between inequality and the populism of
      a country's leaders. Lets look at the relationship between the log of inequality
      and the log of populism."),
    mainPanel(
      img(src='model_2_populism_log.png', align = "center"),
      ### the rest of later code
      h3(""),
      p("")
    ),
    p("We too see here that the confidence here contains 0, more evidence that there
      is also no relationship between the log of inequality and the log of the populist
      rhetoric used by a nation's leaders.")
    ),
    titlePanel("Inequality and Social Spending"),
    verticalLayout(
    h3("Linear Model"),
    p("Here we once again see a simple linear model which highlights the relationship
      between inequality and social spending The output of the model is shown below, and is
      also visually represented in the social spending tab above."),
    mainPanel(
      img(src='model_1_socialgdp.png', align = "center"),
      ### the rest of later code
      h3(""),
      p("")
    ),
    p("Here we see a negative coefficient of -.003. While this number may seem small,
      this is mostly due to the figures that we are working with. This coefficient means
      that a increase of 1% of a nation's GDP in social spending reduces the share of
      national income owned by the top 1% by .3 percent, which is significant when the
      difference in shares of income for the top 1% between the most egalitarian and most
      unequal countries is only a few percent. Even if you still consider this difference
      small, the most important aspect about it is that it is statistically significant.
      Since the 95% confidence interval does not include 0, we can say with statistic
      signifance that there is a negative causal relationship between inequality and social
      spending.")
    ),
    titlePanel("Inequality and Happiness"),
    verticalLayout(
    h3("Linear Model"),
    p("Here we see a linear model between the happiness variable and the inequality metric.
      As seen in the visual representation on the happiness tab, there would appear to be a
      significant negative relationship between the happiness variable and inequality in our
      model. The model below explores this potential relationship."),
    mainPanel(
      img(src='model_1_happiness.png', align = "center"),
      ### the rest of later code
      h3(""),
      p("")
    ),
    p("As we can see in the model, there is statisticall siginficant evidence for a
      negative relationship between happiness and inequality. This is another way of
      saying that as inequality in a country increases, the happiness score in that
      country decreases. The variance in the data has resulted in quite a large range
      of possible coefficients for how much a change inequality affects happiness, but
      the important takeaway here is that the confidence level does not include zero. 
      This signifies statistically significant evidence of a negative causal relationship."),
    h3("Multiple Regression Model"),
    p("The dataset included several other pertinent indicators of a nation's wellbeing,
      and I thought it would be interesting to see if inequality affected any of these
      metrics. Due to the nature of linear regression, I had to flip the direction of causality -
      for example, see how freedom, family, and generosity impacted inequality rather
      than the other way around, but the findings from the multiple linear regression should
      still be useful."),
    mainPanel(
      img(src='model_2_happiness.png', align = "center"),
      ### the rest of later code
      h3(""),
      p("")
    ),
    p("The results of our multiple linear regression show that inequality does not have a
      statsitically significant relationship with every variable in this dataset. The 95% 
      confidence intervals for the coefficients of freedom, trust, and generosity contain
      zero, meaning that there is no evidence of a causal relationship between inequality
      and any of these variables. Meanwhile, this is not true for the family and trust variables.
      Inequality, according to this multiple linear regression model, has a negative causal
      relationship with health and geneorsity.")
    )),
    tabPanel("About",
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
      World Happiness Report."), 
             titlePanel("About the Author"),
             p("My name is Tivas Gupta and I study Economics at Harvard.
             I am passionate about data science and interested in 
             the role it plays in economics and in forming public
             policy.
             You can reach me at tivasgupta@college.harvard.edu.
             My GitHub, which contains some of my other work and explorations
             in Data Analysis, is https://github.com/TivasG.")))

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
