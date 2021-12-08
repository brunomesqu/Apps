
library(shiny)
library(shinythemes)
library(modelr)
library(tidyverse)
library(ggplot2)
library(mlbench)

# Define UI for application.
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # Application title
                titlePanel("The Sigmoid and Your Data"),
                
                # Sidebar with a slider input for input 
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("intercept","Intercept:",
                                    min = -9,
                                    max = -3,
                                    step = 0.1,
                                    value = -7),
                        
                        sliderInput("coefficient","Coefficient:",
                                    min=0,
                                    max=0.1,
                                    step=0.01,
                                    value = 0)
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        plotOutput("plot1")
                    )
                )
)

# Define server logic required to draw the plot
server <- function(input, output) {
    # Capture input from sliders    
    sf <- reactive({
        data.frame(
            coefficient = as.numeric(input$coefficient),
            intercept = as.numeric(input$intercept)
        )
    })
    
    # Organize input data into a data frame that will be used to draw the plot
    output$plot1 <- renderPlot({
        
        df <- as.data.frame(sf())
        
        data("PimaIndiansDiabetes2", package = "mlbench")
        PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
        mydata <- PimaIndiansDiabetes2 %>% 
            mutate(diab = case_when(diabetes == "neg" ~ 0,
                                    TRUE ~ 1))
        
        phat <- (exp((df$intercept)+((df$coefficient)*mydata$glucose))/(1+(exp((df$intercept)+((df$coefficient)*mydata$glucose)))))
        phat.df <- as.data.frame(phat)
        mydata <- cbind(phat.df,mydata)
        
        # Plot graph
        
        ggplot(mydata, aes(x=glucose, y=diab)) + 
            geom_point(size = 2,alpha=.5) +
            geom_line(aes(y=phat), color="darkred", size = 1) + 
            theme_bw(base_size = 16)+
            labs(title = "Probabilities in Logistic Regression",
                 x = 'Predictor',
                 y = 'Binary Outcome')
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
