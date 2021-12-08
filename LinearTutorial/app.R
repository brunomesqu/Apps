
library(shiny)
library(shinythemes)
library(modelr)
library(tidyverse)
library(ggplot2)

# Define UI for application.
ui <- fluidPage(theme = shinytheme("sandstone"),

    # Application title
    titlePanel("Fitting a Line to Your Data"),

    # Sidebar with a slider input for input 
    sidebarLayout(
        sidebarPanel(
            sliderInput("slope",
                        "Slope:",
                        min = 0,
                        max = 5,
                        step = 0.01,
                        value = 4),
            
            sliderInput("intercept","Intercept:",
                        min=0,
                        max=8,
                        step=0.01,
                        value = 2)
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
            slope = as.numeric(input$slope),
            intercept = as.numeric(input$intercept)
        )
    })
    
# Organize input data into a data frame that will be used to draw the plot
    output$plot1 <- renderPlot({
        
        df <- as.data.frame(sf())
        
        dataplot <- as.data.frame(sim1)
        
        yhat <- df$intercept + (df$slope*dataplot$x)
        dataplot <- cbind(dataplot,yhat)
        
# Calculate the parameters 
        
        diff <- dataplot$y - yhat
        Varfit <- (mean(diff^2))
        Varmean <- mean((sim1$y-mean(sim1$y))^2)
        Rsqrd <- as.numeric((Varmean-Varfit)/Varmean)
        subtitle <- sprintf("Rsqrd: %g",round(Rsqrd, digits=3))
        lineplot <- as.data.frame(cbind(df,Rsqrd,subtitle))
        
        ggplot(dataplot, aes(x = x,y = y)) + 
            geom_point(size = 2) +
            geom_abline(data = lineplot,size = 1, aes(intercept = intercept, slope = slope)) +
            geom_segment(aes(xend=x),yend=yhat,color="darkred")+
            scale_color_manual(values = c(resid = "darkred"), labels = c(resid = "residuals"))+
            theme_bw(base_size=16)+
            labs(title = "Regression Line on Scatterplot"
                 ,subtitle = subtitle,
                 x = 'Predictor',
                 y = 'Variable')
            
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
