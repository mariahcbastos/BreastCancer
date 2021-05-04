library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(ggthemes)

breastcancer <- read.csv("breastcancer.csv")

ui <- fluidPage(
    titlePanel("Breast Cancer"),
    sidebarLayout(mainPanel(h3("Breast Cancer"),
                            plotlyOutput("breastcancer")
    ),
    sidebarPanel(
        selectInput(inputId = "diagnosis",
                     label = h4("Diagnosis"),
                     choices = c("B", "M")),
        sliderInput(inputId = "area_mean",
                    label = h4("Area Mean:"),
                    min = 144,
                    max = 2500,
                    value = 144)
    )
    ))

server <- function(input, output) {
    
    output$breastcancer <- renderPlotly({
        
        plot <- breastcancer%>% filter(area_mean >= input$area_mean, diagnosis == input$diagnosis) %>%
            ggplot(mapping = aes(x = smoothness_mean, y = area_mean, color = diagnosis), 
                   text = paste0("Smoothness Mean:", str_to_title(smoothness_mean),
                                 "Area Mean:", str_to_title(area_mean),
                                 "Diagnosis:", str_to_title(diagnosis))) +
            geom_line() +
            theme_few() +
            theme(legend.position = "none") +
            scale_x_continuous() +
            scale_y_continuous() +
            labs(title = "Breast Cancer",
                 x = "Smoothness Mean",
                 y = "Area Mean",
                 caption = "Source = breastcancer.csv")
        
        ggplotly(plot)
    })
    
    
}

shinyApp(ui = ui, server = server)