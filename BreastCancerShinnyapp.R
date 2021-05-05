

library(shiny)


breastcancer <- read.csv("breastcancer.csv")
breastcancer

ui <- fluidPage(
    titlePanel("Breast Cancer"),
    sidebarLayout(mainPanel(h3("Breast Cancer"),
                            plotlyOutput("breastcancer")
    ),
    sidebarPanel(
        radioButtons(inputId = "diagnosis",
                     label = h4("Diagnosis"),
                     choices = c("B", "M")),
        sliderInput(inputId = "smoothness_mean",
                    label = h4("Smoothness Mean:"),
                    min = 0.05263,
                    max = 0.1634,
                    value = 0.05263),
        sliderInput(inputId = "area_mean",
                    label = h4("Area Mean:"),
                    min = 144,
                    max = 2500,
                    value = 144)

    )
    ))

server <- function(input, output) {
    
    
    output$breastcancer <- renderPlotly({
        
        plot <- breastcancer %>% filter(diagnosis == input$diagnosis, area_mean >= input$area_mean) %>%
             ggplot(mapping = aes(x = smoothness_mean, y = area_mean, color = diagnosis)) +
                geom_line() +
                theme_minimal() +
                labs(title = "Breast Cancer",
                     x = "Smoothness Mean",
                     y = "Texture Mean",
                     caption = "Source = breastcancer.csv")
            
            ggplotly(plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)