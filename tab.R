library(shiny)
library(ggplot2)
library(ggridges)
library(dplyr)
library(NHANES)

ui <- fluidPage(
  titlePanel("BMI by Age & Gender"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Select Gender", choices = c("Both", "male", "female")),
      checkboxInput("show_obese", "Show Obesity Range (BMI ≥ 30)", value = FALSE)
    ),
    mainPanel(
      plotOutput("ridge_plot", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    NHANESraw %>%
      filter(!is.na(Age), !is.na(BMI), !is.na(Gender)) %>%
      mutate(Age_group = cut(
        Age,
        breaks = c(0,18,30,45,60,80,100),
        labels = c("0-17","18-29","30-44","45-59","60-79","80+"),
        right = FALSE
      ))
  })
  
  filtered_data <- reactive({
    if (input$gender == "Both") return(data())
    data() %>% filter(Gender == input$gender)
  })
  
  
  output$ridge_plot <- renderPlot({
    d <- filtered_data()
    
    median_bmi <- d %>%
      group_by(Age_group) %>%
      summarize(m = median(BMI), .groups = "drop") %>%
      mutate(
        y_val = as.numeric(Age_group),
        ystart = y_val - 0.15,
        yend   = y_val + 0.15
      )
    
    p <- ggplot(d, aes(x = BMI, y = Age_group, fill = Gender)) +
      geom_density_ridges(alpha = 0.7, scale = 0.9) +
      geom_segment(
        data = median_bmi,
        aes(x = m, xend = m, y = ystart, yend = yend),
        linewidth = 0.8,
        color = "black",
        inherit.aes = FALSE
      ) +
      geom_text(
        data = median_bmi,
        aes(x = m, y = yend + 0.1, label = round(m, 1)),
        color = "black",
        size = 3,
        inherit.aes = FALSE
      ) +
      scale_fill_manual(values = c("male" = "#1f77b4", "female" = "#FF66DF")) +
      labs(
        title = "Interactive BMI Distribution",
        x = "BMI",
        y = "Age Group",
        fill = "Gender"
      ) +
      theme_minimal(base_size = 14)

    if (input$show_obese) {
      p <- p +
        annotate(
          "rect",
          xmin = 30, xmax = Inf, ymin = -Inf, ymax = Inf,
          fill = "red",
          alpha = 0.5
        ) +
        geom_vline(
          xintercept = 30,
          color = "red",
          linetype = "dashed",
          linewidth = 0.9
        ) +
        annotate(
          "text",
          x = 35,
          y = 6.45,
          label = "Obese Range",
          color = "black",
          size = 6,
          fontface = "bold",
          hjust = 0
        )
    }
    
    p
  })
}

shinyApp(ui, server)
