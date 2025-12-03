library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(NHANES)

clean_data <- NHANESraw |>
  select(Age, Gender, BMI, PhysActiveDays, Work, Depressed, SleepHrsNight, 
         Alcohol12PlusYr, Marijuana, HardDrugs) |>
  filter(!is.na(Age), !is.na(Gender), !is.na(BMI), !is.na(Work), 
         !is.na(Depressed), !is.na(Alcohol12PlusYr), !is.na(Marijuana), 
         !is.na(HardDrugs), !is.na(PhysActiveDays), !is.na(SleepHrsNight)) |>
  mutate(
    Work_bin = factor(ifelse(Work == "Working", "Yes", "No"), levels = c("No", "Yes")),
    Depressed_bin = factor(ifelse(Depressed %in% c("Several", "Most"), "Yes", "No"),
                           levels = c("No", "Yes")),
    Alcohol_bin = factor(ifelse(Alcohol12PlusYr == "Yes", "Yes", "No"), levels = c("No", "Yes")),
    Drug_bin = factor(ifelse(Marijuana == "Yes" | HardDrugs == "Yes", "Yes", "No"), 
                      levels = c("No", "Yes")),
    PhysActive_cat = factor(case_when(
      PhysActiveDays == 0 ~ "None",
      PhysActiveDays <= 2 ~ "Low (1-2)",
      PhysActiveDays <= 5 ~ "Moderate (3-5)",
      PhysActiveDays > 5 ~ "High (>5)"
    ), levels = c("None", "Low (1-2)", "Moderate (3-5)", "High (>5)")),
    Sleep_cat = factor(case_when(
      SleepHrsNight < 5 ~ "Very Short (<5)",
      SleepHrsNight < 7 ~ "Short (5-7)",
      SleepHrsNight <= 9 ~ "Normal (7-9)",
      SleepHrsNight > 9 ~ "Long (>9)"
    ), levels = c("Very Short (<5)", "Short (5-7)", "Normal (7-9)", "Long (>9)"))
  )

facet_labels <- c(
  Work_bin = "Working Status",
  Depressed_bin = "Depression",
  Alcohol_bin = "Alcohol Use (12+ Days per Year)",
  Drug_bin = "Any Drug Use",
  PhysActive_cat = "Physical Activity Level",
  Sleep_cat = "Sleep Duration"
)

ui <- fluidPage(
  titlePanel("BMI Distribution Across Lifestyle Factors"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "variable",
        "Select Variable to Display Boxplot For:",
        choices = setNames(names(facet_labels), facet_labels),
        selected = "Work_bin"
      )
    ),
    mainPanel(
      plotOutput("boxplot", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  output$boxplot <- renderPlot({
    var_to_plot <- input$variable
    
    plot_data <- clean_data |>
      select(BMI, all_of(var_to_plot)) |>
      rename(category = all_of(var_to_plot))
    
    ggplot(plot_data, aes(x = category, y = BMI, fill = category)) +
      geom_boxplot(outlier.alpha = 0.3) +
      stat_summary(
        fun = median,
        geom = "text",
        aes(label = round(..y.., 1)),
        position = position_dodge(width = 0.75),
        vjust = -0.5,
        size = 3.5,
        color = "black"
      ) +
      labs(
        title = paste("BMI Distribution by", facet_labels[[var_to_plot]]),
        x = NULL,
        y = "BMI"
      ) +
      theme_economist(base_size = 14) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 25)),
        plot.margin = margin(20, 20, 40, 10)
      )
  })
}

shinyApp(ui, server)
