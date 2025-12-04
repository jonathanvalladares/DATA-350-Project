#Group 14

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(NHANES)
library(scales)
library(paletteer)
library(ggtext)

#Jonathon
N <- NHANES::NHANESraw %>%
  mutate(Hypertension = case_when(BPSysAve >= 130 & BPDiaAve > 80 ~ "Hypertension", BPSysAve < 130 & BPDiaAve <= 80 ~ "Normal Blood Pressure")) %>%
  #from https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/TCHOL_J.htm. Used conversion factor to take normal guidlined into mmol/L
  #Ranges from https://www.hopkinsmedicine.org/health/treatment-tests-and-therapies/lipid-panel#:~:text=Optimal:%20Less%20than%20100%20mg,protect%20you%20against%20heart%20disease.
  mutate(Cholesterol = case_when(TotChol <= 5.172 ~ "Normal", TotChol <= 6.16 ~ "Borderline", TotChol > 6.16 ~ "High")) %>%
  mutate(AgeGroup = cut(Age, breaks = c(0, 20, 40, 60, 80), labels = c("0–20", "21–40", "41–60", "61–80"))) %>%
  select(AgeGroup, Hypertension, Diabetes, BMI, Depressed, Cholesterol, Race1, Gender, HHIncomeMid)
N$Cholesterol <- factor(N$Cholesterol, levels = c("Normal", "Borderline", "High"))
N$Hypertension <- factor(N$Hypertension, levels = c("Normal Blood Pressure", "Hypertension"))

axis_labels <- c(
  Hypertension = "Hypertension",
  Diabetes = "Diabetes",
  Cholesterol = "Cholesterol Levels",
  Depressed = "Days Depressed")

axis_labels_sum <- c(
  Hypertension = "Hypertension",
  Diabetes = "Diabetes",
  Cholesterol = "Cholesterol Levels",
  Depressed = "Days Depressed",
  Gender = "Gender",
  Race1 = "Race",
  HHIncomeMid = "Income",
  AgeGroup = "Age")

#Jon/Jonathan's Code
nhanes_clean <- NHANES::NHANESraw |>
  select(Age, Gender, Race1, BMI, Diabetes,
         Alcohol12PlusYr, Marijuana, HardDrugs,HHIncomeMid) |>
  filter(
    !is.na(Age),
    !is.na(Gender),
    !is.na(Race1),
    !is.na(BMI),
    !is.na(Diabetes)
  ) |>
  mutate(
    Gender   = droplevels(Gender),
    Race1    = droplevels(Race1),
    Diabetes = droplevels(Diabetes),
    AgeGroup = cut(
      Age,
      breaks = c(0, 20, 40, 60, 80),
      labels = c("0–20", "21–40", "41–60", "61–80")
    )
  )


nhanes_sub <- nhanes_clean |>
  pivot_longer(
    cols = c(Alcohol12PlusYr, Marijuana, HardDrugs),
    names_to = "Substance",
    values_to = "Used"
  ) |>
  filter(!is.na(Used))

nhanes_sub_labels <- nhanes_sub |>
  group_by(Substance, Gender, Used) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(Substance, Gender) |>
  mutate(
    pct = count / sum(count),
    label = paste0(count, "\n(", percent(pct), ")")
  ) |>
  ungroup()

demographic_vars <- c("Gender" = "Gender", "Race" = "Race1", "Income" = "HHIncomeMid", "Age" = "AgeGroup")

#Wyatt
clean_data <- NHANESraw |>
  select(Age, Gender, BMI, PhysActiveDays, Work, Depressed, SleepHrsNight, 
         Alcohol12PlusYr, Marijuana, HardDrugs) |>
  filter(!is.na(Age), !is.na(Gender), !is.na(BMI), !is.na(Work), 
         !is.na(Depressed), !is.na(Alcohol12PlusYr), !is.na(Marijuana), 
         !is.na(HardDrugs), !is.na(PhysActiveDays), !is.na(SleepHrsNight)) |>
  mutate(
    Work_bin = factor(ifelse(Work == "Working", "Yes", "No"), levels = c("No", "Yes")),
    Alcohol_bin = factor(ifelse(Alcohol12PlusYr == "Yes", "Yes", "No"), levels = c("No", "Yes")),
    marijuana_bin = factor(ifelse(Marijuana == "Yes", "Yes", "No"), levels = c("No", "Yes")),
    drug_bin = factor(ifelse(HardDrugs == "Yes", "Yes", "No"), levels = c("No", "Yes")),
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
  Alcohol_bin = "Alcohol Use (12+ Days per Year)",
  marijuana_bin = "Marijuana Use",
  drug_bin = "Hard Drug Use",
  PhysActive_cat = "Physical Activity Level",
  Sleep_cat = "Sleep Duration"
)

# Define UI for application that draws a histogram
ui <- navbarPage(
    # Application title
    "Influence of Demographic, Lifestyle, and Health Factors on Obesity",
    tabPanel("Chronic Diseases",
             sidebarLayout(
               sidebarPanel(
                 #Diseases to consider: Depression, Hypertension, Cholesterol, Diabetes
                 radioButtons("var",
                              "Chronic Disease to Plot",
                              choices = c("Hypertension" = "Hypertension", "Diabetes" = "Diabetes", "Cholesterol Levels" = "Cholesterol",  "Depression" = "Depressed"),
                              selected = "Hypertension"),
                 checkboxInput(
                   "lm",
                   "Add Linear Model",
                   value = FALSE
                 )
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 fluidRow(
                   plotOutput("plot")
                 )
               )
             )
    ), 
    
    
    tabPanel(
      "Demographic Factors",
      
      sidebarLayout(
        sidebarPanel(
          
          selectInput(
            "bmi_group1",
            "Primary Demographic Group:",
            choices = demographic_vars,
            selected = "HHIncomeMid"
          ),
          
          selectInput(
            "bmi_group2",
            "Optional Second Group:",
            choices = c("None", demographic_vars),
            selected = "None"
          ),
          
          sliderInput(
            "bmi_alpha",
            "Transparency:",
            min = 0.2, max = 1, value = 1
          ),
          
          checkboxInput(
            "bmi_add_smooth",
            "Add Smoothing Model",
            value = FALSE
          )
        ),
        
        mainPanel(plotOutput("bmi_explorer_plot"))
      )
    ),
    
    tabPanel("Lifestyle Factors",
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
            ),
    tabPanel(
      "Summary",
      sidebarLayout(
        sidebarPanel(
          radioButtons("sum",
                       "Disease to Plot",
                       choices = c("Hypertension" = "Hypertension", "Diabetes" = "Diabetes", "Cholesterol Levels" = "Cholesterol",
                                   "Depression" = "Depressed", "Gender" = "Gender", "Race" = "Race1", "Income" = "HHIncomeMid", "Age" = "AgeGroup"),
                       selected = "Hypertension"),
          actionButton("button", label = "Summarize")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(verbatimTextOutput("summary"),
                   plotOutput("dist")
          )
        )
      )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #Jonathon's Code
  output$plot <- renderPlot({
    xlab <- paste(axis_labels[input$var])
    title <- paste("Distribution, Mean, and Trends of ", axis_labels[input$var], " vs. BMI")
    plot <- ggplot(N %>% tidyr::drop_na(input$var), aes(x = .data[[input$var]], y = BMI, fill = .data[[input$var]])) + 
      geom_violin() + 
      stat_summary(fun.data = function(y) {return(data.frame(y = mean(y), ymin = quantile(y, 0.25), ymax = quantile(y, 0.75)))}, geom = "errorbar", width = 0.1, position=position_dodge(width=0.9)) +
      stat_summary(fun = mean, mapping = aes(group = input$var), position=position_dodge(width=0.9), geom = "point", size = 3, show.legend = F) +
      theme_clean() +
      scale_fill_paletteer_d("soilpalettes::crait") +
      labs(title = title, x = xlab, caption = "Data obtained from the NHANES::NHANESraw dataset") +
      theme(
        plot.title = element_markdown(family = "sans", face = "bold", size = 20),
        plot.caption = element_markdown(family = "sans", size = 10),
        legend.position = "none",
        axis.title.x = element_markdown(family = "sans", face = "bold", size = 15),
        axis.title.y = element_markdown(family = "sans", face = "bold", size = 15),
        axis.text = element_markdown(family = "sans", size = 12),
      )
    if (input$lm) {
      plot <- plot + geom_smooth(mapping = aes(group = input$var), method = "lm", color = "black") 
    }
    plot
  })

  N_reactive_sum <- eventReactive(input$button, {N %>% pull(input$sum) %>% na.omit()})
  N_reactive_plot <- eventReactive(input$button, {
    xlab <- paste(axis_labels_sum[input$sum])
    title <- paste("Distribution of ", axis_labels_sum[input$sum], " Across NHANES Survey")
    ggplot(data = N, aes(x = as.factor(.data[[input$sum]]))) +
      geom_bar(data = N %>% tidyr::drop_na(input$sum), stat = "count", aes(fill = "red"), alpha = 0.8) +
      labs(title = title, y = "Number of Responents", x = xlab, caption = "Data obtained from the NHANES::NHANESraw dataset") +
      theme_clean() + 
      theme(
        plot.title = element_markdown(family = "sans", face = "bold", size = 20),
        plot.caption = element_markdown(family = "sans", size = 10),
        legend.position = "none",
        axis.title.x = element_markdown(family = "sans", face = "bold", size = 15),
        axis.title.y = element_markdown(family = "sans", face = "bold", size = 15),
        axis.text = element_markdown(family = "sans", size = 12),
      )
  })
  
  output$summary <- renderPrint({
    N_reactive_sum() %>% summary()
  })
  
  output$dist <- renderPlot({
    N_reactive_plot()
  })
  
  #Jon/Jonathan's Code
  output$bmi_explorer_plot <- renderPlot({
    
    df <- nhanes_clean
    g1 <- input$bmi_group1
    g2 <- input$bmi_group2
    
    axis_labels_dem <- c(
      Gender = "Gender",
      Race1 = "Race",
      HHIncomeMid = "Income",
      AgeGroup = "Age")
    
    observeEvent(input$bmi_group1, {
      
      
      new_choices <- c("None", demographic_vars[demographic_vars != input$bmi_group1])
      
      
      current_g2 <- input$bmi_group2
      new_selected <- if (current_g2 == input$bmi_group1) "None" else current_g2
      
      updateSelectInput(
        session,
        "bmi_group2",
        choices = new_choices,
        selected = new_selected
      )
    })
    
    group_vars <- c(g1, if (g2 != "None") g2)
    
    summary_df <- df |>
      group_by(across(all_of(group_vars))) |>
      summarize(mean_BMI = mean(BMI, na.rm = TRUE), .groups = "drop")
    
    xlab <- paste(axis_labels_dem[input$bmi_group1])
    p <- ggplot(summary_df, aes(
      x = as.factor(.data[[g1]]),
      y = mean_BMI,
      fill = factor(.data[[g1]])    
    )) +
      geom_col(data = summary_df %>% tidyr::drop_na(), alpha = input$bmi_alpha) +
      labs(
        title = paste("Mean BMI by", g1),
        caption = "Data obtained from the NHANES::NHANESraw dataset",
        x = xlab,
        y = "Mean BMI",
        fill = g1
      ) +
      theme_clean() + 
      theme(
        plot.title = element_markdown(family = "sans", face = "bold", size = 20),
        plot.caption = element_markdown(family = "sans", size = 10),
        legend.title = element_markdown(family = "sans", face = "bold", size = 15),
        legend.text = element_markdown(family = "sans", size = 12),
        legend.position = "right",
        axis.title.x = element_markdown(family = "sans", face = "bold", size = 15),
        axis.title.y = element_markdown(family = "sans", face = "bold", size = 15),
        axis.text = element_markdown(family = "sans", size = 12),
      )
    
    if (g2 != "None") {
      p <- p + facet_wrap(as.formula(paste("~", g2)), strip.position = "bottom") +
        theme(strip.background = element_rect(fill = "white", color = "white"),
              strip.text = element_text(color = "black", face = "bold", size = 12),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank())
    }
    
    if (input$bmi_add_smooth) {
      p <- p + geom_smooth(mapping = aes(y = mean_BMI, group = g1), se = FALSE, method = "lm", color = "black")
    }
    
    p
  })
  
  #Wyatt's Code
  output$boxplot <- renderPlot({
    var_to_plot <- input$variable
    
    plot_data <- clean_data |>
      select(BMI, all_of(var_to_plot)) |>
      rename(category = all_of(var_to_plot))
    
    lifestyle <- ggplot(plot_data, aes(x = category, y = BMI, fill = category)) +
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
        caption = "Data obtained from the NHANES::NHANESraw dataset",
        x = NULL,
        y = "BMI"
      ) +
      theme_clean() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(b = 25)),
        plot.caption = element_markdown(family = "sans", size = 10),
        plot.margin = margin(20, 20, 40, 10)
      )
    lifestyle
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
