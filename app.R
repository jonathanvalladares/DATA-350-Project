#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

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
      "Substance Use",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "substance_choice",
            "Choose Substance(s):",
            choices = c("Alcohol" = "Alcohol12PlusYr", "Marijuana" = "Marijuana", "Hard Drugs" = "HardDrugs"),
            selected = "Alcohol"
          ),
          radioButtons(
            "position_choice",
            "Bar Position:",
            choices = c("stack", "fill", "dodge"),
            selected = "stack"
          ),
          sliderInput(
            "text_size",
            "Label Size:",
            min = 2, max = 6, value = 4
          )
        ),
        mainPanel(plotOutput("sub_plot"))
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
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #Jonathon's Code
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
  
  output$plot <- renderPlot({
    xlab <- paste(axis_labels[input$var])
    title <- paste("Distribution, Mean, and Trends of ", axis_labels[input$var], " vs. BMI")
    plot <- ggplot(N %>% tidyr::drop_na(input$var), aes(x = .data[[input$var]], y = BMI, fill = .data[[input$var]])) + 
      geom_violin() + 
      stat_summary(fun.data = function(y) {return(data.frame(y = mean(y), ymin = quantile(y, 0.25), ymax = quantile(y, 0.75)))}, geom = "errorbar", width = 0.1, position=position_dodge(width=0.9)) +
      stat_summary(fun = mean, mapping = aes(group = input$var), position=position_dodge(width=0.9), geom = "point", size = 3, show.legend = F) +
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
  
  axis_labels_sum <- c(
    Hypertension = "Hypertension",
    Diabetes = "Diabetes",
    Cholesterol = "Cholesterol Levels",
    Depressed = "Days Depressed",
    Gender = "Gender",
    Race1 = "Race",
    HHIncomeMid = "Income",
    AgeGroup = "Age")
  
  N_reactive_sum <- eventReactive(input$button, {N %>% pull(input$sum) %>% na.omit()})
  N_reactive_plot <- eventReactive(input$button, {
    xlab <- paste(axis_labels_sum[input$sum])
    title <- paste("Distribution of ", axis_labels_sum[input$sum], " Across NHANES Survey")
    ggplot(data = N, aes(x = as.factor(.data[[input$sum]]))) +
      geom_bar(data = N %>% tidyr::drop_na(input$sum), stat = "count") +
      labs(title = title, y = "Number of Responents", x = xlab, caption = "Data obtained from the NHANES::NHANESraw dataset") +
      theme(
        plot.title = element_markdown(family = "sans", face = "bold", size = 20),
        plot.caption = element_markdown(family = "sans", size = 10),
        legend.title = element_markdown(family = "sans", face = "bold", size = 15),
        legend.text = element_markdown(family = "sans", size = 12),
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
  
  output$sub_plot <- renderPlot({
    
    df <- nhanes_sub_labels |>
      filter(Substance %in% input$substance_choice)
    
    
    text_position <- switch(
      input$position_choice,
      "stack" = position_stack(vjust = 0.5),
      "fill"  = position_fill(vjust = 0.5),
      "dodge" = position_dodge(width = 0.9)
    )
    
    ggplot(df, aes(x = Gender, y = count, fill = Used)) +
      geom_bar(
        stat = "identity",
        position = input$position_choice
      ) +
      geom_text(
        aes(label = label),
        position = text_position,
        size = input$text_size
      ) +
      facet_wrap(~Substance) +
      scale_fill_manual(values = c("No" = "#0072B2", "Yes" = "#D55E00")) +
      labs(
        title = "Substance Use by Gender",
        x = "Gender",
        y = "Number of Respondents",
        fill = "Used"
      ) +
      theme_economist(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0, face = "bold"),
        strip.text = element_text(face = "bold"),
        legend.position = "top"
      )
  })
  
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
        x = xlab,
        y = "Mean BMI",
        fill = g1
      ) +
      theme_economist(base_size = 14) +
      theme(legend.position = "right",)
    
    if (g2 != "None") {
      p <- p + facet_wrap(as.formula(paste("~", g2)), strip.position = "bottom") + 
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank())
    }
    
    if (input$bmi_add_smooth) {
      p <- p + geom_smooth(mapping = aes(y = mean_BMI, group = g1), se = FALSE, method = "lm", color = "black")
    }
    
    p
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
