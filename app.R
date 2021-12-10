
library(shiny)
library(tidyverse)

#Data load and tidy
togoi <- read_csv(here::here("togoi.csv"))

togoi <- togoi %>%
  mutate(Larva.tmt = as.factor(str_c(togoi$Larva.Temp.C, " ", togoi$Larva.Food))) %>%
  select("Larva.tmt", "sex", "wing.length", "adult.survival.days") %>%
  filter(wing.length != "NA") 

options(shiny.autoreload = TRUE)


ui <- fluidPage(
  titlePanel("Mosquito body size analyses"),
  tags$small("Markus Thormeyer, Phd Student, UBC Zoology"),
  tags$br(),
  tags$br(),
  "This app will help you compare how different larval growth treatments, 
  through manipulation of temperature and nutrition, affect adult body size and 
  lifespan. Adult lifespan is measured by days. With this app, you can explore
  how larval treatments affect body size, and how this may affect the lifespan
  of the mosquito.",
  "With the larval treatments, the number corresponds to the rearing temperature 
  recorded as degrees Celsius, and the 'Low', 'Med', 'High', correspond to 
  relative nutrition levels.",
  tags$br(),
  tags$br(),
  "Please start by selecting any of the following treatments below.",
  tags$br(),
  tags$br(),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "my_checkbox_larva.tmt", "Select Larva Treatment Type",
        choices = unique(togoi$Larva.tmt)
      ),
      strong("Sex differentiation"),
      #These check boxes are my first feature, and they determine which larval
      #treatments are included in 
      checkboxInput(
        "sex_checkbox_box", "Click for boxplot sex differentiation", 
        value = FALSE),
      checkboxInput(
        "sex_checkbox_hist", "Click for histogram sex differentiation", 
        value = FALSE),
      #This slider is my second feature, and it determines which group of 
      #mosquitoes, based on size are included in the histogram
      sliderInput("wing.length", "Select Mosquito Wing Lengths (cm)", 
                  min = 2, max = 4.1, value = c(2, 4.1)),
      tags$br(),
      tags$br(),
      downloadButton(outputId = "downloadData", 
                     label = "Download the summary statistics table"),
      tags$br(),
      downloadButton(outputId = "downloadPlot", 
                     label = "Download the boxplot"),
      tags$br(),
      downloadButton(outputId = "downloadHist",
                     label = "Download the histogram")),
    mainPanel(fluidRow(
      #Split the main panel to have both the table and the plot
      column(width = 7, plotOutput("my_plot")),
      column(width = 4, tableOutput("my_table"))),
      tags$br(),
      tags$br(),
      fluidRow(
        column(width = 7, plotOutput("survival_plot")))
    )
  )
)

server <- function(input, output) {
  
  #This reactive expression is for the body size box plot and 
  #summary statistics
  filtered <- reactive({
    req(input$my_checkbox_larva.tmt)
    togoi %>%
      filter(Larva.tmt %in% c(input$my_checkbox_larva.tmt))
  })
  
  #This reactive expression is for the slider, affecting the lifespan histogram
  filtered.size <- reactive({
    req(input$my_checkbox_larva.tmt)
    togoi %>%
      filter(wing.length < input$wing.length[2],
             wing.length > input$wing.length[1]) %>%
      filter(Larva.tmt %in% c(input$my_checkbox_larva.tmt))
  })
  
  #This boxplot is my 3rd feature, and displays the different body sizes of 
  #mosquitoes by larval treatment
  output$my_plot <- renderPlot(
    if(input$sex_checkbox_box == TRUE){
      filtered() %>%
        ggplot(aes(x = Larva.tmt, y = wing.length)) +
        geom_jitter(aes(alpha = 0.6), size =3, width = 0.3)+
        geom_boxplot()+
        theme(legend.position = "none")+
        ylab("Wing Length (cm)")+
        xlab("Larval Treatments")+
        ggtitle("Mosquito Body Size Boxplot")+
        facet_wrap(vars(sex))
    }else{
      filtered() %>%
        ggplot(aes(x = Larva.tmt, y = wing.length)) +
        geom_jitter(aes(alpha = 0.6), size =3, width = 0.3)+
        geom_boxplot()+
        theme(legend.position = "none")+
        labs(title = "Mosquito Wing Length")+
        ylab("Wing Length (cm)")+
        ggtitle("Mosquito Body Size Boxplot")+
        xlab("Larval Treatments")
    }
  )
  
  #This histogram is my 4th feature, and shows the adult lifespan  
  output$survival_plot <- renderPlot(
    if(input$sex_checkbox_hist == TRUE){
      filtered.size() %>%
        ggplot(aes(adult.survival.days))+
        geom_histogram()+
        facet_grid(rows = vars(Larva.tmt), cols = vars(sex))+
        xlab("Adult Lifespan (days)")+
        ggtitle("Mosquito Adult Lifespan Histogram")+
        ylab("Number of mosquitoes")+
        xlim(0,32)
    }else{
      filtered.size() %>%
        ggplot(aes(adult.survival.days))+
        geom_histogram()+
        ggtitle("Mosquito Adult Lifespan Histogram")+
        facet_grid(rows = vars(Larva.tmt))+
        xlab("Adult Lifespan (days)")+
        ylab("Number of mosquitoes")+
        xlim(0,32)
    }
  )
  
  #This table is my 5th feature, and it provides summary statistics for the 
  #body sizes by treatment
  output$my_table <- renderTable({
    if(input$sex_checkbox_box == TRUE){
    filtered() %>%
      group_by(Larva.tmt, sex) %>%
      summarize("Mean (cm)" = mean(wing.length),
                "Median (cm)" = median(wing.length),
                "SD (cm)" = sd(wing.length))
    }else{
    filtered() %>%
      group_by(Larva.tmt) %>%
      summarize("Mean (cm)" = mean(wing.length),
                "Median (cm)" = median(wing.length),
                "SD (cm)" = sd(wing.length))
    }
  })
  
  #My 6th and last feature is the download buttons for plots and the table
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mosquito_bodysize_data", ".csv", sep="")},
    content = function(file){
      if(input$sex_checkbox_box == TRUE){
        data <- filtered() %>%
          group_by(Larva.tmt, sex) %>%
          summarize("Mean (cm)" = mean(wing.length),
                    "Median (cm)" = median(wing.length),
                    "SD (cm)" = sd(wing.length))
      }else{
        data <- filtered() %>%
          group_by(Larva.tmt) %>%
          summarize("Mean (cm)" = mean(wing.length),
                    "Median (cm)" = median(wing.length),
                    "SD (cm)" = sd(wing.length))
      }
      write.csv(data, file)}
  )
  
  output$downloadPlot <- downloadHandler(
    filename <- function() {
      paste('mosquito_bodysize_plot', 'png', sep = ".")},
    content <- function(file){
      png(file)
      
      mosplot.size <- if(input$sex_checkbox_box == TRUE){
        filtered() %>%
          ggplot(aes(x = Larva.tmt, y = wing.length)) +
          geom_jitter(aes(alpha = 0.6), size =3, width = 0.3)+
          geom_boxplot()+
          theme(legend.position = "none")+
          ylab("Wing Length (cm)")+
          xlab("Larval Treatments")+
          ggtitle("Mosquito Body Size Boxplot")+
          facet_wrap(vars(sex))
      }else{
        filtered() %>%
          ggplot(aes(x = Larva.tmt, y = wing.length)) +
          geom_jitter(aes(alpha = 0.6), size =3, width = 0.3)+
          geom_boxplot()+
          theme(legend.position = "none")+
          ylab("Wing Length (cm)")+
          ggtitle("Mosquito Body Size Boxplot")+
          xlab("Larval Treatments")
      }
      
      print(mosplot.size)
      
      dev.off()
    },
    contentType = "image/png"
  )
  
  output$downloadHist <- downloadHandler(
    filename <- function() {
      paste('mosquito_lifespan_histogram', 'png', sep = ".")},
    content <- function(file){
      png(file)
      
      mosplot.hist <- if(input$sex_checkbox_hist == TRUE){
          filtered.size() %>%
            ggplot(aes(adult.survival.days))+
            geom_histogram()+
            facet_wrap(vars(sex))+
            xlab("Adult Lifespan (days)")+
            ggtitle("Mosquito Adult Lifespan Histogram")+
            ylab("Number of mosquitoes")+
            xlim(0,32)
        }else{
          filtered.size() %>%
            ggplot(aes(adult.survival.days))+
            geom_histogram()+
            ggtitle("Mosquito Adult Lifespan Histogram")+
            xlab("Adult Lifespan (days)")+
            ylab("Number of mosquitoes")+
            xlim(0,32)
        }
      
      print(mosplot.hist)
      
      dev.off()
    },
    contentType = "image/png"
  )
  
}

shinyApp(ui = ui, server = server)
