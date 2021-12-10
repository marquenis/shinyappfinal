
library(shiny)
library(tidyverse)

#Data load and tidy
togoi <- read_csv(here::here("togoi.csv"))

togoi$Larva.tmt <- as.factor(
  str_c(togoi$Larva.Temp.C, " ", togoi$Larva.Food))
togoi$Adult.tmt <- as.factor(
  str_c(togoi$Adult.Temp.C, " ", togoi$Adult.Food))

togoi <- togoi %>%
  filter(wing.length != "NA") %>%
  select("Larva.tmt", "sex", "wing.length", "adult.survival.days") 

togoi$adult.survival.days[is.na(togoi$adult.survival.days)] = 31  



options(shiny.autoreload = TRUE)


ui <- fluidPage(
  titlePanel("Mosquito body size analyses"),
  tags$small("Markus Thormeyer, Phd Student, UBC Zoology"),
  tags$br(),
  tags$br(),
  "This app will help you compare how different larval growth treatments, 
  through manipulation of temperature and nutrition, affect adult body size and 
  lifespan. Adult lifespan is measured by days. With this app, you can explore
  larval treatments affect body size, and how ",
  "The number corresponds to the rearing temperature recorded as degrees 
  Celsius, and the 'Low', 'Med', 'High', correspond to relative nutrition 
  levels.",
  tags$br(),
  tags$br(),
  "Please start by selecting any of the following treatments below. The checkboxes
  affect both plots, but the slider only affects the histogram.",
  tags$br(),
  tags$br(),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "my_checkbox_larva.tmt", "Select Larva Treatment Type",
        choices = unique(togoi$Larva.tmt)
      ),
      strong("Boxplot sex differentiation"),
      checkboxInput(
        "sex_checkbox", "Click for plot sex differentiation", 
        value = FALSE),
      sliderInput("wing.length", "Select Mosquito Wing Lengths (cm)", 
                  min = 2.055, max = 4.071, value = c(2.055, 4.071)),
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
      column(width = 7, plotOutput("my_plot")),
      column(width = 4, tableOutput("my_table"))),
      tags$br(),
      tags$br(),
      plotOutput("survival_plot")
    )
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    req(input$my_checkbox_larva.tmt)
    togoi %>%
      filter(Larva.tmt == input$my_checkbox_larva.tmt)
  })
  
  filtered.size <- reactive({
    req(input$my_checkbox_larva.tmt)
    togoi %>%
      filter(wing.length < input$wing.length[2],
             wing.length > input$wing.length[1],
             Larva.tmt == input$my_checkbox_larva.tmt)
  })

  output$my_plot <- renderPlot(
    if(input$sex_checkbox == TRUE){
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

  output$survival_plot <- renderPlot(
    if(input$sex_checkbox == TRUE){
      filtered.size() %>%
        ggplot(aes(adult.survival.days))+
        geom_histogram()+
        facet_wrap(vars(sex))+
        xlab("Adult Lifespan (days)")+
        ggtitle("Mosquito Adult Lifespan Histogram")+
        ylab("Number of mosquitoes")+
        xlim(0,31)
    }else{
      filtered.size() %>%
        ggplot(aes(adult.survival.days))+
        geom_histogram()+
        ggtitle("Mosquito Adult Lifespan Histogram")+
        xlab("Adult Lifespan (days)")+
        ylab("Number of mosquitoes")+
        xlim(0,31)
    }
  )

  output$my_table <- renderTable(
    filtered() %>%
      group_by(Larva.tmt, sex) %>%
      summarize("Mean (cm)" = mean(wing.length),
                "Median (cm)" = median(wing.length),
                "SD (cm)" = sd(wing.length))
    
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mosquito_bodysize_data", ".csv", sep="")},
    content = function(file){
      data <- filtered()%>%
        group_by(Larva.tmt, sex) %>%
        summarize("Mean (cm)" = mean(wing.length),
                  "Median (cm)" = median(wing.length),
                  "SD (cm)" = sd(wing.length))
      write.csv(data, file)}
  )
  
  output$downloadPlot <- downloadHandler(
    filename <- function() {
      paste('mosquito_bodysize_plot', 'png', sep = ".")},
    content <- function(file){
      png(file)
      
      mosplot.size <- if(input$sex_checkbox == TRUE){
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
      
      mosplot.hist <- if(input$sex_checkbox == TRUE){
        if(input$sex_checkbox == TRUE){
          filtered.size() %>%
            ggplot(aes(adult.survival.days))+
            geom_histogram()+
            facet_wrap(vars(sex))+
            xlab("Adult Lifespan (days)")+
            ggtitle("Mosquito Adult Lifespan Histogram")+
            ylab("Number of mosquitoes")+
            xlim(0,31)
        }else{
          filtered.size() %>%
            ggplot(aes(adult.survival.days))+
            geom_histogram()+
            ggtitle("Mosquito Adult Lifespan Histogram")+
            xlab("Adult Lifespan (days)")+
            ylab("Number of mosquitoes")+
            xlim(0,31)
        }
      }
      
      print(mosplot.hist)
      
      dev.off()
    },
    contentType = "image/png"
  )
  
}

shinyApp(ui = ui, server = server)
