
library(shiny)
library(tidyverse)

#Data load and tidy
togoi <- read_csv(here::here("togoi.csv"))

togoi$Larva.tmt <- as.factor(
  str_c(togoi$Larva.Temp.C, " ", togoi$Larva.Food))
togoi$Adult.tmt <- as.factor(
  str_c(togoi$Adult.Temp.C, " ", togoi$Adult.Food))

togoi.size <- togoi %>%
  filter(wing.length != "NA") %>%
  select("Larva.tmt", "sex", "wing.length") 

togoi.survival <- togoi %>%
  mutate(status = as.numeric(if_else(died.naturally == "yes","1","0"))) %>%
  select("Larva.tmt", "sex", "status", "adult.survival.days") 

togoi.survival$adult.survival.days[is.na(togoi$adult.survival.days)] = 31  



options(shiny.autoreload = TRUE)


ui <- fluidPage(
  titlePanel("Mosquito body size analyses"),
  tags$small("Markus Thormeyer, Phd Student, UBC"),
  tags$br(),
  tags$br(),
  "This app will help you compare how different larval treatments of 
  temperature and nutrition affect adult body size.",
  "The number corresponds to the rearing temperature recorded as degrees 
  Celsius, and the 'Low', 'Med', 'High', correspond to relative nutrition 
  levels",
  tags$br(),
  tags$br(),
  "Please start by selecting any of the following treatments below",
  tags$br(),
  tags$br(),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "my_checkbox_larva.tmt", "Select Larva Treatment Type",
        choices = unique(togoi.size$Larva.tmt)
      ),
      strong("Boxplot sex differentiation"),
      checkboxInput(
        "sex_checkbox", "Click for plot sex differentiation", 
        value = TRUE),
      tags$br(),
      strong("Summary statistics"),
      tableOutput("my_table"),
      tags$br(),
      tags$br(),
      downloadButton(outputId = "downloadData", label = "Download the table"),
      tags$br(),
      downloadButton(outputId = "downloadPlot", label = "Download the plot")),
    mainPanel(
      plotOutput("my_plot"),
      plotOutput("survival_plot")
    )
  )
)

server <- function(input, output) {
  
  #My first feature is this widget which will allow for the selection of 
  #different larval treatment groups for comparison by visual analysis (boxplot),
  #or summary statistics (table). This widget is also capable of splitting the
  #plot into male and female mosquitoes. 
  filtered <- reactive({
    req(input$my_checkbox_larva.tmt)
    togoi.size %>%
      filter(Larva.tmt == input$my_checkbox_larva.tmt)
  })
  
  #My second feature is this plot which displays the different body sizes of
  #mosquitoes grown at different larval temperatures and nutrition contents
  output$my_plot <- renderPlot(
    if(input$sex_checkbox == TRUE){
      filtered() %>%
        ggplot(aes(x = Larva.tmt, y = wing.length)) +
        geom_jitter(aes(alpha = 0.6), size =3, width = 0.3)+
        geom_boxplot()+
        theme(legend.position = "none")+
        ylab("Wing Length (cm)")+
        xlab("Larval Treatments")+
        facet_wrap(vars(sex))
    }else{
      filtered() %>%
        ggplot(aes(x = Larva.tmt, y = wing.length)) +
        geom_jitter(aes(alpha = 0.6), size =3, width = 0.3)+
        geom_boxplot()+
        theme(legend.position = "none")+
        labs(title = "Mosquito Wing Length")+
        ylab("Wing Length (cm)")+
        xlab("Larval Treatments")
    }
  )
  
  #My third feature is this output table which provides some basic stats 
  #results of the selected mosquito growth treatments.
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
          facet_wrap(vars(sex))
      }else{
        filtered() %>%
          ggplot(aes(x = Larva.tmt, y = wing.length)) +
          geom_jitter(aes(alpha = 0.6), size =3, width = 0.3)+
          geom_boxplot()+
          theme(legend.position = "none")+
          ylab("Wing Length (cm)")+
          xlab("Larval Treatments")
      }
      
      print(mosplot.size)
      
      dev.off()
    },
    contentType = "image/png"
  )
  
}

shinyApp(ui = ui, server = server)
