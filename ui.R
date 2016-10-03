# ui.R

shinyUI(fluidPage(
  titlePanel("Clubes de Ciencia Summer 2016 Monterrey"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("PrePost", 
                  label = "Please select from the dropdown menu:",
                  choices = list("Pre and Post Assessment Scores" = 
                              c("Histograms",
                                "Violin Plots",
                                "Box Plots",
                                "Scatter Plot"),
                              "Additional" =
                              c("Gender", 
                                "Club Rating",
                                "Wordcloud - How would you improve the club?",
                                "Wordcloud - How did your attitude towards science change?",
                                "Wordcloud - What do you think of Clubes de Ciencia?")),
                  selected = NULL)
      
      ),
    
    mainPanel(plotOutput("plot"))
  )
))