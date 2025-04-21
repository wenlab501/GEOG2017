# 安裝必要套件
# install.packages("shiny")
# install.packages("ggplot2")

library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("中央極限定理模擬教學 (CLT Simulator)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "每次抽樣樣本數:", min = 5, max = 100, value = 30, step = 5),
      sliderInput("rep", "抽樣次數:", min = 10, max = 1000, value = 200, step = 10),
      selectInput("dist", "母體分布型態:", 
                  choices = c("右偏（指數分布）" = "exp", "常態分布" = "norm", "均勻分布" = "unif")),
      actionButton("resample", "重新抽樣")
    ),
    mainPanel(
      plotOutput("populationPlot"),
      plotOutput("samplingDistPlot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  generate_population <- reactive({
    switch(input$dist,
           "exp" = rexp(100000, rate = 1/5),
           "norm" = rnorm(100000, mean = 50, sd = 10),
           "unif" = runif(100000, min = 0, max = 100))
  })
  
  sampled_means <- eventReactive(input$resample, {
    pop <- generate_population()
    replicate(input$rep, mean(sample(pop, input$n)))
  })
  
  output$populationPlot <- renderPlot({
    pop <- generate_population()
    ggplot(data.frame(x = pop), aes(x)) +
      geom_histogram(bins = 50, fill = "orange", color = "black") +
      labs(title = "母體資料分布", x = "數值", y = "頻數")
  })
  
  output$samplingDistPlot <- renderPlot({
    means <- sampled_means()
    ggplot(data.frame(mean = means), aes(mean)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      labs(title = "抽樣平均的分布（CLT）", x = "樣本平均", y = "頻數")
  })
}

# Run app
shinyApp(ui = ui, server = server)
