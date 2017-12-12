#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# load ----
library(shiny)
library(tidyverse)

theme_set(theme_bw(base_size=16))
# data ----
spr <-  read_csv('spr.csv')

# functions ----

f.spr <- function(x, M, F){
  for(i in 2:nrow(spr)){
    x$N_F.0[i] =  x$N_F.0[i-1] - x$N_F.0[i-1] * M
    x$N_F[i] =  x$N_F[i-1] - x$N_F[i-1] * (M + F)}
  x %>% 
    summarise_all(funs(sum)) %>% 
    mutate(spr = N_F / N_F.0)
  
}



# UI ----
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Age-based SPR"),
  
  sidebarLayout(
    sidebarPanel(sliderInput("age", "Max Age", min = 0, max = max(spr$age) + 5,
                             value = max(spr$age)),
                 sliderInput("F", "F level", min = 0, max = 0.2, value = 0.08),
                 sliderInput("M", "M level", min = 0, max = 0.2, value = 0.08),
                 radioButtons("mature", "Maturity type",
                              list("Data" = "data",
                                   "Literature" = "literature"))),
    mainPanel(
      tabsetPanel(
        tabPanel("SPR plot", plotOutput("plot2", height = 600, width = 600)),
        tabPanel("Abundance", plotOutput("plot", height = 600, width = 600))
        
      )
  )
  )
)


server <- function(input, output) {
  
  # figure 1 ----
  filtered <- reactive({
    filter(spr, age <= input$age) -> spr
  
      for(i in 2:length(spr$age)){
        spr$N_F.0[i] =  spr$N_F.0[i-1] - spr$N_F.0[i-1] * input$M
        spr$N_F[i] =  spr$N_F[i-1] - spr$N_F[i-1] * (input$M + input$F) 
      }
    spr
  })
  
  output$plot <- renderPlot({
    
    ggplot(filtered(), aes(age, N_F.0)) + geom_point() +
      geom_line(aes(age, N_F)) +
      xlab("Age") + ylab("Abundance (#'s)")
  })
  
  # figure 2 ----
  filtered2 <- reactive({
    filter(spr, age <= input$age)  %>% 
       mutate(N_F1 = 1000,
             N_F2 = 1000,
             N_F3 = 1000,
             N_F4 = 1000,
             N_F5 = 1000,
             N_F6 = 1000,
             N_F7 = 1000,
             N_F8 = 1000,
             N_F9 = 1000,
             N_F10 = 1000,
             N_cur = 1000) -> spr
    
    for(i in 2:length(spr$age)){
      spr$N_F.0[i] =  spr$N_F.0[i-1] - spr$N_F.0[i-1] * input$M
      spr$N_F1[i] =  spr$N_F1[i-1] - spr$N_F1[i-1] * (input$M + 0.02)
      spr$N_F2[i] =  spr$N_F2[i-1] - spr$N_F2[i-1] * (input$M + 0.04)
      spr$N_F3[i] =  spr$N_F3[i-1] - spr$N_F3[i-1] * (input$M + 0.06)
      spr$N_F4[i] =  spr$N_F4[i-1] - spr$N_F4[i-1] * (input$M + 0.08)
      spr$N_F5[i] =  spr$N_F5[i-1] - spr$N_F5[i-1] * (input$M + 0.10)
      spr$N_F6[i] =  spr$N_F6[i-1] - spr$N_F6[i-1] * (input$M + 0.12)
      spr$N_F7[i] =  spr$N_F7[i-1] - spr$N_F7[i-1] * (input$M + 0.14)
      spr$N_F8[i] =  spr$N_F8[i-1] - spr$N_F8[i-1] * (input$M + 0.16)
      spr$N_F9[i] =  spr$N_F9[i-1] - spr$N_F9[i-1] * (input$M + 0.18)
      spr$N_F10[i] =  spr$N_F10[i-1] - spr$N_F10[i-1] * (input$M + 0.20)
      spr$N_cur[i] = spr$N_cur[i-1] - spr$N_cur[i-1] * (input$M + input$F)
    } 
    
      spr %>% 
        summarise_all(funs(sum)) %>% 
         mutate(spr1 = N_F1 / N_F.0,
               spr2 = N_F2 / N_F.0,
               spr3 = N_F3 / N_F.0,
               spr4 = N_F4 / N_F.0,
               spr5 = N_F5 / N_F.0,
               spr6 = N_F6 / N_F.0,
               spr7 = N_F7 / N_F.0,
               spr8 = N_F8 / N_F.0,
               spr9 = N_F9 / N_F.0,
               spr10 = N_F10 / N_F.0,
               sprcur = N_cur / N_F.0)  %>% 
        gather(model, value, -age, -weight, -maturity, -N_F, -N_F.0, -N_F1,-N_F2,-N_F3,-N_F4,-N_F5,-N_F6,-N_F7,-N_F8, -N_F9,-N_F10, -N_cur) %>% 
        mutate(F = c(seq(0.02, 0.20, by = 0.02), input$F)) -> out
      out
   })
  
  output$plot2 <- renderPlot({
    
    ggplot(filtered2(), aes(F, value)) + 
      geom_line() + 
      ylab ('SPR') + xlab("Fishing mortality F") + 
      geom_point(data = filter(filtered2(), model=='sprcur'), aes(input$F, value), color = 4, size = 4) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1))
  
  })
}
shinyApp(ui = ui, server = server)
