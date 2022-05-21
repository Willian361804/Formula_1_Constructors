library(shiny)
library(tidyverse)
library(fmsb)
library(shinythemes)

carros = read.csv("F1.csv",sep=";",header = T)
equipe = unique(carros$Construtor)

##__________________________________________________________________

car = t(carros)
car = as.data.frame(car)
rownames(car) <- c("","Número_GPs","Número_Temporadas","Número_Pilotos",
                   "Vitórias","Poles","Voltas mais rápidas","Pódios")
names(car) <- car[1,]
car <- car[-1,]

for(i in 1:60){
    car[,i] <- sapply(car[,i], as.numeric)
}

##__________________________________________________________________

names(carros) <- c("","Número_GPs","Número_Temporadas","Número_Pilotos",
                   "Vitórias","Poles","Voltas mais rápidas","Pódios")
rownames(carros) <- equipe
carros <- carros[,-1]

a = c(1040,1040,1040,1040,1040,1040,1040)
b = c(0,0,0,0,0,0,0)
carros = rbind(a,b,carros)

for(i in 1:7){
    carros[,i] <- sapply(carros[,i], as.numeric)
}

##___________________________________________________

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
    #theme = shinythemes("nome do tema")
    #themeSelector(),
    #img(src = "logoF1.jpg"),
    # Application title
    titlePanel("60 equipes com mais corridas na Fórmula 1"),
    
    hr(),
    
    fluidRow(
        column(12,
           selectInput("Equipe","Selecione a equipe",choices = equipe),
           actionButton("Processar","Processar")),
        
    ),
    hr(),
    fluidRow(
        column(7,
               h2(textOutput("Eq")), plotOutput("Barras")),
        column(5,plotOutput("Radar"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$Processar,{
        const = input$Equipe
        output$Eq = renderText({const})
        output$Barras = renderPlot({ par(mar=c(4,10,4,4))
                                     barplot(car[,as.character(const)],horiz = T,
                                             names.arg = rownames(car),
                                             col="light blue",las=1,,xaxt="n") 
                                     text(8,barplot(car[,as.character(const)],horiz = T,
                                                    names.arg = rownames(car),
                                                    col="light blue",las=1,,xaxt="n"),
                                          car[,as.character(const)],cex = 1) })
        
        output$Radar = renderPlot({ radarchart(carros[c(1,2,as.character(const)),], 
                                               pcol=rgb(0.2,0.5,0.5,0.9),
                                               pfcol=rgb(0.2,0.5,0.5,0.5),plwd=3,
                                               axistype=1,caxislabels = seq(0,1050,260),
                                               cglcol="grey",
                                               cglty=1,axislabcol="grey",cglwd=0.8,
                                               vlcex=0.8) })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)