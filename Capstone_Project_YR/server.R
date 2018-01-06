library(shiny)
library(tm)
library(SnowballC)
library(wordcloud)
library(RWeka)
source("predictioncode.R")
load("UniGram.RData", envir = .GlobalEnv)
load("BiGram.RData", envir = .GlobalEnv)
load("TriGram.RData", envir = .GlobalEnv)
load("QuadGram.RData", envir = .GlobalEnv)
load("QuintGram.RData", envir = .GlobalEnv)

shinyServer(function(input, output) {
        
        result <- reactive({
                prediction(input$text)
                })
        
        output$option1 <- renderText({as.character(result()[1, "Pred"])})
        output$option2 <- renderText({as.character(result()[2, "Pred"])})
        output$option3 <- renderText({as.character(result()[3, "Pred"])})
        output$option4 <- renderText({as.character(result()[4, "Pred"])})
        output$option5 <- renderText({as.character(result()[5, "Pred"])})
        })



