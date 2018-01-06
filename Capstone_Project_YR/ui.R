library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("darkly"),
                            
        titlePanel("Capstone Project - Word Prediction"),
        hr(),
        fluidRow(HTML("<div style='margin-left:20px;margin-bottom:5px;color:white;'>Directions:</div>")),
        fluidRow(HTML("<div style='margin-left:50px;margin-bottom:5px;color:white;'>1) Enter a phrase in the text box.</div>")),
        fluidRow(HTML("<div style='margin-left:50px;margin-bottom:5px;color:white;'>2) Hit the submit button.</div>")),
        fluidRow(HTML("<div style='margin-left:50px;margin-bottom:5px;color:white;'>3) Wait for algorithm to run.</div>")),
        hr(),
        sidebarLayout(
                sidebarPanel(
                        textInput("text", "Enter text here:", value = "How are you"),
                        submitButton("Predict Next Word"),
                        h6(em("Note: Program only accepts the English language, and will ignore numbers, punctuation and special characters."))
                ),
                mainPanel(
                        h4("Top Predicted Word:"),
                        tags$ul(
                                tags$li(tags$span(style="color:lightgreen",
                                  tags$strong(textOutput("option1"))))),
                        hr(),
                        h4("Other Word Suggestions:"),
                        tags$ul(
                                tags$li(textOutput("option2")),
                                tags$li(textOutput("option3")),
                                tags$li(textOutput("option4")),
                                tags$li(textOutput("option5"))
                        )
                        
                )
        ),
        hr(),
        fluidRow(HTML("<div style='margin-left:20px;margin-bottom:5px;color:white;'>Developed by:
                      <a href='https://www.linkedin.com/in/yonatanrafael/'>Yonatan Rafael</a></div>")),
        hr(),
        fluidRow(HTML("<div style='margin-left:20px;margin-bottom:5px;color:white;'>Other Resources:</div>")),
        fluidRow(HTML("<div style='margin-left:50px;margin-bottom:5px;;color:white;'>
                      <a href='https://www.coursera.org/specializations/jhu-data-science'>Presentation Slides</a></div>")),
        fluidRow(HTML("<div style='margin-left:50px;margin-bottom:5px;;color:white;'>
                      <a href='https://www.coursera.org/specializations/jhu-data-science'>Coursera Data Science Specialization Course</a></div>"))
)
)