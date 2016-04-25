library(shiny)
shinyUI(pageWithSidebar(
    headerPanel(h2("Data Science Capstone - next word prediction")),
    sidebarPanel(
        p('Next word prediction is widely used in smartphones, tablets and search engines.'),
        p("This Shiny app implements a simply next word prediction tool based on Interpolated Kneser-Ney Smoothing method. Necessary data and input may take 10-20 seconds to load for the first trial, your patience is much appreciated."),
        p("When you type in a few words, this Shiny app will produce at most 3 most likely next words, sorted by probability, separated by comma(,)."),
        textInput(inputId="input_text",label="Your typed input"),
        #Don't calculate unless user hit Go! button
        actionButton("goButton", "Go!")
    ),
    mainPanel(
        #An image of BMI chart
#        img(src="bmichartcalculator.jpg", height = 200, width = 500),
#        p("Above image from squarespace.com"),
        #Display results
        p('You entered text:'),
        verbatimTextOutput('otext'),
        p('The predicted next word(s)'),
        verbatimTextOutput('next_word')
    )
))
