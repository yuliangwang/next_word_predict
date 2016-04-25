library(shiny)
library(tm)
library(data.table)
load("KN_prob.RData")

predict_next_word<-function(input_text,dt2,dt3,dt4,uni_freq){
    #return 3 most likely results
    max_results<-3
    #preprocessing
    input_text<-removePunctuation(input_text)
    input_text<-removeNumbers(input_text)
    input_text<-tolower(input_text)
    input_text<-unlist(strsplit(input_text," "))
    #select last 3 words, as we only handles up to 4-grams for fast response. 
    if(length(input_text)>3){
        input_text<-input_text[(length(input_text)-2):length(input_text)]
    }
    
    #First try if input matches any 4-grams that begins with the 3 input words, if so, select 3 most probably next words.
    #If not, recursively go to 3-grams and 2-grams. 
    gram4<-dt4[trigram1==paste(input_text,collapse="_"),]
    if (nrow(gram4)==0){
        gram3<-dt3[bigram1==paste(input_text[(length(input_text)-1):length(input_text)],collapse="_"),]
        nrow_gram3<-nrow(gram3)
        if (nrow_gram3!=0){
            next_word<-gram3$end[1:(min(max_results,nrow_gram3))]
        } else {
            gram2<-dt2[unigram1==input_text[length(input_text)],]
            nrow_gram2<-nrow(gram2)
            if (nrow_gram2!=0){
                next_word<-gram2$end[1:(min(max_results,nrow(gram2)))]
            } else {
                next_word<-names(uni_freq)[1]
            }
        }
    } else {
        next_word<-gram4$end[1:(min(max_results,nrow(gram4)))]
    }
    return(paste(next_word,collapse=", "))
}


shinyServer(
    function(input,output) {
        #Display user's input
        output$otext<-renderPrint({as.character(input$input_text)})
        #Predict next words
        dataInput<-reactive({
            if(input$goButton){predict_next_word(input$input_text,dt2,dt3,dt4,uni_freq)}
        })
        #Display results
        output$next_word<-renderPrint({dataInput()})
    }
)
