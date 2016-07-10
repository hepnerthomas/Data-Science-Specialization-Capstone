### Load NGramModel 
source('ShinyModelFinal.R', local = TRUE)

### Load libraries
library(shiny)
library(ggplot2)
library(scales)
library(wordcloud)
library(RColorBrewer)

### Define server logic
shinyServer(function(input, output, clientData) {

  ## TenaciousTerrier Logo:
  output$logo = renderImage({

    filename = normalizePath(file.path('images',
                                          paste('TerrierSpeech3_new.png', sep='')))
      
    ## Return a list containing the filename and alt text
    width  = clientData$output_cloud_width
    height = clientData$output_cloud_height * 0.45 * 1.5
    list(src = filename,
         width = width, 
         height = height, 
         alt = paste("Image number"))
      
  }, deleteFile = FALSE)
  
  ## Top 3 Predictions:
  output$topWords = renderPlot({
    
    ## Create dataframe of top3 next words
    if(input$userText != "") {
      freqTerms = predictNextWord(input$userText)
      freqTerms = freqTerms[1:5, ]
      freqTerms$probs = freqTerms$probs / sum(freqTerms$probs)
      
      rownames(freqTerms) = freqTerms$words
      freq = as.vector(freqTerms$probs, mode = "any")
      names(freq) = freqTerms$words
      colors = c("#800000", "#8B0000", "#A52A2A", "#B22222", "green3")[0:dim(freqTerms)[1]]
      totals = rep(1, dim(freqTerms)[1])
      
      ## Build and Print Plot
      p = ggplot(freqTerms, aes(reorder(names(freq), freq), freq))
      p = p + labs(x = "", y = "", title = "Next Word Predictions") 
      p = p + geom_bar(mapping = aes(reorder(names(freq), freq), totals, fill = freq)
                       , stat = "identity", fill = "grey" , color = "grey"
                       , width = 0.75)
      p = p + geom_bar(mapping = aes(reorder(names(freq), freq), freq, fill = freq)
                       , stat = "identity", fill = colors , color = colors
                       , width = 0.75)
      p = p + coord_flip()
      p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9, color = "black") 
                    , axis.ticks.x = element_blank() 
                    , axis.text.y = element_text(hjust = 1, face = "bold", size = 16, color = "black") 
                    , axis.ticks.y = element_blank()
                    , title = element_text(face = "bold", size = 16, color = "black")
                    , panel.background = element_blank()
                    , panel.grid = element_blank()
                    , plot.margin = unit(c(1, 1, 0.3, 0.3), "lines")
                    , plot.background = element_rect(color = "black", size = 2, fill = "#2fa4e7") 
                    , complete = TRUE)
      p = p + geom_text(mapping = aes(reorder(names(freq), freq), freq), stat = "identity"
                        , label = paste(round(freq*100, 0), "%", sep = "")
                        , size = 6, hjust = -0.4, color = "black") 
      p = p + scale_y_continuous(limits = c(0, 1), labels = NULL) 
      print(p)
    }
    
    ## Build Empty Plot
    else {
      freqTerms = data.frame()  
      p = ggplot()
      p = p + labs(x = "", y = "", title = "Next Word Predictions") 
      p = p + theme(title = element_text(face = "bold", size = 16, color = "black")
                    , panel.background = element_blank()
                    , panel.grid = element_blank()
                    , plot.margin = unit(c(1, 1, 0.3, 0.3), "lines")
                    , plot.background = element_rect(color = "black", size = 2, fill = "#2fa4e7") 
                    , complete = TRUE)
      print(p)
    }
    
  })
  
  ### Model Accuracy 
  output$accuracy = renderPlot({
   
    ## Check Model Accuracy for Each Successive Word
    accuracyTerms = NULL
    if(input$userText != "") {
      accuracyTerms = checkAccuracy(input$userText)
    }
    else {
      accuracyTerms = data.frame()
    }
    colors = "green3"
    pcts = as.vector(accuracyTerms$accuracy, mode = "any")
    totals = rep(1, dim(accuracyTerms)[1])
    x = factor(rev(rownames(accuracyTerms)), levels = rev(rownames(accuracyTerms)))
    
    ## Build and Print Plot
    p = ggplot(accuracyTerms, aes(x = x))
    p = p + labs(x = "", y = "", title = "Cumulative Model Accuracy") 
    p = p + geom_bar(mapping = aes(x = x, y = totals, totals, fill = totals)
                     , stat = "identity", fill = "grey" , color = "grey"
                     , width = 0.75)
    p = p + geom_bar(mapping = aes(x = rev(x), y = pcts, pcts, fill = pcts)
                     , stat = "identity", fill = colors , color = colors
                     , width = 0.75)
    p = p + coord_flip()
    p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9, color = "black") 
                  , axis.ticks.x = element_blank() 
                  , axis.text.y = element_text(hjust = 1, face = "bold", size = 16, color = "black") 
                  , axis.ticks.y = element_blank()
                  , title = element_text(face = "bold", size = 16, color = "black")
                  , panel.background = element_blank()
                  , panel.grid = element_blank()
                  , plot.margin = unit(c(1, 1, 0.3, 0.3), "lines")
                  , plot.background = element_rect(color = "black", size = 2, fill = "#2fa4e7") 
                  , complete = TRUE)
    p = p + geom_text(mapping = aes(x = rev(x), y = pcts, totals), stat = "identity"
                      , label = paste(round(pcts*100, 0), "%", sep = "")
                      , size = 6, hjust = -0.4, color = "black") 
    p = p + scale_y_continuous(limits = c(0, 1), labels = NULL) 
    print(p)


    
  })
  
  ### Generate Word Cloud 
  output$cloud = renderPlot({
    
    if(input$userText != "") {
      freqTerms = predictNextWord(input$userText)
      mu = mean(freqTerms$probs)
      sigma = sd(freqTerms$probs)
      freqTerms$probs = round((((freqTerms$probs - mu) / sigma) + 1.0)*10, 0)
      pal2 = brewer.pal(8,"Dark2")
      wordcloud(freqTerms$words, freqTerms$probs, scale = c(8, 0.2)
                , min.freq = 1, random.order = FALSE, rot.per = 0.15, colors = pal2)
    }
    
  })
  
})