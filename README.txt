Thomas Hepner, April 2016: 

Project code for the Data Science Specialization 
Capstone. The code uses an English language corpus
consisting of blogs, news articles, and tweets to build
a predictive text model. 

The final URL is here: https://tenaciousterrier.shinyapps.io/TenaciousTerrierTextPrediction/

The code is split into three main files/functions: 

1. BuildTables: 
	- Loads the corpus
	- Builds N-Grams with R packages

2. ShinyModelFinal: 
	- Creates interpolation model with N-Grams
	- Generates top 5 word predictions with confidence bar

3. ui / server: 
	- Builds Shiny app to implement predictive text model