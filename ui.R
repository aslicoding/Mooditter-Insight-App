source("helper_functions.R")
library(shiny)    

shinyUI(
  fluidPage(theme="bootstrap.css", title="Mooditter: Tracking the Pulse of Mood on Twitter",
           
			#Heading image
            img(src = "pic.png",height=250,width=675, align="center"), align="center",
			#Text format for the input text box label
            tags$style(type='text/css', 'label {font-size: 14px;}', '.recalculating {opacity: 1.0;}'),       
			#Text format for the ouput
			tags$style(type='text/css', 'h2 {color: black;}'),
  
  mainPanel(width = 12, align="center",
		br(),
		textInput("variable", "Enter a Twitter handle without the @ sign", ""),
		actionButton("submit", "check out how your friend has been feeling lately"),
		titlePanel(textOutput("text")),
		br(),
		div(class = "row", align="center", div(imageOutput("img1", width = "100%", height = "110%"), plotOutput("chart1",  width = "100%", height = "125%"), class = "span6")),
		div(class = "row", align="center", div(imageOutput("img2", width = "100%", height = "110%"), plotOutput("chart",  width = "100%", height = "125%"), class = "span6")),
		div(class = "row", align="center", div(imageOutput("img3", width = "100%", height = "110%"), plotOutput("chart2",  width = "100%", height = "125%"), class = "span6"))
	
	)
))


