source('helper_functions.R')
library(shiny)

shinyServer(function(input, output) {
	#Do things only if the button is clicked
    observeEvent(input$submit ,{
		#Initialize message and chart flag variables
		pmsg <- paste(" ")
		pgood <- 0

		#Get the Twitter handle from the text field
		twitterHandle <- input$variable
		#Get the number of characters in the Twitter handle
		handleLen <- nchar(twitterHandle)
		
		#Create a new Progress object
		progress <- shiny::Progress$new()		
		#Initialize the progress object
		progress$set(message = "Status:", value = 0)
		progress$inc(1/5, detail = paste("downloading tweets & analyzing..."))
		  #Get probability
		  #First check if the input field was left empty. If not (>0), run happyorsad. If yes (else), display no input error.
		if(handleLen > 0){
			#Error: Twitter handle too long
			if(handleLen > 15){
				pmsg <- paste("Could not process: ", twitterHandle, " is not a correctly formatted Twitter handle (more than 15 characters). Please try again.")
				progress$inc(5/5, detail = paste("Aborted. Invalid input..."))
				Sys.sleep(1)
			}
			#First error: Twitter account is private
			else if(happyorsad(twitterHandle) == "err1"){
				pmsg <- paste("Could not process: ", twitterHandle," is a private Twitter handle. Please try again.")
				progress$inc(5/5, detail = paste("Aborted. Invalid input..."))
				Sys.sleep(1)
			}
			#Second error: Twitter account does not exist
			else if(happyorsad(twitterHandle) == "err2"){
				pmsg <- paste("Could not process: ", twitterHandle," is not a valid Twitter handle. Please try again.")
				progress$inc(5/5, detail = paste("Aborted. Invalid input..."))
				Sys.sleep(1)
			}
			#No error: display probablity of sadness
			else{
				pmsg <- paste (twitterHandle,"'s probability of being sad is ", happyorsad(twitterHandle))
				#Set a flag for generating charts
				pgood <- 1
			}
		}
		else{
			#Error: nothing entered
			pmsg <- paste("You did not enter a Twitter handle. Please try again.")
			progress$inc(5/5, detail = paste("Aborted. Invalid input..."))
			Sys.sleep(1)
		}
		output$text <- renderText({pmsg})
		
		#Generate charts only if no errors.
		if(pgood > 0){
			#Increment progress bar by 20%
			progress$inc(1/5, detail = paste("40% done"))
			Sys.sleep(2)
			
			#Get charts and increment progress bar by 20% for first two charts.
			ch0 <- plotdatamaker(twitterHandle)
			progress$inc(1/5, detail = paste("60% done"))
			ch1 <- plotdatamaker2(twitterHandle)
			progress$inc(1/5, detail = paste("80% done"))
			ch2 <- plotdatamaker3(twitterHandle)
		
			#Render charts
			output$img1 <- renderImage({list(src = "www/pic1.png", contentType = "image/png", height=125, width=338)},deleteFile = FALSE)
			output$chart <- renderPlot({ch0}, height=250, width=675)
			output$img2 <- renderImage({list(src = "www/pic3.png", contentType = "image/png", height=125, width=338)},deleteFile = FALSE)
			output$chart1 <- renderPlot({ch1}, height=250, width=675)
			output$img3 <- renderImage({list(src = "www/pic4.png", contentType = "image/png", height=125, width=338)},deleteFile = FALSE)
			output$chart2 <- renderPlot({ch2}, height=250, width=675)
			pgood <- 0
			Sys.sleep(1)
			#Increment progress bar to complete
			progress$inc(1/5, detail = paste("Finished!"))
			Sys.sleep(2)
		}
		#Make sure progress bar object is destroyed once reactive observeEvent is complete
		on.exit(progress$close())
	})
})