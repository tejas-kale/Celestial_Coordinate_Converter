# A Shiny application is simply a directory containing a user-interface definition, a server script, and any additional data, scripts, or other resources required to support the application

library(shiny)

# Define the UI for RA-DEC format converter
shinyUI(pageWithSidebar(

	# Application title
	headerPanel("RA-DEC format converter"), 

	# Sidebar with controls to select the input file, name the 
	# output file, choose columns, input number of significant 
	# digits to retain, tell whether the output table should 
	# have column names and, if the output is in time format,
	# the type to save	
	sidebarPanel(
		tags$head (
			tags$style(type="text/css", "label.radio { display: inline-block; padding-left: 20px; }", ".radio input[type=\"radio\"] { float: none; vertical-align: baseline; margin-top: -5px; }"),
			tags$style(type="text/css", "select { max-width: 200px; }"),
        		tags$style(type="text/css", "textarea { max-width: 185px; }"),
		        tags$style(type="text/css", "text { width: 15px; !important }"),
		        tags$style(type="text/css", ".jslider { max-width: 200px; }"),
		        tags$style(type="text/css", ".well { max-width: 310px; }"),
		        tags$style(type="text/css", ".span4 { max-width: 310px; }"),
			tags$style(type="text/css", "h4 {background-color: #E9D9C8; }")
		),

		radioButtons(inputId="conv_num", label="How many values do you wish to convert?", choices=list("Single"="single", "Multiple"="multiple")),

		tags$hr(),

		uiOutput("ui_widgets")
	),

	mainPanel(
		tabsetPanel(id="tabs",
			tabPanel("Original Data", tableOutput("orig_data")),
			tabPanel("Converted Data", tableOutput("conv_data"))
		)
	)
))	

