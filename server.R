library(shiny)
source("ra_dec_conv_func.R", local=TRUE)
options(shiny.maxRequestSize=30*1024^2)

# Define server logic required to convert RA-DEC values to a particular format
shinyServer(function(input, output, session) {

	output$ui_widgets <- renderUI({
		if(input$conv_num=="single") {
			helpText({
				"Coming soon"
			})
		} else 
		if(input$conv_num=="multiple") {
			list(
				tags$h4("Input information"),
				
				tags$div(title="Please restrict the column delimiter in the input file to: , ; \\t |",
					fileInput("in_file", "File:", accept=c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))),
				tags$div(title="Check/Uncheck box based on whether input data has column names", checkboxInput(inputId="is_header", label="Header?", value=TRUE)),
				tags$hr(),	
				tags$div(title="Select column numbers of RA and DEC values", textInput(inputId="col_sel", label="RA & DEC columns:", value="1,2")),
				
				tags$hr(),
				tags$hr(),

				tags$h4("Output information"),

				tags$div(title="Name of the output file", textInput(inputId="out_file", label="File name:", value="output.txt")),
				tags$hr(),
				tags$div(title="Delimiter between columns in the output file", 
					selectInput("out_sep", "Column delimiter:", 
						list("Comma"=",",
						     "Tab"="\t",
						     "Space"=" ",
						     "Pipe"="|",
						     "Semi-colon"=";"))),
				tags$hr(),
				tags$div(title="Number of significant digits to print in the output file", numericInput(inputId="num_sig_dig", label="Significant digits:", value=4)),
				tags$hr(),
				tags$div(title="Check/Uncheck box based on whether output columns should be named", checkboxInput("col_names", "Print column names?", FALSE)),
				tags$hr(),
				tags$div(title="Type of 'time' output required", 
					selectInput("out_form", "Format for 'time' output:",
						list("Hours:Minutes:Seconds"="hms",
						     "Colon"="colon",
						     "Space"="space"))),
				br(),
				actionButton(inputId="convert_data", label="Convert Data"),

				tags$hr(),
				tags$hr(),
		
				tags$div(title="Download data"),
				tags$h4("Download Converted Data"),

				downloadButton("down_conv_data", "Download")
			)
			
		}
	})

	
	output$orig_data <- renderTable({

		# input$in_file will be NULL initially. After the user selects and uploads a
		# file, it will be a data frame with 'name', 'size', 'type' and 'datapath'
		# columns. The 'datapath' column will contain the local filenames where the 
		# data can be found.
		if(is.null(input$in_file))
			return(NULL)

		read_data(input$in_file$datapath, input$is_header, TRUE)
	})

	ra_dec_conv_data <- reactive({
		if(input$convert_data==0)
			return(NULL)
		isolate({
			ra_dec_conv_func(input$in_file$datapath, input$is_header, input$col_sel, input$out_file, input$out_sep, input$num_sig_dig, input$col_names, input$out_form)
		})
	})

	output$conv_data <- renderTable({
		if(is.null(input$in_file))
			return(NULL)
		head(ra_dec_conv_data(), 50)
	})

	observe({
		if(is.null(input$convert_data) || input$convert_data==0)
			updateTabsetPanel(session, "tabs", selected="Original Data")
		else
			updateTabsetPanel(session, "tabs", selected="Converted Data")
	})

	observe({
		input$in_file$name
		updateTabsetPanel(session, "tabs", selected="Original Data")
	})

	output$down_conv_data <- downloadHandler(
		filename = function() {input$out_file},
		content = function(file) {
			write.table(ra_dec_conv_data(), file, sep=input$out_sep, row.names=FALSE, col.names=input$col_names, quote=FALSE)
		}
	)
})
