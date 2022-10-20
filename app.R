#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

min_temp <- 0
max_temp <- 100

min_height <- 0
max_height <- 0.01

time_frame <- 500
pop <- c(1, 3, 23)
pop_saved <- c()

# Define UI for application that draws a histogram
ui <- fluidPage(

	titlePanel(
		h1("TPC Simulation", align="center")
	),
  
    fluidRow(
    	column(4,
    		h3("TPC Curve", style="text-align: center"),
        	plotOutput("tpc_plot"),
        	wellPanel(
        		h3("TPC Parameters:", align="center"),
        		fluidRow(
        			column(6,
    					sliderInput("tpc_mean",
    				   			"TPC Mean",
    				   			min = min_temp,
    				   			max = max_temp,
    				   			value = mean(c(min_temp, max_temp))),
    					sliderInput("tpc_height",
    				   			"TPC Height",
    				   			min = min_height,
    				   			max = max_height,
    				   			value = mean(c(min_height, max_height)))
        			),
					column(6,
						   sliderInput("death",
						   			"Death Rate",
						   			min = 0,
						   			max = max_height,
						   			value = 0.0005),
						   sliderInput("area",
						   			"Area Multiplier",
						   			min = 0.01,
						   			max = 0.1,
						   			value = 0.05)
					)
        		)
        	)
    	),
    	
    	column(4, style="border-left: 1px solid black; border-right: 1px solid black;",
			h3("Temperature", align="center"),
			plotOutput("temp_plot"),
			wellPanel(
				h3("Temperature Parameters:", align="center"),
				fluidRow(
				   	column(6,
						sliderInput("temp_base",
						  		"Temp. Baseline",
						  		min = min_temp,
						  		max = max_temp,
						  		value = mean(c(min_temp, max_temp))),
						sliderInput("temp_amp",
						  		"Temp. Amplitude",
						  		min = 0,
						  		max = 1,
						  		value = 0.25)
				   	
					),
					column(6,
						   sliderInput("temp_period",
						   			"Temp. Period",
						   			min = 100,
						   			max = 500,
						   			value = 300),
						   sliderInput("temp_var_std",
						   			"Temp. Var. STD",
						   			min = 0,
						   			max = 30,
						   			value = 10)
						   
					)
				)
			)
    	),
		
		column(4, 
			h3("Simulation", align="center"),
			plotOutput("growth_plot"),
			wellPanel( align="center",
				h3("Simulation Controls:", align="center"),
				actionButton("run", "Run"),
				actionButton("save", "Save"),
				sliderInput("pop_init",
							"Initial Population",
							min = 10,
							max = 100,
							value = 25)
			)
		)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
	pop <- c()
	pop_saved <- c()
	
    output$tpc_plot <- renderPlot({
    	temp <- seq(min_temp, max_temp, 0.5)
    	std <- input$area / (input$tpc_height * sqrt(2 * pi))
    	growth <- input$area * dnorm(temp, mean = input$tpc_mean, sd=std)
    	death <- rep(input$death, length(temp))
    	
    	tpc_data <- data.frame(temp=temp, growth=growth, death=death)
    	
    	ggplot(tpc_data) +
    		geom_line(aes(x=temp, y=death, colour="Death Rate"), size=2) +
    		geom_line(aes(x=temp, y=growth, colour="Growth Rate"), size=2) +
    		coord_cartesian(xlim = c(min_temp, max_temp), ylim = c(min_height, max_height)) +
    		labs(x="Temperature", y="Rate", colour="Rates")
    })
    
    output$temp_plot <- renderPlot({
    	time <- seq(0, time_frame, 1)
    	temp <- input$temp_base * (1 + input$temp_amp * sin((2*pi*time) / input$temp_period))
    	std <- input$temp_var_std;
    	
    	temp_df <- data.frame(time=time, temp=temp)
    	
    	ggplot(temp_df) +
    		geom_ribbon(aes(x=time, ymin=temp-2*std, ymax=temp+2*std), alpha=0.3) +
    		geom_ribbon(aes(x=time, ymin=temp-std, ymax=temp+std), alpha=0.3) +
    		geom_line(aes(x=time, y=temp), size=2) +
    		geom_hline(aes(yintercept=input$temp_base), size=1, alpha=0.5, linetype="dashed") +
    		coord_cartesian(xlim = c(0, time_frame), ylim = c(min_temp, max_temp)) +
    		labs(x="Time", y="Temperature")
    })
    
    draw_growth_plot <- function(pop) {
    	time <- seq(0, time_frame, 1)
    	print(length(pop))
    	print(length(pop_saved))
    	
    	if (length(pop) == 0) {
    		sim_df <- data.frame(time=time)
    		ggplot(sim_df) +
    			geom_blank() +
    			coord_cartesian(xlim = c(0, time_frame), ylim = c(0, 100)) +
    			labs(x="Time", y="Population")
    	} else if(length(pop_saved) == 0) {
    		sim_df <- data.frame(time=time, pop=pop)
    		output$growth_plot <- renderPlot({
    			ggplot(sim_df) +
    				geom_line(aes(x=time, y=pop), size=1) +
    				coord_cartesian(xlim = c(0, time_frame), ylim = c(0, max(100, max(pop)))) +
    				labs(x="Time", y="Population")
    		})
    	} else {
    		sim_df <- data.frame(time=time, pop=pop, pop_saved=pop_saved)
    		output$growth_plot <- renderPlot({
    			ggplot(sim_df) +
    				geom_line(aes(x=time, y=pop_saved), size=1, alpha=0.3, linetype="dashed") +
    				geom_line(aes(x=time, y=pop), size=1) +
    				coord_cartesian(xlim = c(0, time_frame), ylim = c(0, max(100, max(pop), max(pop_saved)))) +
    				labs(x="Time", y="Population")
    		})
    	}
    }
    
    output$growth_plot <- renderPlot({
    	time <- seq(0, time_frame, 1)
		sim_df <- data.frame(time=time)
		draw_growth_plot(c());
    })
    
    observeEvent(input$run, {
    	current_pop <- input$pop_init
    	std <- input$area / (input$tpc_height * sqrt(2 * pi))
    	pop <<- c(current_pop)
    	
    	for(time in 1:time_frame) {
    		temp <- input$temp_base * (1 + input$temp_amp * sin((2*pi*time) / input$temp_period))
    		current_pop <- current_pop + (input$area * dnorm(temp, mean = input$tpc_mean, sd=std) - input$death) * current_pop
    		pop <<- c(pop, current_pop)
    	}
    	
    	print(length(pop))
    	
    	draw_growth_plot(pop);
    })
    
    observeEvent(input$save, {
    	pop_saved <<- pop
    	
    	draw_growth_plot(pop)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
