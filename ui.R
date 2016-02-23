library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Estimated Aviation Scheme Global Offset Demand"),

  # h2("The current proposal is to offset 100% of emissions above 2020 levels (i.e. not total emissions)."),

 # Emissions growth rates are based on
 #	ICACO's moderate technology growth model.")),

  # Sidebar with a slider input for number of observations
  sidebarPanel(

    checkboxInput("rebase", "Rebase to 2012 emissions", TRUE),

    p("ICAO modeled emissions are higher than actual emissions in 2020.
	This can be taken into account by rebasing to actual 2012 emissions."),

    hr(),

    sliderInput("emissions",
                  "Percentage of excess emissions offset:",
                  min = 0,
                  max = 100,
                  value = 50),
    p("This shows the impact of varying the amount of excess emissions (over 2020) that are offset."),
     
    hr(),

    radioButtons("start", "Start year:",
             c("Start as proposed (2020)" = "proposed",
               "Start early (2019)" = "early",
               "Start late (2021)" = "late")),
    
    p("This shows the impact of varying the proposed start date of the scheme.")
  ), 

  # Show a plot of the generated distribution
  mainPanel(
    # Blurb
    h4("The current proposal is to offset 100% of emissions above 2020 levels (i.e. not total emissions). The data for this 
	model comes from the International Civil Aviation Organization (ICAO) global aviation carbon dioxide emissions 
	projections to 2050 and can be found here:", a("www.icao.int/emissions.",
	href="http://www.icao.int/environmental-protection/GIACC/Giacc-4/CENV_GIACC4_IP1_IP2%20IP3.pdf",target="_blank"),
        " The model  uses scenario 3 - low aircraft technology improvements and moderate operational improvements"),
    
    plotOutput("distPlot", height=350),
    
    br(),
    h4("Disclaimer: various assumputions have been made in this model, which may not provide a reliable guide to the future. 
        There are various risks including, but not limited to: modeling, poltical, technology, economic, and environmental risks."),
    br(), 
    h4("Built using ",span(img(src="http://shiny.rstudio.com/tutorial/lesson2/www/bigorb.png", height = 50, width = 50)))
  )
))
