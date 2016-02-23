library(shiny)

#
# Logic for a very simple model of offset credit demand for the proposed international aviation emissions trading scheme.
# The model is based emissions data and emissions projections from http://www.icao.int/environmental-protection/GIACC/Giacc-4/CENV_GIACC4_IP1_IP2%20IP3.pdf
#

shinyServer(function(input, output) {

#
# Caclulation Variable Declarations
#
    t         <- array (2012:2025)   	   # projection period
    EmNoReb   <- array(dim(t))             # Emissions without rebasing 
    EmReb     <- array(dim(t))             # Rebased emissions (accounts for the difference between reported emissions and modeled) 
    gth       <- array(dim(t))             # Emissions growth rates no rebasing
    rgth      <- array(dim(t))		   # Rebased emission growth rates
    base      <- 2020 - t[1]	           # Base year
    a         <- 689			   # Actural emissions in 2012
    p         <- c(758,0,0,0,872,0,0,0,999,0,0,0,0,1176) # Base data no rebase
    m         <- c(689,0,0,0,793,0,0,0,908,0,0,0,0,1069) # Rebase data

#
# Output Variable Declarations that account for early (2019), proposed (2020), and late (2021) emissions trading start dates
#
    s2019		<- array (2019:2025)
    OffsetsNoReb2019   	<- array(dim(s2019))	# Offset demand
    EmissionsNoReb2019 	<- array(dim(s2019))
    OffsetsReb2019     	<- array(dim(s2019))	# Offset demand
    EmissionsReb2019   	<- array(dim(s2019))

    s2020          	<- array (2020:2025)    
    OffsetsNoReb2020   	<- array(dim(s2020))	# Offset demand
    EmissionsNoReb2020 	<- array(dim(s2020))
    OffsetsReb2020     	<- array(dim(s2020))	# Offset demand
    EmissionsReb2020   	<- array(dim(s2020))

    s2021		<- array (2021:2025)
    OffsetsNoReb2021   	<- array(dim(s2021))	# Offset demand
    EmissionsNoReb2021 	<- array(dim(s2021))
    OffsetsReb2021     	<- array(dim(s2021))	# Offset demand
    EmissionsReb2021   	<- array(dim(s2021))

#
# emission growth rate projections
# 
    for (i in 1:4) {
        gth[i]<-((p[5]/p[1])^(1/(t[5]-t[1])))-1
        rgth[i]<-((m[5]/m[1])^(1/(t[5]-t[1])))-1
    }

    for (i in 5:8) {
       gth[i]<-((p[9]/p[5])^(1/(t[9]-t[5])))-1
       rgth[i]<-((m[9]/m[5])^(1/(t[9]-t[5])))-1
    }

    for (i in 9:14) {
       gth[i]<-((p[14]/p[9])^(1/(t[14]-t[9])))-1
       rgth[i]<-((m[14]/m[9])^(1/(t[14]-t[9])))-1
    }

#
# emissions projections
#
    EmNoReb[1] <- p[1]
    EmReb[1] <- m[1]
    for (i in 2:14) {
       EmNoReb[i] <- EmNoReb[i-1]*(1+gth[i-1])
       EmReb[i] <- EmReb[i-1]*(1+rgth[i-1])
    }

#
# Output emission projections
#
    for (i in 1:dim(s2019)) {
        EmissionsNoReb2019[i] = EmNoReb[base+i-1] 
    	OffsetsNoReb2019[i] <- (EmissionsNoReb2019[i] - EmNoReb[base])
        
	EmissionsReb2019[i] = EmReb[base+i-1] 
    	OffsetsReb2019[i] <- (EmissionsReb2019[i] - EmReb[base])
    }

    for (i in 1:dim(s2020)) {
        EmissionsNoReb2020[i] = EmNoReb[base+i-1] 
    	OffsetsNoReb2020[i] <- (EmissionsNoReb2020[i] - EmNoReb[base])
        
	EmissionsReb2020[i] = EmReb[base+i-1] 
    	OffsetsReb2020[i] <- (EmissionsReb2020[i] - EmReb[base])
    }

    for (i in 1:dim(s2021)) {
        EmissionsNoReb2021[i] = EmNoReb[base+i-1] 
    	OffsetsNoReb2021[i] <- (EmissionsNoReb2021[i] - EmNoReb[base])
        
	EmissionsReb2021[i] = EmReb[base+i-1] 
    	OffsetsReb2021[i] <- (EmissionsReb2021[i] - EmReb[base])
    }

#
# Make outputs reactive on inputs
#
    zr <- reactive ({input$emissions})
    sr <- reactive ({input$rebase})
    tz <- reactive ({input$start})

#
# Display results
#
  output$distPlot <- renderPlot({
   
    x <- input$emissions           # proportion of emissions to be offset
	
    for (i in 1:dim(s2019)) {
        OffsetsNoReb2019[i] <- (x/100) * (OffsetsNoReb2019[i])
        OffsetsReb2019[i] <- (x/100) * (OffsetsReb2019[i])
    }

    for (i in 1:dim(s2020)) {
        OffsetsNoReb2020[i] <- (x/100) * (OffsetsNoReb2020[i])
        OffsetsReb2020[i] <- (x/100) * (OffsetsReb2020[i])
    }
	
	for (i in 1:dim(s2021)) {
        OffsetsNoReb2021[i] <- (x/100) * (OffsetsNoReb2021[i])
        OffsetsReb2021[i] <- (x/100) * (OffsetsReb2021[i])
    }

    if (input$start == "early") {
	if (input$rebase == TRUE) {
	   y <- rbind(EmissionsReb2019, OffsetsReb2019)
	} else {
    	   y <- rbind(EmissionsNoReb2019, OffsetsNoReb2019)
	}
        barplot(y, names.arg=s2019, col=c("blue", "red"), ylim=c(0,1200), xlab="Year",ylab="MtCO2", beside=TRUE)
    }

    if (input$start == "proposed") {
	if (input$rebase == TRUE) {
	    y <- rbind(EmissionsReb2020, OffsetsReb2020)
	} else {
    	    y <- rbind(EmissionsNoReb2020, OffsetsNoReb2020)
	}
        barplot(y, names.arg=s2020, col=c("blue", "red"), ylim=c(0,1200), xlab="Year",ylab="MtCO2", beside=TRUE)
    }

    if (input$start == "late") {
	if (input$rebase == TRUE) {
	    y <- rbind(EmissionsReb2021, OffsetsReb2021)
	} else {
      	    y <- rbind(EmissionsNoReb2021, OffsetsNoReb2021)
	}
        barplot(y, names.arg=s2021, col=c("blue", "red"), ylim=c(0,1200), xlab="Year",ylab="MtCO2", beside=TRUE)
    }
	
    # add a legend 
    legend ("topright", c("Emissions", "Offsets"), fill = c("blue", "red"))
  })

})
