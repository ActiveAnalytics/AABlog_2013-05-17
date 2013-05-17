options(stringsAsFactors = F)
require(ggplot2)
require(gridExtra)

# Creating the environment
assign("envPrevData", new.env(), envir = .GlobalEnv)

# Function to get the data
GetLiveData <- function(sSymbol = "GOOG")
{
	sAddress <- paste("http://download.finance.yahoo.com/d/quotes.csv?s=", sSymbol, "&f=nsb2b3v0&e=.csv", sep = "")
	cat("Downloading data from ", sAddress, "\n")
	dfYahooData <- read.table(sAddress, sep = ",", header = F)
	Time <- Sys.time()
	dfYahooData <- cbind(dfYahooData, "Time" = Time)
	names(dfYahooData) <- c("Name", "Symbol", "Ask", "Bid", "Volume", "Time")
	dfYahooData
}

# Function to update the current data
UpdateStockData <- function(sSymbol = "GOOG")
{
	envPrevData <- get("envPrevData", envir = .GlobalEnv, mode = "environment")
	dfCurr <- GetLiveData(sSymbol = sSymbol)
	print(dfCurr)
	try(envPrevData$dfStockData <- rbind.data.frame(envPrevData$dfStockData, dfCurr))
	assign("envPrevData", envPrevData, envir = .GlobalEnv)
	
	invisible()
}

# The plot function
plotChart <- function(){
	envPrevData <- get("envPrevData", envir = .GlobalEnv, mode = "environment")
	dfStockData <- envPrevData$dfStockData
	if(nrow(dfStockData) > 4){
		AskMovement <- factor(sign(c(0, diff(dfStockData$Ask))), levels = c(-1, 0, 1), labels = c("Down", "No Change", "Up"))
		BidMovement <- factor(sign(c(0, diff(dfStockData$Bid))), levels = c(-1, 0, 1), labels = c("Down", "No Change", "Up"))
		VolumeChange <- c(0, diff(dfStockData$Volume))
		dfStockData <- data.frame(dfStockData, AskMovement, BidMovement, VolumeChange)
		
		dfStockData$Mid <- with(dfStockData, .5*(Bid + Ask))
		
		bAPlot <- ggplot(dfStockData, aes(Time, Mid,
		  ymin = Bid, ymax= Ask, colour = AskMovement))
		bAPlot <- bAPlot + geom_linerange(lwd = 1.5) + xlab("") + ylab("Price\n")
		bAPlot <- bAPlot +  theme(legend.position = "top", plot.margin = unit(c(0, .5, -1.5, 0), "lines"), 
			axis.text.y = element_text(angle = 90), axis.text.x = element_blank()) + labs(colour = "Ask Movement") + 
			xlim(range(dfStockData$Time)) + scale_colour_manual(values=c("red", "blue", "green"))
			
		VolPlot <- qplot(y = VolumeChange, x = Time, data=dfStockData, geom="bar", stat = "identity", fill = AskMovement)
		VolPlot <- VolPlot + theme(legend.position = "none", plot.margin = unit(c(0, .5, 0, 0), "lines"), 
			axis.text.y = element_text(angle = 90)) + xlab("\nTime") + ylab("Volume Change\n") +
			xlim(range(dfStockData$Time)) + scale_colour_manual(values=c("red", "blue", "green"))
			
		grid.arrange(bAPlot, VolPlot, nrow = 2, heights = c(1.5, 1))
	}
}


# Running the process
CurrTime <- Sys.time()
while(CurrTime < CurrTime + 60*60){
UpdateStockData("GOOG")
plotChart()
Sys.sleep(30)
}



