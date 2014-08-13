plot1 <- function(){
  
    NEI <- readRDS("summarySCC_PM25.rds")
    
    sums <- tapply(NEI$Emissions,NEI$year,sum)
    years <- names(sums)
    data <- data.frame(Emissions=sums,Year=years)
    data$Year <- as.character(data$Year)
    data$Emissions <- as.numeric(data$Emissions)
    
    par(mar=c(4,4,2,2))
    
    png("plot1.png",480,480)
  
        with(data, plot(Year, Emissions, type="o"
                        ,main="Total Emissions in US from 1999 to 2008"
                        ,xlim=c(1998,2009), ylim=c(3000000,8000000)
                        ,ylab="Emissions, tons(in blue)"
                        )
             )
      
        with(data, text(Year,Emissions, prettyNum(Emissions,big.mark=","),pos=1,cex=0.6,col="blue"))
        with(data, text(Year,Emissions, Year, pos=3, cex=0.9))    
        
    
    dev.off()
}