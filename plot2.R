plot2 <- function(){
  
  NEI <- readRDS("summarySCC_PM25.rds")
  
  baltimore <- subset(NEI, fips==24510)
  sums <- tapply(baltimore$Emissions,baltimore$year,sum)
  years <- names(sums)
  data <- data.frame(Emissions=sums,Year=years)
  data$Year <- as.character(data$Year)
  data$Emissions <- as.numeric(data$Emissions)
  
  par(mar=c(4,4,2,2))
  
  png("plot2.png",480,480)
  
  with(data, plot(Year, Emissions, type="o"
                  ,main="Total Emissions in Baltimore City from 1999 to 2008"
                  ,xlim=c(1998,2009), ylim=c(1500,3500)
                  ,ylab="Emissions, tons(in blue)"
  )
  )
  
  with(data, text(Year,Emissions
                  ,prettyNum(Emissions,big.mark=",")
                  ,pos=1,cex=0.6,col="blue"))
  
  with(data, text(Year,Emissions, Year
                  ,pos=3, cex=0.9))    
  
  
  dev.off()
}