plot3 <- function(){

  library(ggplot2)
  
  NEI <- readRDS("summarySCC_PM25.rds")

  baltimore <- subset(NEI, fips==24510)
  data <- transform(baltimore, type.year=paste(type,year,sep="-"))
  
  Emissions <- with(data, tapply(Emissions,type.year,sum))
  Year <- rep(c(1999,2002,2005,2008),4)
  Type <- gl(4,4,labels=c("Non-Road","Non-Point","On-Road","Point"))
  
  d <- data.frame(Emissions,Year,Type)  

  png("plot3.png",480,480)

  p <- qplot(Year,Emissions,data=d,col=Type) 
  p <- p + labs(title="Emissions by Type in Baltimore City") 
  p <- p + geom_line()
  
  print(p)
  
  dev.off()
  
}