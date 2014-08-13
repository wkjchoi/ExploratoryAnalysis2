plot4 <- function(){
  
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  i1 <- grep("[Cc]oal",SCC$Short.Name)
  i2 <- grep("Comb",SCC$Short.Name)
  i <- intersect(i1,i2)
  
  SCCid <- NEI[i,"SCC"]
  data <- NEI[is.element(NEI$SCC,SCCid),]
  
  Emissions <- round(tapply(data$Emissions,data$year,sum),0)
  Year <- c(1999,2002,2005,2008)
  df <- data.frame(Emissions, Year)
  
  par(mar=c(5,5,2,3))
  png("plot4.png",480,480)
  
    with(df, plot(Year,Emissions,type="o",xlim=c(1998,2009),ylim=c(20000,120000)))
        with(df,text(Year,Emissions,prettyNum(Emissions,big.mark=","),pos=1,col="blue",cex=0.6))
        with(df,text(Year,Emissions,Year,pos=3,cex=1))
        title(main="Total Emissions from Coal Combustion-Related Sources from 1999 to 2008",cex.main=1)
  
  dev.off()

}