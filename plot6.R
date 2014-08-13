plot6 <- function(){
  
  
  
  i <- grep("[Vv]ehicle",SCC$Short.Name)
  SCCid <- NEI[i,"SCC"]
  
  data <- subset(NEI, fips=="24510" | fips=="06037")
  data <- data[is.element(data$SCC,SCCid),]
  data <- split(data,data$fips)
  dataB <- data[["24510"]]
  dataC <- data[["06037"]]
  
  Emissions <- round(tapply(dataB$Emissions,dataB$year,sum),0)
  Year <- c(1999,2002,2005,2008)
  dfB <- data.frame(Emissions, Year)
  
  Emissions <- round(tapply(dataC$Emissions,dataC$year,sum),0)
  Year <- c(1999,2002,2005,2008)
  dfC <- data.frame(Emissions, Year)
  
  png("plot6.png",1000,540)
  
  par(mar=c(5,4,2,1))
  par(mfrow=c(1,2))
  par(oma=c(1,1,3.2,1))
  
  with(dfB, plot(Year,Emissions,type="o"
                 ,ylab="Emissions, tons(in blue)")
                 ,xlim=c(1998,2011),ylim=c(400,2800))  
            with(dfB,text(Year,Emissions
                ,prettyNum(Emissions,big.mark=",")
                ,pos=1,col="blue",cex=0.6))
            with(dfB,text(Year,Emissions,Year
                ,pos=3,cex=0.9))
            with(dfB,title(main="Baltimore",cex.main=0.9))
  
  with(dfC, plot(Year,Emissions,type="o"
                 ,ylab="Emissions, tons(in red)")
                 ,xlim=c(1998,2009),ylim=c(9000,30000))
            with(dfC,text(Year,Emissions
                ,prettyNum(Emissions,big.mark=",")
                ,pos=1,col="red",cex=0.6))  
            with(dfC,text(Year,Emissions,Year
                ,pos=3,cex=0.9))
            with(dfB,title(main="California",cex.main=0.9))
  
  title(main="Total Emissions from Motor Vehicle Sources in Baltimore from 1999 to 2008", outer=TRUE)
  
  dev.off()
  
}
