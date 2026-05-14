data<-read.csv("vgsales.csv")
pdf("visualization.pdf")

#deleting unused rows
df<-data[!(data$Platform=="2600" | data$Platform=="3DO"| data$Platform=="3DS"| data$Platform=="DC"| data$Platform=="DS"| data$Platform=="GB"| data$Platform=="GBA"| data$Platform=="GC"| data$Platform=="GEN"| data$Platform=="GG"| data$Platform=="N64"| data$Platform=="NES"| data$Platform=="NG"| data$Platform=="PC"| data$Platform=="PCFX"| data$Platform=="SAT"| data$Platform=="SCD"| data$Platform=="SNES"| data$Platform=="TG16"| data$Platform=="Wii"| data$Platform=="WiiU"| data$Platform=="WS"),]


#updating the data with Play Station & XBox
df$Platform[df$Platform == 'PS2'] <- 'PlayStation'
df$Platform[df$Platform == 'PS3'] <- 'PlayStation'
df$Platform[df$Platform == 'PS4'] <- 'PlayStation'
df$Platform[df$Platform == 'PSV'] <- 'PlayStation'
df$Platform[df$Platform == 'PS'] <- 'PlayStation'
df$Platform[df$Platform == 'PSP'] <- 'PlayStation'
df$Platform[df$Platform == 'XOne'] <- 'XBox'
df$Platform[df$Platform == 'X360'] <- 'XBox'
df$Platform[df$Platform == 'XB'] <- 'XBox'

#Install tidyverse, run the below line by removing '# symbol'
#install.packages("tidyverse")

library(tidyverse)

#spread the values
df1<-df %>% group_by_at(vars(-Global_Sales)) %>%  mutate(row_id=1:n()) %>% ungroup() %>% spread(key=Platform, value=Global_Sales) %>%  select(-row_id)


library(dplyr)

#getting count,mean & standard deviation for Play station & XBox
group_by(df, Platform) %>% summarise(count = n(), mean = mean(Global_Sales , na.rm = TRUE),sd = sd(Global_Sales , na.rm = TRUE))

#boxplot
boxplot(df1[,10:11],xlab="Platforms",ylab="Global Sales (number of units sold in million)",main="Global Sales of video games for platforms PlayStation & XBox")


dt <-df$Global_Sales
dtMin=min(dt,na.rm=TRUE)
dtMax=22.1
dtMean=mean(dt,na.rm=TRUE)
dtSd=sd(dt,na.rm=TRUE)

#histogram
h<-hist(dt, breaks = 10, col = "lavender", ylab = "Frequency of Global Sales",xlab = "Global Sales (number of units sold in million)",main = "Distribution of Global Sales of video games", freq = TRUE, xlim=c(0,25), ylim=c(0,10000))
x <-seq(dtMin, dtMax , length = 1000)
y1 <-dnorm(x, mean=dtMean, sd=dtSd)
y1 <- y1 *diff(h$mids[1:2]) *length(dt)
lines(x, y1, col="#FC4E07", lwd=2)

dev.off()

