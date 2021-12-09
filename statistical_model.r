#developed by me for practical research 2

install.packages("tidyverse") #Install this packages first
install.packages("ggpubr")


library("ggplot2")
library("ggpubr")
library("tidyverse")
cor.test(tgroup2.fps$latency, tgroup2.fps$fps.drop, method = "pear")
ggscatter(tgroup4.fps, x = "latency", y = "fps.drop",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Latency (ms)", ylab = "Video Cache Miss")

#visualizing the independent variables

lt2 <- lat2ping

lt2 %>%
  ggplot(aes(y=ping, x=time))+
  ggtitle("Network Latency ")+ #formatting
  labs(y = "Latency (ms)", x = "Time in seconds")+
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10))+
  #scale_y_continuous(trans='log2')+ #log function for better view
  geom_line(size = 0.2, color = "dark green")

latencytdt <- c(44,31,15,2)
lab <- c("TGroup 1", "TGroup 2", "TGroup 3", "TGroup 4")
barplot(latencytdt, names.arg=lab, xlab="Treatment Groups", ylab="FPS Drop", col="purple", border="purple")

sum(control.speed$upload)
sum(control.speed$download)

compACM <- c(30,28,35,47,47,17,15,17,25,37,10,8,10,14,21,5,4,8,5,7) #enter ACM values
x2 <-mean(compACM) #get the mean
x1 <-c(17) #define initial value
x3 <- x1-x2 #subtract x1 and x2
x4 <- x3/x1 #divide the difference with the initial value
x5 <- x4*100 #multiply by 100 to get percentage
print (-x5) #print and add absolute value to be positive

compspeed <- c(353090,550148, 1336455, 2833350 ) #enter ACM values
x2 <-mean(compspeed) #get the mean
x1 <-c(31517.631) #define initial value
x3 <- x1-x2 #subtract x1 and x2
x4 <- x3/x1 #divide the difference with the initial value
x5 <- x4*100 #multiply by 100 to get percentage
print (-x5) #print and add absolute value to be positive

kb <- c(31517.631, 1268261)
lab <- c("Control Group", "Treatments")
barplot(kb, names.arg=lab, xlab="Mean", ylab="Data Usage (KB)", col="purple", border="purple")

m <- c(80,80,100,250,500) #pre determined values in latency
mean(m)
summary(m)

x1 <- c(202) #ch4 part3; mean of trgroups in latency
x2 <- c(60.8) #mean of latency by control group
x3 <- x2 - x1 
x4 <- x3/x1
x5 <- x4*100
print (-x5)

mean(ping.control$ping)
        

#comparing all treatment groups performance issue (latency, each number follows: 80,80,100,250,500ms)

tr1 <- c(106,103,120,140,147)
tr2 <- c(75,68,71,88,97)
tr3 <- c(38,34,34,41,71)
tr4 <- c(9,9,15,11,20)
lat <- c(80,80,100,250,500,80,80,100,250,500)

summary(Untitled.8) #calculation
ln <- linearregressiontest
lin <- lm(Untitled.8$tr1+Untitled.8$tr2+Untitled.8$tr3+Untitled.8$tr4 ~ Untitled.8$lat, data = ln)
summary(lin)

ggplot(Untitled.8,aes(y=lat))+ #visualization
  labs(x = "Error rate", y = "Latency (ms)")+
  geom_line(aes(x=tr1, colour = "treatment 1 (50KB/s)"))+
  geom_line(aes(x=tr2, colour = "treatment 2 (100KB/s)"))+
  geom_line(aes(x=tr3, colour = "treatment 3 (250KB/s)"))+
  geom_line(aes(x=tr4, colour = "treatment 4 (500KB/s)"))

