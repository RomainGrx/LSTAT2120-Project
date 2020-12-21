#https://www.kaggle.com/nisargpatel/automobiles


#Data cleaning
data <- read.csv("C:\\Users\\cedri\\Documents\\GitHub\\LSTAT2120-Project\\datasets\\automobile.csv", sep=",", dec=".",header=TRUE)

data <- data[,c(1:8,10:26)]

colnames(data)[3] <- "make"
data <- data[!data$make=="alfa-romero",]
data <- data[!data$make=="chevrolet",]
data <- data[!data$make=="isuzu",]
data <- data[!data$make=="jaguar",]
data <- data[!data$make=="mercury",]
data <- data[!data$make=="porsche",]
data <- data[!data$make=="renault",]

colnames(data)[15] <- "number_of_cylinders"
#data <- data[!data$number_of_cylinders=="twelve",]
#data <- data[!data$number_of_cylinders=="three",]

data <- droplevels(data)
#data <- sample(data)

#DIvision train an test set
set.seed((2))
sample <- sample.int(n=nrow(data),size=floor(.25*nrow(data)),replace=F)
train <- data[-sample,]
test <- data[sample,]

#Variables analysis
#Qualitative variables
library(RColorBrewer)
pdf("C:\\Users\\cedri\\Documents\\Unif\\2020-2021\\Q7\\LSTAT2120 Modèles linéaires\\plots\\pie_qual_var.pdf") 
par(mfrow=c(3,3),cex=1,mai=c(0.01,0.01,0.01,0.01))

make <- table(data$make)
pourcentage_make <- round(as.vector(make)/sum(as.vector(make))*100,1)
pie(as.vector(make),labels=paste(names(make),"(",pourcentage_make,"%)",sep=""),radius=0.6,col = brewer.pal(n=11,name="RdYlBu"),cex=0.45)

colors=c("#CC0000","#FF0000") #hexadecimal color code
fuel_type <- table(data$fuel_type)
pourcentage_fuel_type <- round(as.vector(fuel_type)/sum(as.vector(fuel_type))*100,1)
pie(as.vector(fuel_type),labels=paste(names(fuel_type),"(",pourcentage_fuel_type,"%)",sep=""),radius=0.6,col = brewer.pal(n=2,name="RdYlBu"),cex=0.45)

aspiration <- table(data$aspiration)
pourcentage_aspiration <- round(as.vector(aspiration)/sum(as.vector(aspiration))*100,1)
pie(as.vector(aspiration),labels=paste(names(aspiration),"(",pourcentage_aspiration,"%)",sep=""),radius=0.6,col=brewer.pal(n=2,name="RdYlBu"),cex=0.45)

number_of_doors <- table(data$number_of_doors)
pourcentage_number_of_doors <- round(as.vector(number_of_doors)/sum(as.vector(number_of_doors))*100,1)
pie(as.vector(number_of_doors),labels=paste(names(number_of_doors),"(",pourcentage_number_of_doors,"%)",sep=""),radius=0.6,col = brewer.pal(n=2,name="RdYlBu"),cex=0.45)

body_style <- table(data$body_style)
pourcentage_body_style <- round(as.vector(body_style)/sum(as.vector(body_style))*100,1)
pie(as.vector(body_style),labels=paste(names(body_style),"(",pourcentage_body_style,"%)",sep=""),radius=0.6,col = brewer.pal(n=5,name="RdYlBu"),cex=0.45)

drive_wheels <- table(data$drive_wheels)
pourcentage_drive_wheels <- round(as.vector(drive_wheels)/sum(as.vector(drive_wheels))*100,1)
pie(as.vector(drive_wheels),labels=paste(names(drive_wheels),"(",pourcentage_drive_wheels,"%)",sep=""),radius=0.6,col = brewer.pal(n=3,name="RdYlBu"),cex=0.45)

engine_type <- table(data$engine_type)
pourcentage_engine_type <- round(as.vector(engine_type)/sum(as.vector(engine_type))*100,1)
pie(as.vector(engine_type),labels=paste(names(engine_type),"(",pourcentage_engine_type,"%)",sep=""),radius=0.6,col = brewer.pal(n=6,name="RdYlBu"),cex=0.45)

number_of_cylinders <- table(data$number_of_cylinders)
pourcentage_number_of_cylinders <- round(as.vector(number_of_cylinders)/sum(as.vector(number_of_cylinders))*100,1)
pie(as.vector(number_of_cylinders),labels=paste(names(number_of_cylinders),"(",pourcentage_number_of_cylinders,"%)",sep=""),radius=0.6,col = brewer.pal(n=5,name="RdYlBu"),cex=0.45)

fuel_system <- table(data$fuel_system)
pourcentage_fuel_system <- round(as.vector(fuel_system)/sum(as.vector(fuel_system))*100,1)
pie(as.vector(fuel_system),labels=paste(names(fuel_system),"(",pourcentage_fuel_system,"%)",sep=""),radius=0.6,col = brewer.pal(n=7,name="RdYlBu"),cex=0.45)

dev.off()

#Quantitative variables
library(ggplot2)
library(e1071)
names_c <- c("ï..symboling","normalized_losses","wheel_base","length","width","height","curb_weight","engine_size","bore","stroke","compression_ratio","horsepower","peak_rpm","city_mpg","highway_mpg")
continuous <- data[,names_c]   #c(1:2,9:13,16,18:24)
means <- sapply(continuous,mean)
k <- sapply(continuous,kurtosis)
s <- sapply(continuous,skewness)
sd <- sapply(continuous,sd)

pdf("C:\\Users\\cedri\\Documents\\Unif\\2020-2021\\Q7\\LSTAT2120 Modèles linéaires\\plots\\histo_qual_var.pdf")
par(mfrow=c(4,4))
for(i in names(continuous)){
  hist(continuous[[i]],xlab="",label=FALSE,plot=TRUE,freq=F,main=i)
  curve(dnorm(x,mean=means[i],sd=sd[i]),col="red",lwd=2,add=TRUE,yaxt="n")
  
  abline(v=means[i],lty=1,col="darkblue",lwd=2)
  abline(v=means[i]+sd[i],lty=2)
  abline(v=means[i]-sd[i],lty=2)
  abline(v=means[i]+2*sd[i],lty=2)
  abline(v=means[i]-2*sd[i],lty=2)
}
dev.off()

names_nc <- c("make","fuel_type","aspiration","number_of_doors","body_style","drive_wheels","engine_type","number_of_cylinders","fuel_system")

#Boxplots by make
pdf("C:\\Users\\cedri\\Documents\\Unif\\2020-2021\\Q7\\LSTAT2120 Modèles linéaires\\plots\\boxplot_by_make.pdf") 
for(i in data[,names_c]){
  boxplot(i~data[,"make"],ylab=names_nc[j],xlab="")
}
dev.off()

#Boxplots by price
pdf("C:\\Users\\cedri\\Documents\\Unif\\2020-2021\\Q7\\LSTAT2120 Modèles linéaires\\plots\\boxplot_by_price.pdf") 
for(i in data[,names_nc]){
  boxplot(data$price~i,xlab=names_c[j],ylab="price")
}
dev.off()

#correlation matrix
pdf("C:\\Users\\cedri\\Documents\\Unif\\2020-2021\\Q7\\LSTAT2120 Modèles linéaires\\plots\\correlation_plot.pdf")
library(corrplot)
out <- data[,names_c]
Ma <- cor(out)
corrplot(Ma, type="upper",order="hclust",col=brewer.pal(n=11,name="RdYlBu"),tl.col="black")
dev.off()

#First model with raw data
M <- lm(price~.,train)
names <- names(coef(M))[2:length(names(coef(M)))]
summary(M)








