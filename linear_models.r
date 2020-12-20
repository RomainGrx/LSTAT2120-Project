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
data <- sample(data)

#DIvision train an test set
set.seed((2))
sample <- sample.int(n=nrow(data),size=floor(.25*nrow(data)),replace=F)
train <- data[-sample,]
test <- data[sample,]

#Variables analysis
#Categorical variables
pdf("pie_plots.pdf") 
par(mfrow=c(3,3),cex=1,mai=c(0.01,0.01,0.01,0.01))

make <- table(data$make)
pourcentage_make <- round(as.vector(make)/sum(as.vector(make))*100,1)
pie(as.vector(make),labels=paste(names(make),"(",pourcentage_make,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(make))),cex=0.45)

fuel_type <- table(data$fuel_type)
pourcentage_fuel_type <- round(as.vector(fuel_type)/sum(as.vector(fuel_type))*100,1)
pie(as.vector(fuel_type),labels=paste(names(fuel_type),"(",pourcentage_fuel_type,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(fuel_type))),cex=0.45)

aspiration <- table(data$aspiration)
pourcentage_aspiration <- round(as.vector(aspiration)/sum(as.vector(aspiration))*100,1)
pie(as.vector(aspiration),labels=paste(names(aspiration),"(",pourcentage_aspiration,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(aspiration))),cex=0.45)

number_of_doors <- table(data$number_of_doors)
pourcentage_number_of_doors <- round(as.vector(number_of_doors)/sum(as.vector(number_of_doors))*100,1)
pie(as.vector(number_of_doors),labels=paste(names(number_of_doors),"(",pourcentage_number_of_doors,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(number_of_doors))),cex=0.45)

body_style <- table(data$body_style)
pourcentage_body_style <- round(as.vector(body_style)/sum(as.vector(body_style))*100,1)
pie(as.vector(body_style),labels=paste(names(body_style),"(",pourcentage_body_style,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(body_style))),cex=0.45)

drive_wheels <- table(data$drive_wheels)
pourcentage_drive_wheels <- round(as.vector(drive_wheels)/sum(as.vector(drive_wheels))*100,1)
pie(as.vector(drive_wheels),labels=paste(names(drive_wheels),"(",pourcentage_drive_wheels,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(drive_wheels))),cex=0.45)

engine_type <- table(data$engine_type)
pourcentage_engine_type <- round(as.vector(engine_type)/sum(as.vector(engine_type))*100,1)
pie(as.vector(engine_type),labels=paste(names(engine_type),"(",pourcentage_engine_type,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(engine_type))),cex=0.45)

number_of_cylinders <- table(data$number_of_cylinders)
pourcentage_number_of_cylinders <- round(as.vector(number_of_cylinders)/sum(as.vector(number_of_cylinders))*100,1)
pie(as.vector(number_of_cylinders),labels=paste(names(number_of_cylinders),"(",pourcentage_number_of_cylinders,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(number_of_cylinders))),cex=0.45)

fuel_system <- table(data$fuel_system)
pourcentage_fuel_system <- round(as.vector(fuel_system)/sum(as.vector(fuel_system))*100,1)
pie(as.vector(fuel_system),labels=paste(names(fuel_system),"(",pourcentage_fuel_system,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(fuel_system))),cex=0.45)

dev.off()
