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
#pdf("pie_plots.pdf") 
par(mfrow=c(3,3),cex=1,mai=c(0.01,0.01,0.01,0.01))

make <- table(data$make)
pourcentage_make <- round(as.vector(make)/sum(as.vector(make))*100,1)
pie(as.vector(make),labels=paste(names(make),"(",pourcentage_make,"%)",sep=""),radius=0.6,col = rainbow(length(as.vector(make))),cex=0.5)


#dev.off()
