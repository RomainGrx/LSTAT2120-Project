#https://www.kaggle.com/nisargpatel/automobiles


#Data cleaning
data <- read.csv("./datasets/automobile.csv", sep=",", dec=".",header=TRUE)

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

names(data)[names(data)=="?..symboling"] <- "symboling"

#DIvision train an test set
set.seed((2))
sample <- sample.int(n=nrow(data),size=floor(.25*nrow(data)),replace=F)
train <- data[-sample,]
test <- data[sample,]

#Variables analysis
#Qualitative variables
library(RColorBrewer)
par(mfrow=c(3,3),cex=1,mai=c(0.01,0.01,0.01,0.01))

make <- table(data$make)
pourcentage_make <- round(as.vector(make)/sum(as.vector(make))*100,1)
pie(as.vector(make),labels=paste(names(make),"(",pourcentage_make,"%)",
                                 sep=""),
    radius=0.9,col = brewer.pal(n=9,name="RdYlBu"),cex=0.6)

colors=c("#CC0000","#FF0000") #hexadecimal color code
fuel_type <- table(data$fuel_type)
pourcentage_fuel_type <- round(as.vector(fuel_type)/
                                 sum(as.vector(fuel_type))*100,1)
pie(as.vector(fuel_type),labels=paste(names(fuel_type),
                                      "(",pourcentage_fuel_type,"%)",sep=""),
    radius=0.9,col = brewer.pal(n=3,name="RdYlBu"),cex=0.6)

aspiration <- table(data$aspiration)
pourcentage_aspiration <- round(as.vector(aspiration)/
                                  sum(as.vector(aspiration))*100,1)
pie(as.vector(aspiration),labels=paste(names(aspiration),
                                       "(",pourcentage_aspiration,"%)",sep=""),
    radius=0.9,col=brewer.pal(n=3,name="RdYlBu"),cex=0.6)

number_of_doors <- table(data$number_of_doors)
pourcentage_number_of_doors <- round(as.vector(number_of_doors)/
                                       sum(as.vector(number_of_doors))*100,1)
pie(as.vector(number_of_doors),labels=paste(names(number_of_doors),
                                            "(",pourcentage_number_of_doors,
                                            "%)",sep=""),
    radius=0.9,col = brewer.pal(n=3,name="RdYlBu"),cex=0.6)

body_style <- table(data$body_style)
pourcentage_body_style <- round(as.vector(body_style)/
                                  sum(as.vector(body_style))*100,1)
pie(as.vector(body_style),labels=paste(names(body_style),
                                       "(",pourcentage_body_style,"%)",sep=""),
    radius=0.9,col = brewer.pal(n=5,name="RdYlBu"),cex=0.6)

drive_wheels <- table(data$drive_wheels)
pourcentage_drive_wheels <- round(as.vector(drive_wheels)/
                                  sum(as.vector(drive_wheels))*100,1)
pie(as.vector(drive_wheels),labels=paste(names(drive_wheels),
                                         "(",pourcentage_drive_wheels,
                                         "%)",sep=""),
    radius=0.9,col = brewer.pal(n=3,name="RdYlBu"),cex=0.6)

engine_type <- table(data$engine_type)
pourcentage_engine_type <- round(as.vector(engine_type)/
                                   sum(as.vector(engine_type))*100,1)
pie(as.vector(engine_type),labels=paste(names(engine_type),
                                        "(",pourcentage_engine_type,
                                        "%)",sep=""),
    radius=0.9,col = brewer.pal(n=6,name="RdYlBu"),cex=0.6)

number_of_cylinders <- table(data$number_of_cylinders)
pourcentage_number_of_cylinders <- 
  round(as.vector(number_of_cylinders)/
          sum(as.vector(number_of_cylinders))*100,1)
pie(as.vector(number_of_cylinders),labels=paste(names(number_of_cylinders),
                                                "(",pourcentage_number_of_cylinders
                                                ,"%)",sep=""),
    radius=0.9,col = brewer.pal(n=5,name="RdYlBu"),cex=0.6)

fuel_system <- table(data$fuel_system)
pourcentage_fuel_system <- round(as.vector(fuel_system)/
                                   sum(as.vector(fuel_system))*100,1)
pie(as.vector(fuel_system),labels=paste(names(fuel_system),
                                        "(",pourcentage_fuel_system,"%)",
                                        sep=""),
    radius=0.9,col = brewer.pal(n=7,name="RdYlBu"),cex=0.6)

dev.off()

names_nc <- c("make","fuel_type","aspiration","number_of_doors","body_style",
              "drive_wheels","engine_type","number_of_cylinders","fuel_system")
noncontinious <- data[,names_nc] 

#Quantitative variables
library(ggplot2)
library(e1071)
names_c <- c("symboling","normalized_losses","wheel_base","length","width",
             "height","curb_weight","engine_size","bore","stroke","compression_ratio",
             "horsepower","peak_rpm","city_mpg","highway_mpg","price")
continuous <- data[,names_c]   #c(1:2,9:13,16,18:24)
means <- sapply(continuous,mean)
k <- sapply(continuous,kurtosis)
s <- sapply(continuous,skewness)
sd <- sapply(continuous,sd)

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



#Boxplots by make
par(mfrow=c(1,1),mai=c(0.35,0.35,0.35,0.35),cex=0.6)
j <- 1
for(i in data[,names_c]){
  boxplot(i~data[,"make"],ylab=names_c[j],xlab="")
  j <- j+1
}
dev.off()

#Boxplots by price
par(mfrow=c(1,1),cex=0.6,mai=c(0.55,0.55,0.55,0.55))
j <- 1
for(i in data[,names_nc]){
  boxplot(data$price~i,xlab=names_nc[j],ylab="price")
  j <- j+1
}
dev.off()

#correlation matrix
library(corrplot)
out <- data[,names_c]
Ma <- cor(out)
corrplot(Ma, type="upper",order="hclust",col=brewer.pal(n=11,name="RdYlBu"),
         tl.col="black")
dev.off()

#First model with raw data
M <- lm(price~.,train)
names <- names(coef(M))[2:length(names(coef(M)))]
summary(M)


#multicollinearity
library(olsrr)
#print(ols_vif_tol(M))
usdm::vifcor(train[,names_c])

train$curb_weight <- NULL
train$horsepower <- NULL
train$length <- NULL
train$city_mpg <- NULL

#heterodastisity
library(lmtest)
library(olsrr)

# TODO
bptest(non_multi_model, varformula = train$make~train$price, data=data)

for (name in colnames(train)){
  print(name)
  if (name != "make" && name != "fuel_type" && name != "aspiration" && 
      name != "number_of_doors" && name != "body_style" && name != "drive_wheels" 
      && name != "engine_type" && name != "number_of_cylinders" 
      && name != "fuel_system"){
    plot(non_multi_model$residuals~train[,name], ylab="residuals", xlab=name, 
         main=paste(name, " vs resiudals"))
  }else{
    boxplot(non_multi_model$residuals~train[,name], xlab = name,
            ylab = "residuals", main=paste(name, " vs resiudals"))
  }
}

df <- data.frame(matrix(ncol = 2, nrow = 0))
for (i in colnames(train)){
  t<-gqtest(M, order.by=train[,i], fraction=2) 
  df<-rbind(df, c(i, t$p.value))
  print(paste("p−value for ",i ," is ",t$p.value , sep=""))
}
colnames(df) <- c('variable', 'p-value')


library(caret)
to_box_cox <- c("normalized_losses", "wheel_base", "width", "height", "engine_size")
for (col in to_box_cox){
  dist <- caret::BoxCoxTrans(train[,col])
  pred <- predict(dist, train[,col])
  train[,col] <- NULL
  new_name <- paste(col, "_boxcox", sep="")
  names_c <- union(names_c, c(new_name))
  train[,new_name] <- pred
}

train$drive_wheels <- NULL
train$fuel_system <- NULL

#boxplot outliers
names_c <- c("symboling","normalized_losses","wheel_base","width","height",
             "engine_size","bore","stroke","compression_ratio","peak_rpm",
             "highway_mpg","price")
names<-colnames(train[ ,names_c])
j<-1
for(i in train[,names_c]){
  a<-boxplot(i,plot=TRUE,main=names[j])
  out<-a$out
  legend("topright",legend=paste("Number_of_outliers_is_",length(out),sep=" "))
  dev.off()
  j<-j+1
  if(length(out!=0)){
    tou<-rownames(train[which(i %in% out),])
    print(length(tou))
    outli<-c(tou,as.numeric(tou))
  }
}

#cooksdistance raw data
(setattr(train,"row.names",c(seq(1,length(train[,1])))))
M<-lm(price~.,train)
summary(M)
cooksd<-cooks.distance(M)
sample_size <- length(cooksd)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h=(4/sample_size), col="red")
dev.off()

cooksdistance <- cooks.distance(M)
train$cd <- cooksdistance

#remove influentials
influential <- as.numeric(names(cooksdistance)[!(cooksdistance<4/
                                                   length(train[,1]))])
length(influential)
train_without_influ <- subset(train,cd<4/length(train[,1]))
M1<-lm(price~.,train_without_influ)
summary(M1)

#remove outliers
names_c <- c("symboling","normalized_losses","wheel_base","width","height",
             "engine_size","bore","stroke","compression_ratio","peak_rpm",
             "highway_mpg","price")
for (col in intersect(colnames(train), names_c)){
  out <- boxplot.stats(train[,col])$out
  idx <- which(train[,col] %in% out)
  w<-train[-idx,]
}
M2<-lm(price~.,w)
summary(M2)

#remove outliers then influentials
train_without_influ2 <- subset(w,cd<4/length(train[,1]))
M3<-lm(price~.,train_without_influ2)
summary(M3)

#remove influentials then outliers
for (col in intersect(colnames(train_without_influ), names_c)){
  out <- boxplot.stats(train_without_influ[,col])$out
  idx <- which(train_without_influ[,col] %in% out)
  w2<-train_without_influ[-idx,]
}
M4<-lm(price~.,w)
summary(M4)



#feature selection
M0=lm(price~.,train_without_influ)
summary(M0)

library(olsrr)

olsrr::ols_step_both_aic(M0)

print(names(train_without_influ))
train_without_influ$body_style <- NULL
train_without_influ$wheel_base_boxcox <- NULL
train_without_influ$height <- NULL
train_without_influ$wheel_base <- NULL
train_without_influ$height_boxcox <- NULL
train_without_influ$bore <- NULL
train_without_influ$compression_ratio <- NULL
train_without_influ$cd <- NULL

# Linear combination
M0=lm(price~.,train_without_influ)
summary(M0)

train_without_influ$engine_size <- NULL
train_without_influ$make <- NULL

M8=lm(price~.,train_without_influ)
var.test(M0,M8)

#final model
M0=lm(price~.,train_without_influ)
summary(M0)

coeff_ <- M0$coefficients
coeff_df <- data.frame(sort(coeff_))
write.csv(coeff_df, './res/coeffs.csv', row.names=TRUE)

# Inference on the test set 
copy_test <- test
to_keep <- c("make", "engine_size", "highway_mpg", "width", 
             "aspiration", "number_of_cylinders", "stroke", "fuel_type", 
             "peak_rpm", "number_of_doors", "normalized_losses", "engine_type", 
             "symboling")

test <- test[,to_keep]

for (col in intersect(to_keep, to_box_cox)){
  print(col)
  dist <- caret::BoxCoxTrans(test[,col])
  pred <- predict(dist, test[,col])
  test[,col] <- NULL
  test[,paste(col, "_boxcox", sep="")] <- pred
}

# Confidence interval on the test set
inference <- predict(M0, test)
inference.int <- predict(M0, interval="prediction")

rsq <- function (x, y) cor(x, y) ^ 2
print(rsq(copy_test[,"price"], inference))
summary(inference.int)

# Plot of the inference on the true values
plot(inference~copy_test[,"price"], xlab="True values", ylab="Predicted values"
     , main="True values vs predicted values")
lines(c(min(copy_test[,"price"]), max(copy_test[,"price"])), c(min(inference),
                                                               max(inference)), 
      type="l",col="black")





