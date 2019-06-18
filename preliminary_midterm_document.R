#reads in the file
midterm2 <- read.csv("C:/Users/nopassword/Desktop/sqlite-tools-win32-x86-3140100/midterm2.csv",stringsAsFactors=FALSE)


#puts NA in for empty string so that categorical variables are correctly converted to factors
midterm2[midterm2=="" ] <-NA

#checks data type of each variable
str(midterm2)

#converts categorical variables to factors for analysis
midterm2$MaritalStatus<-as.factor(midterm2$MaritalStatus)
midterm2$Gender<-as.factor(midterm2$Gender)
midterm2$target<-as.factor(midterm2$target)
midterm2$YearlyIncome<-as.factor(midterm2$YearlyIncome)

#summary statistics for quantitative variables 
summary(midterm2)
#load ggplot2,plotrix for improved graphics
library(ggplot2)
library(plotrix)

#makes sure all the graphs between this piece of code and dev.off are contained in single pdf
pdf("Espinola_MS5333_Midterm.pdf")

#graphs for quantitative variables
qplot(midterm2$TotalPurchaseYTD,geom="histogram",main="Histogram of Total Purchase for the Year",binwidth=500,xlim=c(0,10000),ylim=c(0,3000),fill=I("orange"),col=I("red"),xlab="Amount Spent So Far This Year")
qplot(midterm2$NumberChildrenAtHome,geom="histogram",main="Histogram of Children At Home",binwidth=1,fill=I("green"),col=I("red"),xlab="Number of Children at Home")
qplot(midterm2$TotalChildren,geom="histogram",main="Histogram of Total Children",binwidth=.5,fill=I("blue"),col=I("red"),xlab="Number of Children")



#graphs for categorical variables


ggplot(midterm2[!is.na(midterm2$Gender), ], aes(x=Gender,fill=Gender)) + geom_bar(stat="count")+theme_minimal()
y<-c(length(which(midterm2$Gender=="M")),length(which(midterm2$Gender=="F")))
lbls <- c("Male", "Female")
pct <- round(y/length(midterm2$Gender)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(midterm2$Gender),labels = lbls, col=rainbow(length(lbls)),main="Pie Chart of Gender")


ggplot(midterm2[!is.na(midterm2$YearlyIncome), ], aes(x=YearlyIncome,fill=YearlyIncome)) + geom_bar(stat="count")+theme_minimal()
lbls1<-paste("$",levels(midterm2$YearlyIncome))
pie3D(table(midterm2$YearlyIncome),labels=lbls1,main="Yearly Income Pie Chart",explode=0.1)


ggplot(midterm2[!is.na(midterm2$MaritalStatus), ], aes(x=MaritalStatus,fill=MaritalStatus)) + geom_bar(stat="count")+theme_minimal()
z<-c(length(which(midterm2$MaritalStatus=="M")),length(which(midterm2$MaritalStatus=="S")))
lbls2 <- c("Married", "Single")
pct1 <- round(z/length(midterm2$MaritalStatus)*100)
lbls2 <- paste(lbls2, pct1) # add percents to labels 
lbls2 <- paste(lbls2,"%",sep="") # ad % to labels 
pie(table(midterm2$MaritalStatus),labels = lbls2, col=c("black","blue"),main="Pie Chart of Marital Status")

ggplot(midterm2[!is.na(midterm2$target), ], aes(x=target,fill=target)) + geom_bar(stat="count")+theme_minimal()
w<-c(length(which(midterm2$target=="0")),length(which(midterm2$target=="1")))
lbls3 <- c("Met target", "Hasn't met target")
pct2 <- round(w/length(midterm2$target)*100)
lbls3<- paste(lbls3, pct2) # add percents to labels 
lbls3 <- paste(lbls3,"%",sep="") # ad % to labels 
pie(table(midterm2$target),labels = lbls3, col=c("green","yellow"),main="Who is Meeting Our Target?")


#stops writing to single pdf
dev.off()


