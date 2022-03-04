
#1
##a
roadrace_data =read.csv("C:/Users/allam/Documents/Spring 2022/CS6313 - Statistical Methods/MiniProjects/MiniProject2/roadrace.csv")
head(roadrace_data$Age)
View(roadrace_data)
#barplot(c(sum(roadrace_data$Maine == "Away"), sum(roadrace_data$Maine == "Maine")), names.arg =c("Away", "Maine"), space = 0.25, ylab = "Number of runners")
#sum(roadrace_data$Maine == "Away")
#sum(roadrace_data$Maine == "Maine")

Maine = sum(roadrace_data$Maine == "Maine")
Away = sum(roadrace_data$Maine == "Away")
barplot(c(Maine,Away),main ="Question1", ylab = "Number of runners", names=c("Maine","Away"),xlab="Type", col="#7CFFBB")

Maine
Away

summary(roadrace_data$Maine)


## a final


roadrace_data = read.csv("C:/Users/allam/Documents/Spring 2022/CS6313 - Statistical Methods/MiniProjects/MiniProject2/roadrace.csv")
Maine = sum(roadrace_data$Maine == "Maine")
Away = sum(roadrace_data$Maine == "Away")
barplot(c(Maine,Away),main ="Question1", ylab = "Number of runners", names=c("Maine","Away"),
        xlab="Type", col="#7CFFBB")
Maine
Away
summary(roadrace_data$Maine)
##b

Away = roadrace_data$Time..minutes.[which(roadrace_data$Maine == 'Away')] 
hist(AwayRunnerTime, main = "Histogram - Runner Time (Away)", xlim = range(0,200), ylim = range(0,2000), 
     xlab = "Away Runner Time (minutes)", ylab = "Frequency", border = "black", col = "yellow" )

Maine = roadrace_data$Time..minutes.[which(roadrace_data$Maine == "Maine")]
hist(MaineRunnerTime, main = "Histogram - Runner Time (Maine)", xlim = range(0,200), ylim = range(0,2000),
     xlab = "Maine Runner Time (minutes)", ylab = "Frequency" , border = "black", col = "yellow")


range(Away)
IQR(Away)
sd(Away)
mean(Away)
median(Away)


range(Maine)
IQR(Maine)
sd(Maine)
mean(Maine)
median(Maine)


summary(Away)
summary(Maine)


##c
Away = roadrace_data$Time..minutes.[which(roadrace_data$Maine == "Away")] 
Maine = roadrace_data$Time..minutes.[which(roadrace_data$Maine == "Maine")]
boxplot(Maine, Away, names = c("Maine Time", "Away Time"), main = "Box plot",ylab = "Frequency", 
        col = "yellow", border= "orange")

##d



#boxplot(MaleAge,FemaleAge,main="Boxplot - Male & Female Runners
#Age",ylab="Male & Female
#        RunnersAge",xlab="Age",col="dodgerblue4",border="black",names=c('MaleAge','FemaleAge'),notch= TRUE,horizontal = TRUE)


MaleAge = roadrace_data$Age[which(roadrace_data$Sex=='M')]
FemaleAge = roadrace_data$Age[which(roadrace_data$Sex=='F')]

min(MaleAge)

MaleAge = as.numeric(as.character(MaleAge))
FemaleAge = as.numeric(as.character(FemaleAge))
min(as.numeric(MaleAge))
mean(FemaleAge)
#MaleAge<-as.numeric(MaleAge)
#FemaleAge<-as.numeric(FemaleAge)

boxplot(MaleAge,FemaleAge,main="Boxplot for Male & Female Runners Age",ylab="Male & Female
          RunnersAge",xlab="Age",col="dodgerblue4",border="black",names=c('RunnersAge_Male','RunnersAge_
                                                                          Female'),notch= TRUE,horizontal = TRUE)
#boxplot(MaleAge, FemaleAge, names = c('Male Runners Age','Female Runners Age'))

#boxplot(MaleAge,FemaleAge, main = "Box plot - Male & Female runners age", names = c("Maine", "Away"), ylab= "Runners age",col="green")

mean(MaleAge)
median(MaleAge)
summary(MaleAge)
range(MaleAge)
IQR(MaleAge)
sd(MaleAge)

mean(FemaleAge)
median(FemaleAge)
summary(FemaleAge)
range(FemaleAge)
IQR(FemaleAge)
sd(FemaleAge)


#####error here in ##d

#2

data1= read.csv("C:/Users/allam/Documents/Spring 2022/CS6313 - Statistical Methods/MiniProjects/MiniProject2/motorcycle.csv")
FatalAccidents = data1$Fatal.Motorcycle.Accidents
#Now create boxplot for Fatal.Motorcycle.Accidents values
boxplot(FatalAccidents, xlab = "Fatal Motorcycle Accidents", ylab = "Number of Accidents") 
#For calculating the required statistics, we are executing the following code
#Now calculate the Upper and Lower bounds
LowerBound = max(quantile(FatalAccidents, prob=0.25)- 1.5*IQR(FatalAccidents),min(FatalAccidents))
UpperBound=min(quantile(FatalAccidents, prob=0.75) + 1.5*IQR(FatalAccidents),max(FatalAccidents))
#Now check for outliers and display counties that are outliers
FatalCounty=data1$County[which(data1$Fatal.Motorcycle.Accidents<LowerBound | data1$Fatal.Motorcycle.Accidents > UpperBound)]


FatalCounty

summary(FatalAccidents)
range(FatalAccidents)
IQR(FatalAccidents)
sd(FatalAccidents)



#2 final


data1= read.csv("C:/Users/allam/Documents/Spring 2022/CS6313 - Statistical Methods/MiniProjects/MiniProject2/motorcycle.csv")
FatalAccidents = data1$Fatal.Motorcycle.Accidents

boxplot(FatalAccidents, main = "BoxPlot - Motorcycle Accidents", xlab = "Fatal Accidents", ylab = "Accidents count", col="orange") 


LowerBound = max(quantile(FatalAccidents, prob=0.25)- 1.5*IQR(FatalAccidents),min(FatalAccidents))
UpperBound=min(quantile(FatalAccidents, prob=0.75) + 1.5*IQR(FatalAccidents),max(FatalAccidents))

FatalCounty=data1$County[which(data1$Fatal.Motorcycle.Accidents<LowerBound | data1$Fatal.Motorcycle.Accidents > UpperBound)]


FatalCounty

summary(FatalAccidents)
range(FatalAccidents)
IQR(FatalAccidents)
sd(FatalAccidents)

