# Question 1a)

roadrace_data = read.csv("C:/Users/allam/Documents/Spring 2022/CS6313 - Statistical Methods/MiniProjects/MiniProject2/roadrace.csv")
Maine = sum(roadrace_data$Maine == "Maine")
Away = sum(roadrace_data$Maine == "Away")

barplot(c(Maine,Away),main ="Question1", ylab = "Number of runners", names=c("Maine","Away"),
        xlab="Type", col="#7CFFBB")
Maine
Away
summary(roadrace_data$Maine)

# Question 1b)

away_runner_time = roadrace_data$Time..minutes.[which(roadrace_data$Maine == 'Away')] 
hist(away_runner_time, main = "Histogram - Runner Time (Away)", xlim = range(0,200), ylim = range(0,2000), 
     xlab = "Away Runner Time (minutes)", ylab = "Frequency", border = "black", col = "yellow" )

maine_runner_time = roadrace_data$Time..minutes.[which(roadrace_data$Maine == "Maine")]
hist(maine_runner_time, main = "Histogram - Runner Time (Maine)", xlim = range(0,200), ylim = range(0,2000),
     xlab = "Maine Runner Time (minutes)", ylab = "Frequency" , border = "black", col = "yellow")


range(away_runner_time)
IQR(away_runner_time)
sd(away_runner_time)
mean(away_runner_time)
median(away_runner_time)


range(maine_runner_time)
IQR(maine_runner_time)
sd(maine_runner_time)
mean(maine_runner_time)
median(maine_runner_time)


summary(away_runner_time)
summary(maine_runner_time)


# Question 1c)
away_runner_time = roadrace_data$Time..minutes.[which(roadrace_data$Maine == "Away")] 
maine_runner_time = roadrace_data$Time..minutes.[which(roadrace_data$Maine == "Maine")]
boxplot(maine_runner_time, away_runner_time, names = c("Maine", "Away"), main = "Runners' time (in minutes) for Maine and Away runners",ylab = "Frequency", 
        col = "yellow", border= "orange")

# Question 1d)
MaleAge = roadrace_data$Age[which(roadrace_data$Sex=='M')]
FemaleAge = roadrace_data$Age[which(roadrace_data$Sex=='F')]

MaleAge = as.numeric(as.character(MaleAge))
FemaleAge = as.numeric(as.character(FemaleAge))

boxplot(MaleAge, FemaleAge, main = "Box plot - Male & Female runners age", names = c("Male", "Female"), ylab= "Runners age",col="green")

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

# Question 2)
data1 = read.csv("C:/Users/allam/Documents/Spring 2022/CS6313 - Statistical Methods/MiniProjects/MiniProject2/motorcycle.csv")
FatalAccidents = data1$Fatal.Motorcycle.Accidents

boxplot(FatalAccidents, main = "BoxPlot - Motorcycle Accidents", xlab = "Fatal Accidents", ylab = "Accidents count", col="orange") 


LowerBound = max(quantile(FatalAccidents, prob=0.25)- 1.5*IQR(FatalAccidents),min(FatalAccidents))
UpperBound=min(quantile(FatalAccidents, prob=0.75) + 1.5*IQR(FatalAccidents),max(FatalAccidents))

#Outlier details
FatalCounty=data1$County[which(data1$Fatal.Motorcycle.Accidents<LowerBound | data1$Fatal.Motorcycle.Accidents > UpperBound)]
FatalCounty

summary(FatalAccidents)
range(FatalAccidents)
IQR(FatalAccidents)
sd(FatalAccidents)

# Start addition by Allama on outlier details.
outlierValues <- boxplot.stats(FatalAccidents)$out
subset(data1, Fatal.Motorcycle.Accidents == outlierValues, c(County, Fatal.Motorcycle.Accidents))

