# Rosaly Salcedo 
# IST 719 

#Read in Data 

education <- read.csv(file="C:/Users/Rosaly/Documents/IST 719/Project/education.csv", 
                      header=TRUE, sep=",")
View(education)

par(mfrow= c(5,2))

##BROOKLYN
#brooklyn grad rate
hist(education [education$boro == 'K', ]$graduation_rate, breaks = 5)
summary(education [education$Borough == 'BROOKLYN', ]$graduation_rate)
#brooklyn attendance rate 
brok <- hist(education [education$boro == 'K', ]$attendance_rate, breaks = 5)
summary(education [education$borough == ' BROOKLYN  ', ]$attendance_rate)
median(education [education$borough == ' BROOKLYN  ', ]$attendance_rate)

#QUEENS
#queens grad rate
hist(education [education$boro == 'Q', ]$graduation_rate, breaks = 5)
#queens attendance rate
hist(education [education$boro == 'Q', ]$attendance_rate, breaks = 5)



#bronx grad rate 
hist(education [education$boro == 'X', ]$graduation_rate, breaks = 5)
#bronx attendance rate 
hist(education [education$boro == 'X', ]$attendance_rate, breaks = 5)



#Manhattan grad rate 
hist(education [education$boro == 'M', ]$graduation_rate, breaks = 5)
#Manhattan attendance rate 
hist(education [education$boro == 'M', ]$attendance_rate, breaks = 5)



#Stanten Island grad rate 
hist(education [education$boro == 'R', ]$graduation_rate, breaks = 5)
#Stanten Island  attendance rate 
hist(education [education$boro == 'R', ]$attendance_rate, breaks = 5)



install.packages("plotly")
library(plotly)



# What are the stats for college career rate per boro 
p <- plot_ly(y=education$college_career_rate,
             x=education$boro, histfunc='sum', 
             type = "box") %>%
  layout(yaxis=list(type='linear'),
         title = "College Career Rate Per Borough")
p 


##
# Graduation rate Per Borough 
education <- education[!(is.na(education$graduation_rate) | education$graduation_rate==""), ]


View(education)
# Distribution for every borough for graduation Rate 
boro.dist <- split(education$graduation_rate, education$boro)
boxplot(boro.dist)

boro.mean <- sapply(boro.dist, mean)

barplot(boro.mean, col = "beige", main = "Average Graduation Rate\n\ Per Borough" )
abline(h = 0)

#install.packages("psych")
library("psych")

describeBy(education$graduation_rate, education$boro)
##########################################################

##DELETE THIS BOX
# Average COllege Career Rate 
education <- education[!(is.na(education$college_career_rate) | education$college_career_rate==""), ]

boro.dist <- split(education$college_career_rate, education$boro)
boxplot(boro.dist)

boro.mean <- sapply(boro.dist, mean)

barplot(boro.mean, col = "beige", main = "Average College Career Rate\n\ Per Borough" )
abline(h = 0)

#install.packages("psych")
#library("psych")

describeBy(education$graduation_rate, education$boro)


nrow(education)
ncol(education)
##########################################################
# Time Spent in High school Versus Grad Rate 

education2 <- read.csv(file="C:/Users/Rosaly/Documents/IST 719/Project/education2.csv", 
                      header=TRUE, sep=",")

education2 <- education2[!(is.na(education2$graduation_rate) | education2$graduation_rate==""), ]
education2 <- education2[!(is.na(education2$hours) | education2$hours==""), ]


plot_ly(data = education2, x = ~education2$hours, y = ~education2$attendance_rate, size = ~education2$graduation_rate)




##############################################################
# Does Shared Space affect Gradutation Rate?
#grad and borough 

#View(education2)

#shared_space_y <- education2[which (education2$shared_space == "Yes" ),]

#shared_space_n <- education2[which (education2$shared_space == "No"),]

#View(shared_space_n)

#data <- table(shared_space_y$graduation_rate, shared_space_y$shared_space )

#View(data)

#barplot(data, main = "Shared Space by Grad Rate ")


education2 <- read.csv(file="C:/Users/Rosaly/Documents/IST 719/Project/education2.csv", 
                       header=TRUE, sep=",")

# Grouped

library(ggplot2)

bp<- ggplot(education2, aes(fill=education2$shared_space, y=education2$graduation_rate, x=education2$boro)) + 
  geom_bar(stat="identity", position="fill" )

bp



pie <- bp + coord_polar("y", start=0)
pie

##########################################
##########################################
#########################################
###############################################

