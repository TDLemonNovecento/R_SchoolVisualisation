#following https://rkabacoff.github.io/datavis/DataPrep.html
pkgs <- c("ggplot2", "dplyr", "tidyr", 
          "mosaicData", "carData",
          "VIM", "scales", "treemapify",
          "gapminder", "ggmap", "choroplethr",
          "choroplethrMaps", "CGPfunctions",
          "ggcorrplot", "visreg",
          "gcookbook", "forcats",
          "survival", "survminer",
          "ggalluvial", "ggridges",
          "GGally", "superheat",
          "waterfalls", "factoextra",
          "networkD3", "ggthemes",
          "hrbrthemes", "ggpol",
          "ggbeeswarm")
install.packages(pkgs)


#set working directory to same directory as this file is located
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_directory = getwd()
print(paste("working directory is now set to: ", working_directory))


library(readr)

#import data from a comma delimited file
students <- read_csv("clean_data/students.csv")
#drop first column which is just the index (mistake when storing data)
students[-c(1)]

student_occupation <- read.csv("clean_data/student_occupation" )
st_occupation <- student_occupation[,c("a", "Name", "KLP", "Klasse")]

st_frequency <- read_csv("clean_data/student_frequency.csv")
#drop first column which is just the index (mistake when storing data)
st_frequency <- st_frequency[,-1]


#import teacher occupation
#t_monday <- read.csv( "clean_data/teachers_monday.csv")
#t_tuesday <- read.csv( "clean_data/teachers_tuesday.csv")
#t_wednesday <- read.csv("clean_data/teachers_wednesday.csv")
#t_thursday <- read.csv( "clean_data/teachers_thursday.csv")
#t_friday <- read.csv( "clean_data/teachers_friday.csv")
#week_teachers <- data.frame(t(select(teachers_monday[-c(1)], 1,2)))

t_weekly <- read.csv("clean_data/teachers_weekly")
data_values <- read.csv("clean_data/data_values" )

#import weekly occupation
weekly_occupation <- read.csv("clean_data/weekly_occupation.csv")


#transform data
library(dplyr)

#get rid of x in week denominator (starts at X6 > convert to 6)
data_values$X <- as.numeric(substring(data_values$X, 2))
weekly_occupation$x <- as.numeric(substring(weekly_occupation$x, 2))
weekly_occupation <- rename(weekly_occupation, "X" = "x")

df <- left_join(data_values, weekly_occupation, by=NULL, copy =FALSE)
df <- na.omit(df)


### Prepare summary data

library(tidyverse)

by_teacher <- aggregate(x=df$n, by = list(df$Lehrperson), FUN = sum)
by_class <- aggregate(x=df$n, by = list(df$Klassenstufe), FUN = sum)
by_day <- aggregate(x=df$n, by = list(df$Datum), FUN = sum)
by_week <- aggregate(x=df$n, by = list(df$Kalenderwoche), FUN = sum)

#summarize data by weekday
by_day$weekday <- weekdays(as.Date(by_day$Group.1))
by_weekday <- aggregate(x=by_day$x, by = list(by_day$weekday), FUN = mean)
by_weekday$Group.1 <- factor(by_weekday$Group.1, levels= c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"))
by_weekday <- by_weekday[order(by_weekday$Group.1),]


### Plot

# Opening the graphical device
pdf("plots/students_byweek.pdf",
    width = 8, height = 7,       # Width and height in inches
    bg = "white",                # Background color
    colormodel = "cmyk",         # Color model (cmyk is required for most publications)
    paper = "A4")                # Paper size

# Creating a plot
barplot(by_week$x, names.arg = by_week$Group.1, space = 1, col = "#DDDDFF", border = "#0000FF", xlab = "Kalenderwoche", ylab = "Anzahl Schüler im Lernatelier")

# Closing the graphical device
dev.off() 


# By Weekday
pdf("plots/students_byweekday.pdf",
    width = 8, height = 7,       # Width and height in inches
    bg = "white",                # Background color
    colormodel = "cmyk",         # Color model (cmyk is required for most publications)
    paper = "A4")                # Paper size

barplot(by_weekday$x, names.arg = by_weekday$Group.1, space = 1, 
        col = "#DDDDFF", border = "#0000FF", xlab = "Wochentag", ylab = "Durchschnittliche Anzahl Schüler im Lernatelier")
dev.off() 

# By Teacher
pdf("plots/students_byteacher.pdf",
    width = 8, height = 7,       # Width and height in inches
    bg = "white",                # Background color
    colormodel = "cmyk",         # Color model (cmyk is required for most publications)
    paper = "A4")                # Paper size

barplot(by_teacher$x, names.arg = by_teacher$Group.1, space = 1, 
        col = "#DDDDFF", border = "#0000FF", xlab = "Lehrperson Lernatelier",
        ylab = "Anzahl betreute Schüler")
dev.off() 


# By Class
pdf("plots/students_byclass.pdf",
    width = 8, height = 7,       # Width and height in inches
    bg = "white",                # Background color
    colormodel = "cmyk",         # Color model (cmyk is required for most publications)
    paper = "A4")                # Paper size

barplot(by_class$x, names.arg = by_class$Group.1, space = 1,
        col = "#DDDDFF", border = "#0000FF", xlab = "Klassenstufe", ylab = "Anzahl Schüler im Lernatelier")
dev.off()


pdf("plots/students_byweek_asclass.pdf",
    width = 8, height = 7,       # Width and height in inches
    bg = "white",                # Background color
    colormodel = "cmyk",         # Color model (cmyk is required for most publications)
    paper = "A4")                # Paper size

cl <- df[,c(2,5,6)]
library(plyr)
cl <- ddply(cl,.(Klassenstufen, Kalenderwoche),numcolwise(sum))

# Create grouped barplot
library(ggplot2)
ggplot(cl, aes(fill=Klassenstufen, x=Kalenderwoche, y=n, width=.7)) + 
  geom_bar(position = "dodge", stat="identity") +
  theme_minimal() +
  coord_fixed(ratio = 1) +
  labs(x='Kalenderwoche', y='Anzahl Schüler', title='Wöchentliche Belegung des Lernateliers')

dev.off()




#by Studentnames
library(plotly)
library(ggplot2)

st_frequency$x = str_replace_all(st_frequency$x,"[^[:graph:]]", " ") 

df1 <- aggregate(st_frequency, list(st_frequency$n), FUN = paste)
df2 <- aggregate(st_frequency, list(st_frequency$n), FUN = length)
df1$studnamelist <- paste(df1[,]$x,sep=", ", collapse=NULL)

by_studentnames <- data.frame(students = c(df1$studnamelist),
                              visits = c(df1$Group.1),
                              groupsize = c(df2$x))

p <- by_studentnames %>%
  ggplot( aes(x=visits, y=groupsize, text=students))+
  geom_bar(stat="identity", col = "#0000FF", fill = "#DDDDFF")+
  labs(x='Anzahl Besuche im Lernatelier', y='Anzahl Schüler')

pdf("plots/visits_bystudents.pdf",
    width = 8, height = 7,       # Width and height in inches
    bg = "white",                # Background color
    colormodel = "cmyk",         # Color model (cmyk is required for most publications)
    paper = "A4")                # Paper size
p
dev.off()

htmlwidgets::saveWidget(ggplotly(p), file.path('plots/visits_by_students.html'))


#by Classteacher
library(plotly)
library(ggplot2)

names(st_occupation)[names(st_occupation)=="a"] <- "Besuche"

dp <- ggplot(data=st_occupation, aes(x=Klasse, y=Besuche)) +
  geom_bar(stat="identity", col = "#0000FF", fill = "#DDDDFF")+
  labs(x='Klasse', y='Anzahl Besuche im Lernatelier')

pdf("plots/visits_byclasses.pdf",
    width = 8, height = 7,       # Width and height in inches
    bg = "white",                # Background color
    colormodel = "cmyk",         # Color model (cmyk is required for most publications)
    paper = "A4")                # Paper size
dp
dev.off()

p <- ggplot(data=st_occupation, aes(x=Klasse, y=Besuche, text=Name)) +
  geom_bar(stat="identity", col = "#0000FF", fill = "#DDDDFF")+
  labs(x='Klasse', y='Anzahl Besuche im Lernatelier')
htmlwidgets::saveWidget(ggplotly(p), file.path('plots/visits_by_classes.html'))
