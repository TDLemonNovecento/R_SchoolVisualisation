#install all packages
install.packages("rstudioapi")
install.packages("readxl")
install.packages("openxlsx")

#set working directory to same directory as this file is located
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_directory = getwd()
print(paste("working directory is now set to: ", working_directory))

#get excel data
my_excel_table = "Anmeldung_LA.xlsx"
main_sheet = "Wöchentl. Anwesenheitsbericht"
atelier_teacher_sheet = "Start"


#get names of excel sheets
library(readxl)
library(openxlsx)

start_week_date = readxl::read_excel(my_excel_table,
                                     sheet = atelier_teacher_sheet,
                                     col_names = FALSE,
                                     range = "B3:D4") 

data <- read.xlsx(xlsxFile =  my_excel_table,
                  sheet = main_sheet,
                  fillMergedCells = TRUE)


#get variable data part
data_raw <- readxl::read_excel(my_excel_table,
                               sheet = main_sheet,
                               skip = 9)
data_raw_clean <- data_raw[-c(1),]

#extract student data
students <- data_raw_clean[,1:5]
colnames(students) <- c("Name", "Klasse", "KLP", "Nachname_Lehrer", "Vorname_Lehrer")
colnames(students)

#save students
write.csv(students, "clean_data/students.csv")


#extract atelier teacher data
t_monday = data.frame(readxl::read_excel(my_excel_table,
                                     sheet = atelier_teacher_sheet,
                                     range = "H5:K11"))
t_tuesday = data.frame(readxl::read_excel(my_excel_table,
                                      sheet = atelier_teacher_sheet,
                                      range = "H13:K19"))

t_wednesday = data.frame(readxl::read_excel(my_excel_table,
                                        sheet = atelier_teacher_sheet,
                                        range = "H21:K27"))


t_thursday = data.frame(readxl::read_excel(my_excel_table,
                                       sheet = atelier_teacher_sheet,
                                       range = "H29:K35"))

t_friday = data.frame(readxl::read_excel(my_excel_table,
                                     sheet = atelier_teacher_sheet,
                                     range = "H37:K43"))

#convert teacher occupation to one useful df
df_list <- list(df1=t_monday, df2=t_tuesday, df3=t_wednesday, df4=t_thursday, df5=t_friday)

df_list <- lapply(df_list, function(df){
  df$Wochentag <- colnames(df)[1]
  df <- df[,-1]
  df
})

library(tidyverse)
t_weekly <- bind_rows(df_list)
t_weekly$LP[t_weekly$LP == "."] <- NA_character_
t_weekly$Klasse[t_weekly$Klasse == "."] <- NA_real_

#save teacher occupation
write.csv(t_monday, "clean_data/teachers_monday.csv")
write.csv(t_tuesday, "clean_data/teachers_tuesday.csv")
write.csv(t_wednesday, "clean_data/teachers_wednesday.csv")
write.csv(t_thursday, "clean_data/teachers_thursday.csv")
write.csv(t_friday, "clean_data/teachers_friday.csv")

write.csv(t_weekly, "clean_data/teachers_weekly")

### prepare data_values to be useful as legend
data_values <- data.frame(t(data[1:4,-c(1:6)]))
colnames(data_values) <- c("Kalenderwoche", "Datum", "Lehrperson", "Klassenstufen")

library(dplyr)
#get dates and calendar weeks nicely
data_values$Kalenderwoche[data_values$Kalenderwoche != "KW"] <- NA
data_values$KW <- data_values$Kalenderwoche
data_values$Kalenderwoche <- data_values$Datum[data_values$Kalenderwoche == "KW"]
data_values <- fill(data_values, Kalenderwoche)



data_values$Datum <- if_else(is.na(data_values$KW),
                             data_values$Datum,
                             data_values$Lehrperson)

data_values$Datum <- as.Date(as.numeric(data_values$Datum), origin = "1899-12-30")

#remove non-unique entries

data_values$Lehrperson[!(data_values$Lehrperson %in% unique(t_weekly$LP))] <- NA_real_
data_values$Klassenstufen[!(data_values$Klassenstufen %in% unique(t_weekly$Klasse))] <- NA_real_

data_values <- subset(data_values, select = -KW)
#save data_values

write.csv(data_values, "clean_data/data_values" )



#extract atelier places with header data
atelier_raw <- data[-c(1:9),-c(1:5)]


#start sorting data
library(dplyr)
library(tidyverse)

#find all values "a"
weekly_occupation = atelier_raw %>%
  gather(x, value)%>%
  group_by(x)%>%
  tally(value == "a")
write.csv(weekly_occupation, "clean_data/weekly_occupation" )


#clean student dataframe
dim(students)
students <- drop_na(students)

#get student visits
atelier_raw[is.na(atelier_raw)] <- 0
atelier_raw[atelier_raw == ","] <- 0
atelier_raw <- atelier_raw[c(1:dim(students)[1]),]
res <- apply(atelier_raw,MARGIN=1, table)

student_occupation <- as.data.frame(do.call(rbind, res))
student_occupation[student_occupation$`0` == student_occupation$a, c("a", "k")] <- 0

student_occupation$Name <- students$Name
student_occupation$KLP <- students$KLP
student_occupation$Klasse <- students$Klasse


write.csv(student_occupation, "clean_data/student_occupation" )



