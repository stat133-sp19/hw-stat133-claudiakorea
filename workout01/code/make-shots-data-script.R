#Title: Make shots-data.csv
#Description: cleans the given data to create a csv data file 'shots-data.csv'
#Inputs: 5 csv files, data about each player
#Outputs: a csv file

iguodala <- read.csv("C:/Users/CK/Documents/stat133/hw-stat133/workout01/data/andre-iguodala.csv", stringsAsFactors = FALSE)

green <- read.csv("C:/Users/CK/Documents/stat133/hw-stat133/workout01/data/draymond-green.csv", stringsAsFactors = FALSE)

durant <- read.csv("C:/Users/CK/Documents/stat133/hw-stat133/workout01/data/kevin-durant.csv", stringsAsFactors = FALSE)

thompson <- read.csv("C:/Users/CK/Documents/stat133/hw-stat133/workout01/data/klay-thompson.csv", stringsAsFactors = FALSE)

curry <- read.csv("C:/Users/CK/Documents/stat133/hw-stat133/workout01/data/stephen-curry.csv", stringsAsFactors = FALSE)


iguodala$name <- "Andre Iguodala"
green$name <- "Graymond Green"
durant$name <- "Kevin Durant"
thompson$name <- "Klay Thompson"
curry$name <- "Stephen Curry"

iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag == "y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == "y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == "y"] <- "shot_yes"


iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag == "n"] <- "shot_no"
durant$shot_made_flag[durant$shot_made_flag == "n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag == "n"] <- "shot_no"


iguodala$minute <- iguodala$period * 12 + iguodala$minutes_remaining
green$minute <- green$period * 12 + green$minutes_remaining
durant$minute <- durant$period * 12 + durant$minutes_remaining
thompson$minute <- thompson$period * 12 + thompson$minutes_remaining
curry$minute <- curry$period * 12 + curry$minutes_remaining


sink("C:/Users/CK/Documents/stat133/hw-stat133/workout01/output/andre-iguodala-summary.txt" )
summary(iguodala)
sink()

sink("C:/Users/CK/Documents/stat133/hw-stat133/workout01/output/graymond-green-summary.txt" )
summary(green)
sink()

sink("C:/Users/CK/Documents/stat133/hw-stat133/workout01/output/kevin-durant-summary.txt" )
summary(durant)
sink()

sink("C:/Users/CK/Documents/stat133/hw-stat133/workout01/output/klay-thompson-summary.txt" )
summary(thompson)
sink()


sink("C:/Users/CK/Documents/stat133/hw-stat133/workout01/output/stephen-curry-summary.txt" )
summary(curry)
sink()

players <- rbind(iguodala, green, durant, thompson, curry)

write.table(players,file = "C:/Users/CK/Documents/stat133/hw-stat133/workout01/data/shots-data.csv" )

