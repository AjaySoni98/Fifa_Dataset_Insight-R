# loading req. libraries
library(dplyr)
library(magrittr)
library(class) #for KNN
library(ggplot2)
library(caret)
library(gplots)

#setting working directory
setwd("C:/R/RProject")

#reading dataset by proving local location
df = read.csv('C:/R/RProject/players_20.csv')

#keeping only rows which have value of column value_eur gretaer than 1
df = subset(df, df$value_eur>1)

#subsetting dataset to only req. columns
df.subset = select(df, c(player_positions, pace, shooting, passing, dribbling, defending, physic))

#loading the playerposition column as 'character' to enable it for compariosion
df.subset$player_positions = as.character(df.subset$player_positions)

#renaming player positions
for(i in 1:18028){
  words = strsplit(df.subset$player_positions[i], ",")[[1]]
  first_word = words[1]
  if(first_word == "ST" | first_word == "CF"){
    df.subset$player_positions[i] = "1" #Fwd_Centre
  }
  else if(first_word == "LW" | first_word == "RW"){
    df.subset$player_positions[i] = "2" #Fwd_Winger
  }
  else if(first_word == "CM" | first_word == "CAM" | first_word == "CDM"){
    df.subset$player_positions[i] = "3" #Mid_Centre
  }
  else if(first_word == "RM" | first_word == "LM"){
    df.subset$player_positions[i] = "4" #Mid_Side
  }
  else if(first_word == "CB"){
    df.subset$player_positions[i] = "5" #Back_Centre
  }
  else if(first_word == "LB" | first_word == "RB"){
    df.subset$player_positions[i] = "6" #Back_Side
  }
  else if(first_word == "LWB" | first_word == "RWB"){
    df.subset$player_positions[i] = "7" #Back_Winger
  }
}

#excluding rows in player_positions wheree position is GK (goal keeper)
df.subset = subset(df.subset, df.subset$player_positions != "GK")

#converting the column again to a factor
df.subset$player_positions = as.factor(df.subset$player_positions)

#normalization function
nor <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

#normalizing numeric values. also removing player_positions column. because its the target column
df.subset.n = as.data.frame(lapply(df.subset[2:7], nor))

#creating train and test data now
set.seed(123)
dat.d = sample(1:nrow(df.subset.n), size = nrow(df.subset.n)*0.8, replace = FALSE)

train.df = df.subset[dat.d,]
test.df = df.subset[-dat.d,]

#creating train and test data for player_positions column
train.df_labels = df.subset[dat.d,1]
test.df_labels = df.subset[-dat.d,1]

#knn
knn_model = knn(train = train.df, test = test.df, cl=train.df_labels, k = 11)
acc = 100 * sum(test.df_labels == knn_model)/NROW(test.df_labels)

#converting the column again to a factor
df.subset$player_positions = as.factor(df.subset$player_positions)

tab = table(knn_model, test.df_labels)
tab = round((tab/rowSums(tab)), digits = 1)
tab[tab == "NaN"] = 0
colnames(tab) = as.factor(c("Fwd_Centre", "Fwd_Winger", "Mid_Centre", "Mid_Side", "Back_Centre", "Back_Side", "Back_Winger"))
rownames(tab) = as.factor(c("Fwd_Centre", "Fwd_Winger", "Mid_Centre", "Mid_Side", "Back_Centre", "Back_Side", "Back_Winger"))

#heatmap of our prediction
heatmap.2(tab, cellnote = tab, notecol = "black", Rowv = NA, Colv = NA, col = blues9, trace = "none", density.info = "none", srtCol = 45, margins = c(10,10))

agg = aggregate(df.subset[,2:7], list(df.subset$player_positions), mean)
agg = data.matrix(agg[,2:7])
agg = round(agg, digits = 1)
rownames(agg) = c("Fwd_Centre", "Fwd_Winger", "Mid_Centre", "Mid_Side", "Back_Centre", "Back_Side", "Back_Winger")

#heatmap depicting req. in various attributes to classify into a particular position.
heatmap.2(agg, cellnote = agg, notecol = "black", Rowv = NA, Colv = NA, col = blues9, trace = "none", density.info = "none", srtCol = 45, margins = c(10,10))
