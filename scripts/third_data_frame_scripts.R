# We will be talking about data.frames

# Let's import some data
download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")
library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")

#read table

#the first items in the table
head(surveys)

view(surveys)
str(surveys)

#First number of result of dim is rows and second number is columns
dim(surveys)

#number of columns
ncol(surveys)

#last items in the data
tail(surveys)

names(surveys)
#equivalent to
colnames(surveys)

rownames(surveys)

summary(surveys)

#Indexing and subsetting
surveys[1,6]
#[row,column]

#returns the whole row
surveys[1, ]

#returns the whole column
surveys[ ,1]

#returns part of a table
surveys[c(1,2,3),c(5,6)]
surveys[1:3,5:6]

#returns everything except the first column
surveys[, -1]
surveys[,"sex"]
surveys["sex"]
surveys$plot_id

surveys_200_lec <- surveys[200,]
nrow(surveys)

lastrow <- surveys[nrow(surveys),]
