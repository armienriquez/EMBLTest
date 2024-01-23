weight_g <- c(50,60,65,82)

animals <- c("dog","cat","mouse")
#get the length of the vector
length(animals)

#get the type of data contained in the vector
class(animals)
class(weight_g)

#structure of the object
str(animals)

#add an element to the beginning of the vector
animals <- c("cincilla",animals)

#add an element to the end of the vector
animals <- c(animals,"frog")

#how to find out the data type of an object
typeof(animals)

num_char <- c(1,2,3,"a")
num_logical <- c(1,2,3,TRUE)
char_logical <- c("a","b","c",TRUE)
tricky <- c(1,2,3,"4")

# hierarchy is logical to numeric to character 

#subsetting a vector
animals[2]
animals[c(1,2)]
more_animals <- animals[c(1,2,3,2,1,4)]

weight_g
weight_g[c(FALSE, FALSE, TRUE, TRUE)]
weight_g > 63
weight_g[weight_g>63]
weight_g[weight_g > 63 & weight_g <80]
weight_g[weight_g<58 | weight_g > 80]

# <, >, ==, !=, <=, >=, !

weight_g==65

animals[animals =="rat"| animals=="frog"]

# %in% helps us find all elements corresponding to a vector of elements
# of our choice regardless of position

animals %in% c("rat","frog","duck","dog")

# An example of a vector with missing data
heights <- c(2,4,4,NA,6)
mean(heights)
mean(heights,na.rm=TRUE)

is.na(heights)
heights[!is.na(heights)]
str(heights)

#omit the missing data
na.omit(heights)

#extract the complete cases
heights[complete.cases(heights)]

heights<-c(63,69,60,65,NA,68,61,70,61,59,64,69,63)
heights_no_na <- na.omit(heights)
median(heights_no_na)

#This function counts the number of people taller than 67 inches
length(heights_no_na[heights_no_na>67])

#this coerces the logicals into numerical because it is passed to the sum function
sum(heights_no_na>67)

