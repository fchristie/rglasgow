require(tidyverse)

# generate dataframes
mat_no <- matrix(1:16, nrow = 4)
df_pets <- data.frame(
  names = c("Penny", "Spot", "Fluffy", "Chip", "Butch", "Tweet", "Scamp", "Molly",
    "Galaxy", "Bob", "Daisy", "Polly", "Max", "Oscar", "Flap", "Milo", "Cleo"),
  breed = c("Poodle", "Spaniel", "Persian", "Budgie", "Bulldog", "Parrot", "Spaniel",
    "Spaniel", "Tabby", "Budgie", "Bulldog", "Parrot", "Spaniel", "Tabby", "Budgie",
    "Spaniel", "Tabby"),
  animal = c("Dog", "Dog", "Cat", "Bird", "Dog", "Bird", "Dog", "Dog", "Cat", "Bird",
    "Dog", "Bird", "Dog", "Cat", "Bird", "Dog", "Cat"),
  price = c(500, 200, 450, 60, 360, 100, 250, 100, 200, 150, 390, 120, 400, 190,
    250, 300, 220),
  daily_cost = c(10, 5, 7, 1, 20, 10, 20, 5, 6, 2, 9, 3, 10, 8, 2, 9, 5),
  stringsAsFactors = FALSE
)

# define constants
kPetsNumeric <- c("price", "daily_cost")

###########
# iterating
###########
mean(mat_no[, 1])
mean(mat_no[, 2])
mean(mat_no[, 3])
mean(mat_no[, 4])

#####
# for
#####
for (i in 1:ncol(mat_no)) {
  print(mean(mat_no[, i]))
}

#############
# functionals
#############

# simple example function in R
AddOne <- function(x){
  x+1
}

AddOne(5)
AddOne(6)

# R is also vectorised
AddOne(1:10)

RIsFunctional <- function(f){
  f(1:10)
}

mean(1:10)
RIsFunctional(mean)

###########################
# apply family of functions
###########################

#######
# apply
#######
# sum the rows
apply(mat_no, 1, sum)
rowSums(mat_no)

# sum the columns
apply(mat_no, 2, sum)
colSums(mat_no)

# table each of the columns
apply(df_pets, 2, function(x) table(x))
apply(df_pets, 2, function(biscuit) table(biscuit))

# check for NA values
mat_no[3,3] <- NA
mat_no[2,4] <- NA
apply(mat_no, 1, function(x) table(is.na(x)))

########
# lapply
########
# find the mean of each of the numeric columns
lapply(kPetsNumeric, function(colname) mean(df_pets[, colname]))
mean(df_pets$price)
mean(df_pets$daily_cost)

# break the numeric vectors into ranges and table these
cut(1:9, breaks = seq(0, 10, by = 2))

lapply(kPetsNumeric, function(colname){
  x <- df_pets[, colname]
  x_cut <- cut(x, breaks = seq(min(x), max(x), by = (max(x) - min(x))/5))
  table(x_cut)
})

########
# sapply
########
# find the mean of each of the numeric columns
sapply(kPetsNumeric, function(colname) mean(df_pets[, colname]))
mean(df_pets$price)
mean(df_pets$daily_cost)

# be careful with sapply- look what this returns
sapply(kPetsNumeric, function(colname){
  x <- df_pets[, colname]
  x_cut <- cut(x, breaks = seq(min(x), max(x), by = (max(x) - min(x))/5))
  table(x_cut)
})

# plotting
par(mfrow = c(2, 2))
sapply(1:4, function(x) barplot(table(df_pets[, x]), las = 3))

#############
# fun example
#############
kBirthdayWords <- c("Happy", "Birthday", "To", "You", "Dear", "George", "Peacock")
kBirthdayWordOrder <- list(1:4, 1:4, c(1, 2, 5:7), 1:4)

invisible(sapply(kBirthdayWordOrder, function(line){
  cat(paste(kBirthdayWords[line]))
  cat("\n")
}))

#####
# map
#####
AddOne
map(1:3, AddOne)

map_dbl(1:3, mean)

mat_no <- data.frame(matrix(1:16, nrow = 4))
map_dfr(mat_no, AddOne)
map_dbl(mat_no, mean)
