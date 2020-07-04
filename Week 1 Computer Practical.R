### Importing Data into R
# Read in financial data
data <- read.csv("Financial data.csv", header = T)
# First 6 rows
head(data)
# Last 2 rows
tail(data, n= 2)
# Rows, columns of dataset
dim(data)

### Summarizing Data
# Summary statistics for mv column
summary(data$mv)
# Variance of mv column
var(data$mv)
# Order years data was collected
sort(unique(data$year))
# Number of years data was collected
length(unique(data$year))
# Number of companies data was collected from
length(unique(data$id))

### Calculations Using R
# Creating new column called totexp
data$totexp <- data$ads + data$rd
# Seeing if column was created
head(data, 2)
# Creating new column called pperatio
data$pperatio <- data$ppe / data$assets
# List of column names
names(data)

### Getting help in R
help(sort)
?sort

### Visualizing Data
# Histogram of pperatio column
# nclass increases number of intervals
hist(x = data$pperatio,
     xlab = "PPE ratio",
     main = "PPE ratio for all the data",
     col = 2,
     nclass = 50)

# Loading R package lattice
require(lattice)
# Lattice plot
histogram(~as.numeric(data$pperatio) | as.factor(data$year),
          xlab = "PPE ratio",
          main = "PPE ratio by financial year")