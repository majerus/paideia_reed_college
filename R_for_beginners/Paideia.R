#------------------------------------------------------------------------------
#
# R for Beginners - Paideia 2015
# Albert Y. Kim and Kristin Bott
# Wednesday, January 21, 2014.
#
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#
# Learn by doing #0
#
#------------------------------------------------------------------------------
# Lines beginning with a hashtag are "comment" lines and are ignored when run
# in the console


#------------------------------------------------------------------------------
# Topic: Variables, assignment, and functions & their arguments
#------------------------------------------------------------------------------
5
count <- 5

# Can also be "chracter strings" i.e. words
name <- "Albert"

# Vectors are created using the "c" for "concatenate"
values <- c(1, 2, 3, 4, 5)
length(values)

# Vectors can have character strings as well
names <- c("Albert", "Kristin", "Albert")

# Matrices
mymatrix <- matrix(c(10, 15, 3, 29), nrow = 2)

# Functions: they have parentheses marking the arguments AKA inputs
sum(values)
sort(values)
sort(values, decreasing=TRUE)

# Help files:
# The help file for any command can be found by executing the following.  In
# particular the examples at the bottom are useful
?sort
help(sort)



#------------------------------------------------------------------------------
#
# Learn by doing #1
#
#------------------------------------------------------------------------------
# Change the "working directory" to match the one where our files are:
# -In the Files panel, navigate to the directory where your files are
# -Click on More -> Set As Working Directory

# Import the CSV file containing the grades using the read.csv() function.
# Setting header=TRUE indicates the first row consists of column names.
# grades is a "data frame" variable of both numerical and categorial variables.
grades <- read.csv(file="final_exam_grades.csv", header=TRUE)

# What do you think the following do?
dim(grades)
names(grades)
head(grades)

# You can pull individual varibles using the '$' operator
grades$grade

# Look at some summary statistics
mean(grades$grade)
sd(grades$grade)
median(grades$grade)
summary(grades$grade)


#--------------------------------------------------------------------
# Did students who handed in their final exams earlier do better?
#--------------------------------------------------------------------
# We now make a scatterplot (AKA bivariate plot) of final exam grade over time
plot(grades$time, grades$grade)

# It's good practice to label the axes and put a title. Note we can split a
# single command into multiple lines.  We need to run all three lines below to
# generate the plot
plot(grades$time, grades$grade,
     xlab="Time to complete exam (in min)",
     main="Final exam grade over time to completion")

# Let's add the "best" fitting line i.e. a regression line.  First we run the
# regression.
regression <- lm(grades$grade ~ grades$time)
summary(regression)

# Plot the regression line
abline(regression)

# Plot a thicker red line (lwd means 'line width')
abline(regression, col="red", lwd=3)


#--------------------------------------------------------------------
# Did the final exam grade vary much by major?
#--------------------------------------------------------------------
# We check this using a boxplot first for all majors.
boxplot(grades$grade)

# We split the boxplot by major.  Note the tilde "~" operator here.
table(grades$major)
boxplot(grades$grade ~ grades$major)

# Make horizontal boxplots
boxplot(grades$grade ~ grades$major, horizontal=TRUE)


#--------------------------------------------------------------------
# Exercises (to do below)
#--------------------------------------------------------------------
# 1. recreate the scatterplot with
# -an appropriate label to the y-axis.
# -an even thicker blue line
# then export the plot to PNG or PDF format by clicking "Export" in the Plot
# panel


# 2. Find a way to answer the question "Did the time to complete the
# final exam vary much by major?"



#------------------------------------------------------------------------------
#
# Learn by doing #2
#
#------------------------------------------------------------------------------
# Import the CSV file containing the OKC data using the read.csv() function.
profiles <- read.csv(file="profiles.csv", header=TRUE)

dim(profiles)
names(profiles)
head(profiles)

#--------------------------------------------------------------------
# What are the genders and sexual orientations of the users?
#--------------------------------------------------------------------
# Let's consider the sex of the users, which is a catergorial variable
profiles$sex

# This is overwhelming, let's count them
table(profiles$sex)

# To get proportions, we need a count of the number of users
n <- nrow(profiles)
table(profiles$sex) / n

# Generate a barplot to graph counts
barplot(table(profiles$sex), xlab="sex", ylab="count")

# Consider sexual orientation, another categorical variable
table(profiles$orientation)
table(profiles$orientation) / n
barplot(table(profiles$orientation), xlab="orientation", ylab="count")

# Now let's look at the crosstabs between the two variables via a "contingency"
# table
table(profiles$orientation, profiles$sex)
table(profiles$orientation, profiles$sex) / n

# One good way to visualize contigency tables is using a mosaicplot
mosaicplot(table(profiles$sex, profiles$orientation), xlab="gender",
           ylab="orientation", main="Sex vs Orientation")


#--------------------------------------------------------------------
# How tall is everyone?
#--------------------------------------------------------------------
# Let's look at heights using a histogram
hist(profiles$height, xlab="Height (in inches)")

# Some people listed their heights as under 55 inches (4'7'') and over 80 inches
# (6'8'').  I'm suspicious of this.  Let's keep people who are 55 inches or more
# and 80 inches or less

# People greater than or equal to 55 inches
profiles$height >= 55
table(profiles$height >= 55)

# Less than or equal to 80 inches
table(profiles$height <= 80)

# Equal to 72 inches.  Note the double "==" operator
table(profiles$height == 72)

# Let's consider people who satisfy BOTH our desired conditions using the AND
# operator "&"
profiles$height >= 55 & profiles$height <= 80
table(profiles$height >= 55 & profiles$height <= 80)

# Create subset of data consisting of only people with reasonable heights.
profiles.subset <-
  subset(profiles,
         profiles$height >= 55 & profiles$height <= 80)

# Let's compare the sizes of the original data frame and the new subsetted data
# frame
dim(profiles)
dim(profiles.subset)

# Let's generated a histogram to display the data.
hist(profiles.subset$height, xlab="Height (in inches)")

# Let's consider men and women's heights separately using the subset() command
# again.  i.e. take the subset of heights where sex is male.  Again, note the
# double "=="
male.heights <- subset(profiles.subset$height, profiles.subset$sex=='m')
female.heights <- subset(profiles.subset$height, profiles.subset$sex=='f')

# par(mfrow=c(2,1)) allows us to show two plots at once
par(mfrow=c(2,1))
hist(female.heights)
hist(male.heights)

# Hard to compare.  Let's make the x-axis range and the number of
# "buckets" in the histograms match
par(mfrow=c(2,1))
hist(female.heights, xlim=c(55, 80), breaks=25)
hist(male.heights, xlim=c(55, 80), breaks=25)


#--------------------------------------------------------------------
# Exercises (to do below)
#--------------------------------------------------------------------
# 1. Re-create the same mosaicplot but for drug use vs sex





# 2. Compare the ages of the male and female SF OkCupid populations







#------------------------------------------------------------------------------
#
# Learn by doing #3: Advanced; the purpose of this is to show how to
# install a package.
#
#------------------------------------------------------------------------------
# First install the "network" package
# -In the panel with Files, Plots, Packages, select Packages
# -Click on Install Packages
# -Search for network package and click Install

# Load the package
library(network)

# Import the 41 x 41 matrix of TRUE/FALSE's of whether or not pairs of people
# know each other
party <- read.csv(file="party.csv", header=TRUE)

# We need to rename the rows with the elements of the X variable, and then
# delete the column
rownames(party) <- party$X
party$X <- NULL
party <- as.matrix(party)

# The network() function from the "network" package generates the social network
# graph
graph <- network(party)

# Let's compute the number of people each person knows.  This can be done by
# summing the columns of the matrix using the "apply" command.
num.people.known <- apply(party, 2, sum)
num.people.known

# Let's plot it with the following arguments
# -using lines and not arrows
# -with labels
# -using pink lines
# -blow up the labels size by a factor of 3
# -pad the outside of the graph with a factor 2 of whitespace
# -let the dot sizes be proportional to the log of the number of people each
#  invitee knows
plot(graph, usearrows=FALSE, displaylabels=TRUE, edge.col = "pink",
     label.cex=3, pad=2,
     vertex.cex=log(num.people.known)
)

# The plot is a little cramped, so let's output to .png file
png("network.png", height=2000, width=2000)
plot(graph, usearrows=FALSE, displaylabels=TRUE, edge.col = "pink",
     label.cex=3, pad=2, vertex.cex=log(num.people.known)
)
dev.off()

