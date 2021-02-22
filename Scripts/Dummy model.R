# Inputs
# Baseline immunity (named vector, names corresponding to lower limits of age groups and vector to levels of immunity.
# Doesn't seem to work if 0 is included?
baseline.immunity <- c('1'=0.5, '5'=0.6, '10'=0.8, '15'=0.8, '25'=0.8, '35'=0.8, '45'=0.8)

# Baseline year (corresponds to a column in the coverage argument) = 1.
# Do any adaptations need to be made to account for monthly steps?
# Year to project to = 2 and 3
# Coverage (matrix in which each row is a dose and each named column is a year)
coverage <- cbind(c(0.8,0.9), c(0.7,0.6), c(0.8, 0.8))
colnames(coverage)<- c("1","2","3")
rownames(coverage)<- c("1","2")
# Schedule (ages at which vaccines are given, in years. Is there a way to account for months- give all ages in months instead?) = 1,2
# Maternal immunity (does this mean immunity in the lowest age group, or the highest?) = 0.5
# Efficacy = 0.91

# Year 1 to Year 2 
library(epimixr)

Y2<- project_immunity(baseline.immunity, 1, 2, coverage = coverage, schedule = c(1,2), 0.5, 0.91)
# This appears to add an age category- the lowest? 

# Year 2 to Year 3
# Does maternal immunity need to change here? 
Y3<- project_immunity(Y2, 2, 3, coverage = coverage, schedule = c(1,2), 0.5, 0.91)

baseline.immunity <- c('0'=NA,'1'=0.5, '5'=0.6, '10'=0.8, '15'=0.8, '25'=0.8, '35'=0.8, '45'=0.8)
Y1_3<- data.frame(baseline.immunity, Y2, Y3)
View(Y1_3)
# Clarify what is happening with lowest age category and maternal immunity