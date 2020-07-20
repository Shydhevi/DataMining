library(tidyverse)
library(readxl)
#read in churn data
churn <- read.csv(file = "C:/Users/shydhevi/Documents/R/Datamining/ProjectOne/churn-1.txt",
                  stringsAsFactors=TRUE)
# Show the first ten records
churn[1:10,]
# Summarize the Churn variable
sum.churn <- summary(churn$Churn)
sum.churn
# Calculate proportion of churners
prop.churn <- sum(churn$Churn ==
                    "True") / length(churn$Churn)
prop.churn
# Bar chart of variable Churn
plot.new()
barplot(sum.churn,
        ylim = c(0, 3000),
        main = "Bar Graph of Churners and
Non-Churners",
        col = "lightblue")
box(which = "plot",
    lty = "solid",
    col="black")
# Make a table for counts of Churn and International Plan
counts <- table(churn$Churn,churn$Intl.Plan,
                dnn=c("Churn", "International Plan"))
counts
#Overlayed bar chart
barplot(counts,
        legend = rownames(counts),
        col = c("blue", "red"),
        ylim = c(0, 3300),
        ylab = "Count",
        xlab = "International Plan",
        main = "Comparison Bar Chart:
Churn Proportions by
International Plan")
box(which = "plot",
    lty = "solid",
    col="black")
# Create a table with sums for both variables
sumtable <- addmargins(counts,
                       FUN = sum)

sumtable
# Create a table of proportions over rows
row.margin <- round(prop.table(counts,
                               margin = 1),
                    4)*100
row.margin
# Create a table of proportions over columns
col.margin <- round(prop.table(counts,
                               margin = 2),
                    4)*100
col.margin
# Clustered Bar Chart, with legend
barplot(counts,
        col = c("blue", "red"),
        ylim = c(0, 3300),
        ylab = "Count",
        xlab = "International Plan",
        main = "Churn Count by
International Plan",
        beside = TRUE)
legend("topright",
       c(rownames(counts)),
       col = c("blue", "red"),
       pch = 15,
       title = "Churn")
box(which = "plot",
    lty = "solid",
    col="black")
# Clustered Bar Chart of Churn and International Plan with legend
barplot(t(counts),
        col = c("blue", "green"),
        ylim = c(0, 3300),
        ylab = "Counts",
        xlab = "Churn",
        main = "International Plan Count by
Churn",
        beside = TRUE)
legend("topright",
       c(rownames(counts)),
       col = c("blue", "green"),
       pch = 15,
       title = "Int'l Plan")
box(which = "plot",
    lty = "solid",
    col="black")
# Histogram of non-overlayed Customer Service Calls
hist(churn$CustServ.Calls,
     xlim = c(0,10),
     col = "lightblue",
     ylab = "Count",
     xlab = "Customer Service Calls",
     main = "Histogram of Customer Service
Calls")
# Download and install the R Package ggplot2
#install.packages("ggplot2")
# Pick any CRAN mirror
# (see example image)
# Open the new package
library(ggplot2)
# Overlayed bar charts
ggplot() +
  geom_bar(data = churn,
           aes(x = factor(churn$CustServ.Calls),
               fill = factor(churn$Churn)),
           position = "stack") +
  scale_x_discrete("Customer Service
Calls") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="Churn"))
+
  scale_fill_manual(values=c("blue",
                             "red"))
ggplot() +
  geom_bar(data=churn,
           aes(x = factor(churn$CustServ.Calls),
               fill = factor(churn$Churn)),
           position = "fill") +
  scale_x_discrete("Customer Service Calls")
+
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="Churn")) +
  scale_fill_manual(values=c("blue", "red"))
# Two-sample T-Test for Int'l Calls
# Partition data
churn.false <- subset(churn,
                      churn$Churn ==
                        "False")
churn.true <- subset(churn,
                     churn$Churn ==
                       "True")
# Run the test
t.test(churn.false$Intl.Calls,
       churn.true$Intl.Calls)
# Scatterplot of Evening Minutes and Day Minutes, colored by Churn
plot(churn$Eve.Mins,
     churn$Day.Mins,
     xlim = c(0, 400),
     ylim = c(0, 400),
     xlab = "Evening Minutes",
     ylab = "Day Minutes",
     main = "Scatterplot of Day
and Evening Minutes by
Churn",
     col = ifelse(churn$Churn==
                    "True",
                  "red",
                  "blue"))
legend("topright",
       c("True",
         "False"),
       col = c("red",
               "blue"),
       pch = 1,
       title = "Churn")

# Scatterplot of Day Minutes and Customer Service Calls, colored by
#Churn
plot(churn$Day.Mins,
     churn$CustServ.Calls,
     xlim = c(0, 400),
     xlab = "Day Minutes",
     ylab = "Customer Service Calls",
     main = "Scatterplot of Day Minutes and
Customer Service Calls by Churn",
     col = ifelse(churn$Churn=="True",
                  "red",
                  "blue"),
     pch = ifelse(churn$Churn=="True",
                  16, 20))
legend("topright",
       c("True",
         "False"),
       col = c("red",
               "blue"),
       pch = c(16, 20),
       title = "Churn")
# Scatterplot matrix
pairs(~churn$Day.Mins +
         churn$Day.Calls +
         churn$Day.Charge)
# Regression of Day Charge vs Day Minutes
fit <- lm(churn$Day.Charge ~
          churn$Day.Mins)
names(churn)
summary(fit)
# Correlation values, with p-values
days <- cbind(churn$Day.Mins,
              churn$Day.Calls,
              churn$Day.Charge)
MinsCallsTest <- cor.test(churn$Day.Mins,
                          churn$Day.Calls)
MinsChargeTest <- cor.test(churn$Day.Mins,
                           churn$Day.Charge)
CallsChargeTest <- cor.test(churn$Day.Calls,
                            churn$Day.Charge)
round(cor(days),
      4)
MinsCallsTest$p.value
MinsChargeTest$p.value
CallsChargeTest$p.value
# Correlation values and p-values in matrix form
# Collect variables of interest
corrdata <-
  cbind(churn$Account.Length,
        churn$VMail.Message,
        churn$Day.Mins,
        churn$Day.Calls,
        churn$CustServ.Calls)
# Declare the matrix
corrpvalues <- matrix(rep(0, 25),
                      ncol = 5)
# Fill the matrix with correlations
for (i in 1:4) {
  for (j in (i+1):5) {
    corrpvalues[i,j] <-
      corrpvalues[j,i] <-
      round(cor.test(corrdata[,i],
                     corrdata[,j])$p.value,
            4)
  }
}
round(cor(corrdata), 4)
corrpvalues



