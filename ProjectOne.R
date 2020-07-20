#1.	Examine the numeric variables for missing values.  For five of the variables with missing values  take appropriate action - eliminate variable, assign zero, average value, etc.  Justify your decision.


library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)


CompustatCompustatdata <- read_excel("C:/Users/shydhevi/Documents/R/Datamining/ProjectOne/AAERDATAHWPart1.xlsx")
head(CompustatCompustatdata)


# find the number of missing values of variable CRIM


names(CompustatCompustatdata) # print a list of variables to the screen.
t(t(names(CompustatCompustatdata)))# print the list in a useful column format

# change the column's name for missing columns
colnames(CompustatCompustatdata)[1:9] <- c("GVKEY","DNUM","CNUM","SMBL","NAICS","FYR","CONAME","yeara","Year and CNUM")  # change the first column's name


# Delete the first row
Compustatdata<-CompustatCompustatdata[-1,]
head(Compustatdata)

#Check the columns that has missing values 

sapply(Compustatdata, class)
#view(Compustatdata)
Compustatdata[, c(10:98)] <- sapply(Compustatdata[, c(10:98)], as.numeric)
sapply(Compustatdata, class)

#1.	Examine the numeric variables for missing values.

#Deleting Emply-RST
#The available values in Emply-RST is same as the Employees.
#And also Emply-RST has 12241 NA's and so we are dropping the variable
summary(Compustatdata$Employees)
summary(Compustatdata$`Emply-RST`)
Compustatdata <- select (Compustatdata,-c(`Emply-RST`))

#Deleting column Year and CNUM
#This is concatenation of columns CNUM and Year. Hence removing this variable
head(Compustatdata$`Year and CNUM`)
head(Compustatdata$GVKEY)
head(Compustatdata$yeara)
Compustatdata <- select (Compustatdata,-c(`Year and CNUM`))

# Replacing Missing values in dividents
#There are 74 missing values and the median is 0 so replacing NA's by 0s

summary(Compustatdata$Dividends)
Compustatdata$Dividends[is.na(Compustatdata$Dividends)] <- 0
summary(Compustatdata$Dividends)

# Removing the variable because of huge amount of missing data
summary(Compustatdata$WCChangTTL)
Compustatdata <- select (Compustatdata,-c(WCChangTTL))

#Replacing the Sales variable by average
Compustatdata$Sales[is.na(Compustatdata$Sales)] <- 0
summary(Compustatdata$Sales)

#2.	Look for redundancies and errors.

# SMBL and CONAME means the same hence remove one
summary(Compustatdata$SMBL)
head(Compustatdata$CONAME)
Compustatdata <- select (Compustatdata,-c(SMBL))

#EPSexcRst and EPSbasic are same and redundant hence remove one
head(Compustatdata$EPSexcRst)
head(Compustatdata$EPSbasic)
Compustatdata <- select (Compustatdata,-c(EPSexcRst))

#NetIncome and NetIncomRST are the same. But NetIncomRST has a lot of missing values. 
#Removing NetIncomRST
head(Compustatdata$NetIncome)
head(Compustatdata$NetIncomRST)
Compustatdata <- select (Compustatdata,-c(NetIncomRST))


#3.	Identify mean, count, sum, median, standard deviation for - Sales, Price_close, Employees variables

Compustatdata %>% summarise(n = n_distinct(Sales),
                            sd = sd(Sales),
                            mean = mean(Sales),
                            med = median(Sales, na.rm = TRUE))

summary(Compustatdata$PriceClose)
Compustatdata %>% summarise(n = n_distinct(PriceClose),
                            sd = sd(PriceClose),
                            mean = mean(PriceClose),
                            med = median(PriceClose, na.rm = FALSE))

str(Compustatdata$Employees)
Compustatdata %>% summarise(n = n_distinct(Employees),
                            sd = sd(Employees),
                            mean = mean(Employees),
                            med = median(Employees, na.rm = TRUE))


#4.	Identify outliers for - SALES, Price_close, Employees


boxplot(Compustatdata$Sales,outcol = "red", outcex = 1.5,main =" Sales")
boxplot(Compustatdata$PriceClose,outcol = "red", outcex = 1.5,main ="PriceClose")
boxplot(Compustatdata$Employees,outcol = "red", outcex = 1.5,main ="Employees")


#5.	Calculate
# a.	Skewness for Sales.  Is there evidence of SKEWNESS?
# b.	Skewness for Z-Score standardized SALES. s there evidence of SKEWNESS?



summary(Compustatdata$Sales)

hist(Compustatdata$Sales)
m <- mean(Compustatdata$Sales); s <- sd(Compustatdata$Sales)
z.weight <- (Compustatdata$Sales - m)/s

length(z.weight)
#Skewness
(3*(mean(Compustatdata$Sales) - median(Compustatdata$Sales)))/sd(Compustatdata$Sales)
(3*(mean(z.weight) - median(z.weight)))/sd(z.weight)



#6.	Normal probability plots
#a.	Construct a normal probability plot for Employees
#b.	Derive a new variable - CASH/Total Assets
#c.	Construct a normal probability plot for new Variable.



#6.	Normal probability plots
# Transformations for Normality 
summary(Compustatdata$Employees)
Compustatdata$Employees[is.na(Compustatdata$Employees)] <- 1
Compustatdata$Employees[Compustatdata$Employees == 0] <- 1

summary(Compustatdata$Employees)
sqrt.Employees <- sqrt(Compustatdata$Employees) # Square root
sqrt.Employees_skew <- (3*(mean(sqrt.Employees) - median(sqrt.Employees))) / sd(sqrt.Employees)
sqrt.Employees_skew
invsqrt.Employees <- 1 / sqrt(Compustatdata$Employees)

invsqrt.Employees_skew <- (3*(mean(invsqrt.Employees) - median(invsqrt.Employees))) /sd(invsqrt.Employees) # Histogram with Normal Distribution Overlay
par(mfrow=c(1,1))
invsqrt.Employees_skew
x <- rnorm(1000000, mean = mean (invsqrt.Employees), sd = sd(invsqrt.Employees))


hist(invsqrt.Employees,breaks=30,col = "lightblue",
     probability = "true",border = "black",xlab = "in sql rt of wt",ylab="counts",main = "histogram")
box(which = "plot", lty = "solid", col="black")
# Overlay with  Normal density
lines(density(x), col = "red")
#a.	Construct a normal probability plot for Employees
qqnorm(invsqrt.Employees , datax = TRUE, col = "red", 
       main = "Normal Q-Q Plot of Employees")
qqline(invsqrt.Employees , col = "blue", datax = TRUE)

#b.	Derive a new variable - CASH/Total Assets

Compustatdata$cashbyAsset <- Compustatdata$CashShortTermInvestments/Compustatdata$TotalAssets

summary(Compustatdata$cashbyAsset)



#c.	Construct a normal probability plot for new Variable.
sqrt.cashbyAsset <- sqrt(Compustatdata$cashbyAsset) # Square root
sqrt.cashbyAsset_skew <- (3*(mean(sqrt.cashbyAsset) - median(sqrt.cashbyAsset))) / sd(sqrt.cashbyAsset)
sqrt.cashbyAsset_skew
invsqrt.cashbyAsset <- 1 / sqrt(Compustatdata$cashbyAsset)

invsqrt.cashbyAsset_skew <- (3*(mean(invsqrt.cashbyAsset) - median(invsqrt.cashbyAsset))) /sd(invsqrt.cashbyAsset) # Histogram with Normal Distribution Overlay
par(mfrow=c(1,1))
invsqrt.cashbyAsset_skew
x <- rnorm(1000000, mean = mean (invsqrt.cashbyAsset), sd = sd(invsqrt.cashbyAsset))


hist(invsqrt.cashbyAsset,breaks=30,col = "lightblue",
     probability = "true",border = "black",xlab = "in sql rt of wt",ylab="counts",main = "histogram")
box(which = "plot", lty = "solid", col="black")
# Overlay with  Normal density
lines(density(x), col = "red")
#a.	Construct a normal probability plot for cashbyAsset
qqnorm(invsqrt.cashbyAsset , datax = TRUE, col = "red",  
       main = "Normal Q-Q Plot of cashbyAsset")
qqline(invsqrt.cashbyAsset , col = "blue", datax = TRUE)



#Part 1.2
Compustatdatapart2 <- read_excel("C:/Users/shydhevi/Documents/R/Datamining/ProjectOne/AAERDATAHWPart2-1.xlsx")
#View(Compustatdatapart2)
#1.	Ensure no missing values or errors.  Address this using methods recommended.  You will want to provide a short explanation for your choice.

sapply(Compustatdatapart2, class)
sum(is.na(Compustatdatapart2))
Compustatdatapart2[is.na(Compustatdatapart2)] <- 0
#2.	Determine which  variables are categorical and which are numeric?
str(Compustatdatapart2)
#3.	Standardize the data where relevant - ie you don't standardize GVKEY or SIC codes, year, or bktype

m <- mean(Compustatdatapart2$totval); s <- sd(Compustatdatapart2$totval)
z.weight <- (Compustatdatapart2$totval - m)/s
z.weight
length(Compustatdatapart2$totval)
# 4 Select two categorical variables and construct a bar chart - don't use GVKEY or CNUM as these are company identifiers
coname_freq <- table(Compustatdatapart2$CONAME)
barplot(coname_freq)

industry_freq <- table(Compustatdatapart2$industry)
barplot(industry_freq)

#5.	Construct a histogram of three numeric variables with an overlay of bankruptcy  variable (bktype)

library(ggplot2)
ggplot(data = Compustatdatapart2, aes(x=Compustatdatapart2$EBIt...66,fill=factor(Compustatdatapart2$bktype)))+
  geom_histogram()+
  scale_x_continuous("EBIt...66")+
  scale_y_discrete("Count")+
  guides(fill=guide_legend(title="bktype"))+
  scale_fill_manual(values=c("blue","red"))

ggplot(data = Compustatdatapart2, aes(x=Compustatdatapart2$Avg_GR,fill=factor(Compustatdatapart2$bktype)))+
  geom_histogram()+
  scale_x_continuous("Avg_GR")+
  scale_y_discrete("Count")+
  guides(fill=guide_legend(title="bktype"))+
  scale_fill_manual(values=c("blue","red"))

ggplot(data = Compustatdatapart2, aes(x=Compustatdatapart2$totval,fill=factor(Compustatdatapart2$bktype)))+
  geom_histogram()+
  scale_x_continuous("totval")+
  scale_y_discrete("Count")+
  guides(fill=guide_legend(title="bktype"))+
  scale_fill_manual(values=c("blue","red"))

# 6 Conduct a correlation analysis of the "Data" variables - exclude the calculated variables

Compustatdatacor <- Compustatdatapart2 %>% select(DATA9...32,DATA12...33,
                                                  DATA24...34,
                                                  DATA25...35,
                                                  DATA34x...36,
                                                  DATA36x...37,
                                                  DATA58...38,
                                                  DATA60...39,
                                                  DATA85...40,
                                                  DATA172...41,
                                                  DATA178...42,
                                                  DATA179...43,
                                                  DATA181...44,
                                                  DATA216...45,
                                                  DATA1...47,
                                                  DATA4...48,
                                                  DATA5...49,
                                                  DATA6...50,
                                                  DATA8x...51,
                                                  DATA9...52,
                                                  DATA12...53,
                                                  DATA24...54,
                                                  DATA25...55,
                                                  DATA34x...56,
                                                  DATA36x...57,
                                                  DATA58...58,
                                                  DATA60...59,
                                                  DATA85...60,
                                                  DATA172...61,
                                                  DATA178...62,
                                                  DATA179...63,
                                                  DATA181...64,
                                                  DATA216...65)
Compustatdatacor.corr <- cor(Compustatdatacor)
Compustatdatacor.corr
summary(Compustatdatacor.corr)
library(corrplot)
corrplot(Compustatdatacor.corr, method = "circle")
#8.Examine the difference in PCA results if you use only the Data variables versus using the constructed (calculated variables).

pcsdata <- prcomp(Compustatdatacor)
summary(pcsdata)

Compustatcalc <- Compustatdatapart2 %>% select(`Total Debt`,
                        MVEquity,
                        `LOG(DEFLTA)=LOG(100*(TA/CPI))`,
                        `MVE+TL`,
                       `D60+D181`,
                        `prc*data25+data6-data60`,
                        `MF/BF`,
                        `P/E`,
                        `Tobin'sNew`,
                         `ME/BE`,
                         `MVE/TL`,
                      `WC/NETSALES`,
                           `CL/TA`,
                        `TD/TA`,
                      `cash/CL`,
                      `WC/TA`,
                      `CASH/TA`,
                       `CA/CL`,
                   `NETSALES/TA`,
                     `NETINC/TA`,
                       `NETINC/FA`,
                      `OPINC/TA`,
                     `RETERN/TA`,
                       `EBIT/TA`,
                  `CASH/NETSALES`,
                    `CA/NETSALE`,
                        `CA/TA`)
pcscalcdata <- prcomp(Compustatcalc)

summary(pcscalcdata)
pcscalcdata$rot[,1:6]

#9.	Focus on the analysis using the calculated data.  How many Principal components should you use? Explain
# I would use first 6 principal components since 90% of variabililty is contained in those variables.

#10 Plot the factor scores for the number of principal components you identified in #9
pairs(pcscalcdata$rot[,1:6],
      labels = c("Component 6 Scores"))

