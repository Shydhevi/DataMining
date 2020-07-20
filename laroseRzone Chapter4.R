#read in the houses data set

houses <- read.csv(file="C:/Users/shydhevi/Documents/R/Datamining/ProjectOne/housesuse.csv",
                   stringsAsFactors = FALSE, header = FALSE)
names(houses) <- c("MVAL", "MINC", "HAGE", "ROOMS", "BEDRMS", "POPN" ,
                   "HHLDS", "LAT", "LONG")
head(houses)
houses$MINC_Z <- (houses$MINC - mean(houses$MINC))/(sd(houses$MINC))
houses$HAGE_Z <- (houses$HAGE - mean(houses$HAGE))/(sd(houses$HAGE))
# Do the same for the remaining variables
houses$MVAL_Z <- (houses$MVAL - mean(houses$MVAL))/(sd(houses$MVAL))
houses$ROOMS_Z <- (houses$ROOMS - mean(houses$ROOMS))/(sd(houses$ROOMS))
houses$BEDRMS_Z <- (houses$BEDRMS - mean(houses$BEDRMS))/(sd(houses$BEDRMS))
houses$POPN_Z <- (houses$POPN - mean(houses$POPN))/(sd(houses$POPN))
houses$HHLDS_Z <- (houses$HHLDS - mean(houses$HHLDS))/(sd(houses$HHLDS))
houses$LAT_Z <- (houses$LAT - mean(houses$LAT))/(sd(houses$LAT))
houses$LONG_Z <- (houses$LONG - mean(houses$LONG))/(sd(houses$LONG))


# Randomly select 90% for the Training dataset
choose <- runif(dim(houses)[1],0, 1)
test.house <- houses[which(choose < .1),]
train.house <- houses[which(choose <= .1),]
#REQUIRES LIBRARY PSYCH
library(psych)
pca1 <- principal(train.house[,c(10:17)],
                  nfactors=8,
                  rotate="none",
                  scores=TRUE)
#PCA VALUES
# Eigenvalues:
pca1$values
# Loadings matrix,
# variance explained,
pca1$loadings
#SCREE PLOT OF VALUES
plot(pca1$values,
     type = "b",
     main = "Scree Plot for Houses Data")
#PLOT FACTOR SCORES
pairs(~train.house$MINC+
         train.house$HAGE+pca1$scores[,3],
       labels = c("Median Income",
                  "Housing Median Age",
                  "Component 3 Scores"))
pairs(~train.house$MINC+
         train.house$HAGE+pca1$scores[,4],
       labels = c("Median Income",
                  "Housing Median Age",
                  "Component 4 Scores"))
#CALCULATE COMMUNNALITIES
comm3 <- loadings(pca1)[2,1]^2+
  loadings(pca1)[2,2]^2 + loadings(pca1)[2,3]^2
comm4 <- loadings(pca1)[2,1]^2+
  loadings(pca1)[2,2]^2+ loadings(pca1)[2,3]^2+
  loadings(pca1)[2,4]^2
comm3; comm4
#VALIDATION OF PCA
pca2 <-
  principal(test.house[,c(10:17)],
            nfactors=4,
            rotate="none",
            scores=TRUE)
pca2$loadings
#CONDUCT FACTOR ANALYSIS USING NEW DATA SET
adult <- read.csv(file="C:/Users/shydhevi/Documents/R/Datamining/ProjectOne/adult.txt",
                  stringsAsFactors = FALSE)
adult$"capnet"<- adult$capital.gain-adult$capital.loss
adult.s <- adult[,c(1,3,5,13,16)]
head(adult.s)
#standardize the data
# Standardize the data:
adult.s$AGE_Z <- (adult.s$age - mean(adult.s$age))/(sd(adult.s$age))
adult.s$DEM_Z <- (adult.s$demogweight - mean(adult.s$demogweight))/(sd(adult.s$demogweight))
head(adult.s$education.num)
adult.s$EDUC_Z <- (adult.s$education.num - mean(adult.s$education.num))/(sd(adult.s$education.num))
adult.s$CAPNET_Z <- (adult.s$capnet - mean(adult.s$capnet))/(sd(adult.s$capnet))
adult.s$HOURS_Z <- (adult.s$hours.per.week - mean(adult.s$hours.per.week))/(sd(adult.s$hours.per.week))
# Randomly select a Training dataset
choose <- runif(dim(adult.s)[1],0, 1)
test.adult <- adult.s[which(choose < .1), c(6:10)]
train.adult <- adult.s[which(choose >= .1), c(6:10)]
# bartlett's test for sphericity
# Requires package psych
library(psych)
corrmat1 <- cor(train.adult,
                method = "pearson")
cortest.bartlett(corrmat1,
                 n = dim(train.adult)[1])
#factor analysis with five components
# Requires psych, GPArotation
#install.packages("GPArotation")
library(GPArotation)
fa1 <- fa(train.adult, nfactors=2, max.iter = 200,  fm = "pa", rotate="none")
fa1$values # Eigenvalues
fa1$loadings # Loadings,
# proportion of variance,
# and cumulative variance
#factor analysis with two components
fa2 <- fa(train.adult, nfactors=2,
          fm = "pa", max.iter = 200,
          rotate="none")
fa2$values # Eigenvalues
fa2$loadings # Loadings
fa2$communality # Communality
#varimax rotation
fa2v <- fa(train.adult,
           nfactors = 2,
           fm = "pa", max.iter = 200,
           rotate="varimax")
fa2v$loadings
fa2v$communality
#user defined composites
small.houses <- houses[,c(4:7)]
a <- c(1/4, 1/4, 1/4, 1/4)
W <- t(a)*small.houses
W
