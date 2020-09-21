---
title: "Galtech PCA"
author: "Carlos Monsivais"
date: "July 30, 2018"
output: html_document
---
#Data Summary
```{r}
library(readxl)
#We need this library to use the function read_excel below to read in the excel file with our data.

url <- "file:///C:/Users/carlos.monsivais/Desktop/PCA Galtech.xlsx"
destfile <- "PCA_Galtech.xlsx"
curl::curl_download(url, destfile)
PCA_Galtech <- read_excel(destfile)
#This is the code we are using to read in the Excel file. 

data = PCA_Galtech
#Renaming the data set from PCA_Galtech to just data for simplicity.

str(data)
#This is the structure of what the data looks like. We can see what the variables are like in how we can see that some are numerical, factor variables, or even characters if there are any.

summary(data)
#Here we can get summary statistics for each variable in the data set.

data$Year = as.factor(data$Year)
#We are creating the variable to be a factor variable which means it will be considered as categorical.

data$Month = as.factor(data$Month)
#We are creating the variable to be a factor variable which means it will be considered as categorical.

str(data)
#Now we are looking at the structure of the data to see that the variable type changed in the sense of going from the variable type that it was to a factor variable.
```

#Seperating data by 2017 and 2018 data
```{r}
#2017
data2017 = data.frame(data[1:116,])
#Here I am manually separating the 2017 data and creating a variable for only the 2017 data. I want to do this to independently analyze the 2017 year.

complete.cases(data2017)
#This function will tell us which rows have complete cases in the sense of all the data and variables being filled out.

data2017 = data2017[complete.cases(data2017),]
#This command is telling all the 2017 data we have subset to only keep the rows that are complete. This helps when analyzing the data to not have missing or incomplete data sets.

#2018
data2018 = data.frame(data[117:232,])
#Here I am manually separating the 2018 data and creating a variable for only the 2018 data. I want to do this to independently analyze the 2018 year.

complete.cases(data2018)
#This function will tell us which rows have complete cases in the sense of all the data and variables being filled out.

data2018 = data2018[complete.cases(data2018),]
#This command is telling all the 2018 data we have subset to only keep the rows that are complete. This helps when analyzing the data to not have missing or incomplete data sets.
```

#Correlation Coefficients
```{r}
#2017
data2017 = data2017[, -c(1,3,4,8,9)]
#Here I am manually removing variables that have very high correlation with one another because of them being functions of another variable or variables that have correlations of 0.99 for the 2017 data.

cor_data2017 = round(cor(data2017), 2)
#I am taking the correlation values of the remaining variables where I am rounding the correlation values to the hundredth's place.

top2017 = cor_data2017
#I am renaming the variable with the correlation matrix as top2017.

top2017[upper.tri(cor_data2017)] = ""
#I am assigning a blank space to the top half of the correlation matrix because the upper triangle of the correlation matrix is the values repeated.

top2017 = as.data.frame(top2017)
#We are creating the correlation matrix into a data frame for organizational purposes.

top2017
#Here is the printed correlation matrix.

#2018
data2018 = data2018[, -c(1,3,4,8,9)]
#Here I am manually removing variables that have very high correlation with one another because of them being functions of another variable or variables that have correlations of 0.99 for the 2018 data.

cor_data2018 = round(cor(data2018), 2)
#I am taking the correlation values of the remaining variables where I am rounding the correlation values to the hundredth's place.

top2018 = cor_data2018
#I am renaming the variable with the correlation matrix as top2018.

top2018[upper.tri(cor_data2018)] = ""
#I am assigning a blank space to the top half of the correlation matrix because the upper triangle of the correlation matrix is the values repeated.

top2018 = as.data.frame(top2018)
#We are creating the correlation matrix into a data frame for organizational purposes.

top2018
#Here is the printed correlation matrix.
```

#Principal Component Analysis
```{r}
#2017
principal_component2017 = prcomp(data2017, center = TRUE, scale.=TRUE) 
#Here we are running our 2017 data through the Principal Component Analysis algorithm. We want our data to be centered at 0 for simplicity reasons since 0 is a value we calculate the absolute value from. Also, we want our data to be scaled to be able to compare the data equivalently. 

important.PC2017 = data.frame(principal_component2017$sdev^2)
#We are using Kaiser's Rule which is a rule of thumb for deciding a suitable cutoff as to which Principal Components we should use and look at. This rule is that the Principal Components that we should consider are any with an eigenvalue greater than 1. As a result, this code calculates the eigenvalues of each component.

rownames(important.PC2017) = c("PC1", "PC2", "PC3", "PC4")
#These will be the row names for the table I will create displaying which Principal Components to use in the analysis.

colnames(important.PC2017) = c("Eigenvalues")
#These will be the column names for the table I will create displaying which Principal Components to use in the analysis.

important.PC2017
#Here is the table to see which Principal Components to use. We are using Kaiser's Rule where any of these eigenvalues above that are greater than 1 are Principal Components we should consider for the analysis.

print(principal_component2017)
#Rotations are the same as loadings which are the correlation values each variable corresponds with the principal component. They tell you how strong the association is between the variable and the component.

summary(principal_component2017)
#Here we can see how much and what proportion of the variability is explained by each Principal Components in 2017.

varimax2017 = varimax(principal_component2017$rotation[,1:2])
#Varimax is a change of the coordinates that summarizes the sum of the variances of the squared loadings. It's goal is to clean up the rotations that we take from prcomp. We are going to get a rotation on top of the original rotation. This tells us which variables are the most important in each component. You want to do it on the Principal Components that passed the Kaiser's Rule because those are the variables that we want to look at the most.

#2018
principal_component2018 = prcomp(data2018, center = TRUE, scale.=TRUE) 
#Here we are running our 2018 data through the Principal Component Analysis algorithm. We want our data to be centered at 0 for simplicity reasons since 0 is a value we calculate the absolute value from. Also, we want our data to be scaled to be able to compare the data equivalently. 

important.PC2018 = data.frame(principal_component2018$sdev^2)
#We are using Kaiser's Rule which is a rule of thumb for deciding a suitable cutoff as to which Principal Components we should use and look at. This rule is that the Principal Components that we should consider are any with an eigenvalue greater than 1. As a result, this code calculates the eigenvalues of each component.

rownames(important.PC2018) = c("PC1", "PC2", "PC3", "PC4")
#These will be the row names for the table I will create displaying which Principal Components to use in the analysis.

colnames(important.PC2018) = c("Eigenvalues")
#These will be the column names for the table I will create displaying which Principal Components to use in the analysis.

important.PC2018
#Here is the table to see which Principal Components to use. We are using Kaiser's Rule where any of these eigenvalues above that are greater than 1 are Principal Components we should consider for the analysis.

print(principal_component2018)
#Rotations are the same as loadings which are the correlation values each variable corresponds with the principal component. They tell you how strong the association is between the variable and the component.

summary(principal_component2018)
#Here we can see how much and what proportion of the variability is explained by each Principal Components in 2018.

varimax2018 = varimax(principal_component2018$rotation[,1:2])
#Varimax is a change of the coordinates that summarizes the sum of the variances of the squared loadings. It's goal is to clean up the rotations that we take from prcomp. We are going to get a rotation on top of the original rotation. This tells us which variables are the most important in each component. You want to do it on the Principal Components that passed the Kaiser's Rule because those are the variables that we want to look at the most.
```

#Principal Component Analysis Effects on Correlation
```{r}
#2017
cor_training2017 = round(cor(principal_component2017$x),2)
#Here I am looking at the correlation value between the principal component values where I am rounding the values off at the hundredths place to create a correlation matrix.

top2017 = cor_training2017
#Renaming the correlation matrix to top2017.

top2017[upper.tri(cor_training2017)] = ""
#I am assigning a blank space to the top half of the correlation matrix because the upper triangle of the correlation matrix is the values repeated.

top2017 = as.data.frame(top2017)
#We are creating the correlation matrix into a data frame for organizational purposes for the 2017 data.

top2017
#Here is the printed correlation matrix. We should see that all the Principal Components have a correlation value of 0 because that's one of the traits of Principal Component Analysis, it decorrelates the variables in the data set.

#2018
cor_training2018 = round(cor(principal_component2018$x),2)
#Here I am looking at the correlation value between the principal component values where I am rounding the values off at the hundredths place to create a correlation matrix.

top2018 = cor_training2018
#Renaming the correlation matrix to top2018.

top2018[upper.tri(cor_training2018)] = ""
#I am assigning a blank space to the top half of the correlation matrix because the upper triangle of the correlation matrix is the values repeated.

top2018 = as.data.frame(top2018)
#We are creating the correlation matrix into a data frame for organizational purposes for the 2018 data.

top2018
#Here is the printed correlation matrix. We should see that all the Principal Components have a correlation value of 0 because that's one of the traits of Principal Component Analysis, it decorrelates the variables in the data set.
```

#Graphing the Bi-PLot
```{r}
#2017
bi_plot2017 = biplot(principal_component2017, choices = 1:2)
#Here we are plotting a bi plot of the Principal Components that passed Kaiser's Rule. In this case it's Principal Component 1 and Principal Component 2. Therefore, we will plot Principal Components 1 and 2 and see how the vectors from the Principal Component interact with each other. The closer the vectors, the higher correlation they have, the farther away the vectors are , the lower the correlation they have. This bi plot is for the 2017 data.

#2018
bi_plot2018 = biplot(principal_component2018, choices = 1:2)
#Here we are plotting a bi plot of the Principal Components that passed Kaiser's Rule. In this case it's Principal Component 1 and Principal Component 2. Therefore, we will plot Principal Components 1 and 2 and see how the vectors from the Principal Component interact with each other. The closer the vectors, the higher correlation they have, the farther away the vectors are , the lower the correlation they have. This bi plot is for the 2018 data.
```
