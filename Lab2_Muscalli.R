library(ggplot2)
library(modeest)

### set working directory
setwd("/Users/mickeymuscalli/Documents/Data Analytics/Lab2")

### read in data
NY.house.data <- read.csv("NY-House-Dataset.csv", header=TRUE)

View(NY.house.data)

##Create Initial Subset, removed massive price and SQFT outliers
NY.house.data.subset <- NY.house.data[NY.house.data$PRICE < 20000000 & NY.house.data$PROPERTYSQFT < 20000,]

NY.house.data.subset

## instantiate variables
NY.house.PRICE <- NY.house.data.subset$PRICE
NY.house.PROPERTYSQFT <- NY.house.data.subset$PROPERTYSQFT
NY.house.BEDS <- NY.house.data.subset$BEDS
NY.house.BATH <- NY.house.data.subset$BATH

NY.house.PRICE

## created linear model of log(Price) ~ log(PropertySqFt)
# find most common value
mfv(NY.house.data.subset$PROPERTYSQFT)[1]

# created secondary subset which removes most common SQFT value of 2184.208, weird and overly specific value
NY.house.data.subset.SQFT <- NY.house.data.subset[NY.house.data.subset$PROPERTYSQFT != mfv(NY.house.data.subset$PROPERTYSQFT)[1],]

lin.mod.SqFt <- lm(log10(NY.house.data.subset.SQFT$PRICE)~log10(NY.house.data.subset.SQFT$PROPERTYSQFT),NY.house.data.subset.SQFT)

summary(lin.mod.SqFt)

ggplot(NY.house.data.subset.SQFT, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.SqFt, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot (Log(Price) ~ Log(SqFt))', x='Fitted Values', y='Residuals')

## created linear model of Price ~ BEDS, created new subset (BEDS < 20)
NY.house.data.subset.BEDS <- NY.house.data.subset[NY.house.data.subset$BEDS < 20,]

lin.mod.BEDS <- lm(NY.house.data.subset.BEDS$PRICE~NY.house.data.subset.BEDS$BEDS,NY.house.data.subset.BEDS)

summary(lin.mod.BEDS)

ggplot(NY.house.data.subset.BEDS, aes(x = PRICE, y = BEDS)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.BEDS, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot Price ~ Beds', x='Fitted Values', y='Residuals')

## created linear model of Price ~ BATH, created new subset (BATH < 10)
NY.house.data.subset.BATH <- NY.house.data.subset[NY.house.data.subset$BATH < 10,]

lin.mod.BATH <- lm(NY.house.data.subset.BATH$PRICE~NY.house.data.subset.BATH$BATH,NY.house.data.subset.BATH)

summary(lin.mod.BATH)

ggplot(NY.house.data.subset.BATH, aes(x = PRICE, y = BATH)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.BATH, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot Price ~ Baths', x='Fitted Values', y='Residuals')
