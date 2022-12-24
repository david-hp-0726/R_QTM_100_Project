######################################################################### 
#### Group 8 Final Project
#### Date: 11/25/2022 
#### Authors: Henry Lee, Michelle Sauceda, David Chen 
######################################################################### 

#################### Import Packages and Dataset #################### 
library(tidyr)
library(dplyr)
library(ggplot2)
setwd("/Users/hp/Desktop/QTM/QTM 100 Lab/FinalProject") # your working directory
nsyr <- read.csv("NSYR_data.csv")



#################### Recoding Process ####################

######## Create New Variables ########
#### Variable: earnings
# see how many categories the variable has
table(nsyr$earnings)

# define a dictionary that takes each income category and returns the middle
# value of that category
dict_earnings <- c(
  "$8,001 - $10,000"= 9000,
  "$4,001 - $6,000" = 5000,
  "$6,001 - $8,000" = 7000,
  "$2,001 - $4,000" = 3000,
  "$2,000 or Less" = 1000,
  "No income" = 0,
  "$18,001 - $20,000" = 19000,
  "$16,001 - $18,000" = 17000,
  "$14,001 - $16,000" = 15000,
  "$12,001 - $14,000" = 13000,
  "$10,001 - 12,000" = 11000, # typo made by the dataset
  "$28,001 - $30,000" = 29000,
  "$26,001 - $28,000" = 27000,
  "$24,001 - $26,000" = 25000,
  "$22,001 - $24,000" = 23000,
  "$20,001 - $22,000" = 21000,
  "$38,001 - $40,000" = 39000,
  "$36,001 - $38,000" = 37000,
  "$34,001 - $36,000" = 35000,
  "$32,001 - $34,000" = 33000,
  "$30,001 - $32,000" = 31000,
  "$48,001 - $50,000" = 49000,
  "$46,001 - $48,000" = 47000,
  "$44,001 - $46,000" = 45000,
  "$42,001 - $44,000" = 43000,
  "$40,001 - $42,000" = 41000,
  "$50,001 or more" = 51000 
  )

# create a new column using the dictionary
nsyr$earnings_num <- dict_earnings[nsyr$earnings]

#### Variable: earnings
# see how many categories the variable has
table(nsyr$debt)

# define a dictionary that takes each debt category and returns the middle value
dict_debt <- c(
  "No debt" = 0,
  "$200 or less" = 100,
  "$201 - $400" = 300,
  "$401 - $600" = 500,
  "$601 - $800" = 700,
  "$801 - $1,000" = 900,
  "$1,001 - $1,200" = 1100,
  "$1,201 - $1,400" = 1300,
  "$1,401 - $1,600" = 1500,
  "$1,601 - $1,800" = 1700,
  "$1,801 - $2,000" = 1900,
  "$2,001 - $2,200" = 2100,
  "$2,201 - $2,400" = 2300,
  "$2,401 - $2,600" = 2500,
  "$2,601 - $2,800" = 2700, 
  "$2,801 - $3,000" = 2900,
  "$3,401 - $3,600" = 3500,
  "$3,601 - $3,800" = 3700,
  "$3,801 - $4,000" = 3900,
  "$4,001 - $4,200" = 4100,
  "$4,201 - $4,400" = 4300,
  "$4,401 - $4,600" = 4500,
  "$4,601 - $4,800" = 4700,
  "$4,801 - $5,000" = 4900,
  "$5,001 - $5,200" = 5100,
  "$5,801 - $6,000" = 5900,
  "$6,601 - $6,800" = 6700,
  "$6,801 - $7,000" = 6900,
  "$7,801 - $8,000" = 7900,
  "$8,001 - $8,200" = 8100,
  "$8,801 - $9,000" = 8900,
  "$9,401 - $9,600" = 9500,
  "$9,801 - $10,000" = 9900,
  "$10,001 or more" = 1100
)

# create a new column using the dictionary
nsyr$debt_num <- dict_debt[nsyr$debt]

#### Variable: FAITH1 
# transform values to a likert scale where 5 corresponds to "Extremely important"
# and 1 corresponds to "Not important at all"
dict_faith <- c(
  "Not important at all" = 1, 
  "Not very" = 2, 
  "Somewhat" = 3,
  "Very" = 4,
  "Extremely important" = 5
)

# create a new column using the dictionary
nsyr$faith_num <- dict_faith[nsyr$FAITH1]

#### Variable: compgrad
# create a dictionary that groups observations based on degree rather than
# year of school
dict_edu <- c(
  "1st grade" = "Below High School", 
  "2nd grade" = "Below High School", 
  "3rd grade" = "Below High School",  
  "4th grade" = "Below High School", 
  "5th grade" = "Below High School", 
  "6th grade" = "Below High School",  
  "7th grade" = "Below High School", 
  "8th grade" = "Below High School", 
  "9th grade" = "High School", 
  "10th grade" = "High School",
  "11th grade" = "High School", 
  "12th grade" = "High School",
  "One year of vocational/technical school" = "Technical School",
  "Two years of vocational/technical school" = "Technical School",
  "Three years of vocational/technical school" = "Technical School",
  "One year of college" = "College",
  "Two years of college" = "College",
  "Three years of college" = "College",
  "Four years of college/grad school" = "College",
  "Five years of college/grad school" = "College",
  "Six years of college/grad school" = "College"
)

# create a new factor variable
nsyr$edu <- factor(NA, levels = c(
  "Below High School", "High School", "Technical School", "College"
))

# use the dictionary to create a new column
nsyr$edu <- dict_edu[nsyr$compgrad]


# reorder the cateogories
nsyr$edu <- factor(nsyr$edu, levels = c(
  "Below High School", "High School", "Technical School", "College"
))

#### Variable: god
# combine "No" and "Unsure" categories
nsyr$god[nsyr$god %in% c("No", "Unsure/Don't know")] <- "No/Unsure"


######## Data Cleaning ########
# Replace Missing Values with NA 
nsyr$attreg[nsyr$attreg == "." | nsyr$attreg == ""] <- NA
nsyr$ATTEND1[nsyr$ATTEND1 == "." | nsyr$ATTEND1 == ""] <- NA
nsyr$FAITH1[nsyr$FAITH1 == "." | nsyr$FAITH1 == ""] <- NA
nsyr$god[nsyr$god == "." | nsyr$god == ""] <- NA

# Add a new category to "ATTEND1" to represent subjects who never partitipate in
# any religious service (subjects who answered no to "attreg")
nsyr$ATTEND1[nsyr$attreg == "No"] <- "Never"

# reorder "ATTEND1" based on frequency of involvement
nsyr$ATTEND1 <- factor(nsyr$ATTEND1, levels = c(
  "Never", "A few times a year", "Many times a year", "Once a month", 
  "2-3 times a month", "Once a week", "More than once a week"
))

# change the dtype of "bmi" to numeric
nsyr$bmi <- as.numeric(nsyr$bmi)

# Remove rows containing NA's
nsyr <- nsyr %>% drop_na(earnings_num, debt_num, attreg, 
                         ATTEND1, FAITH1, god, edu, bmi)

#################### Plots #################### 

#### Barplot (Belief in God vs Education Level) 
ggplot(data = nsyr) +
  geom_bar(aes(x = edu, fill = god), position = "fill") +
  xlab("Education Level") +
  ylab("Proportion") +
  scale_fill_discrete(name = "Do you believe in God") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


#### Boxplot (Income vs Involvement in Religious Service)
ggplot(data = nsyr) +
  geom_boxplot(aes(x = attreg, y = earnings_num, fill = attreg), 
               show.legend = FALSE) +
  xlab("Do you attend religious services more than once a year?") +
  ylab("Annual Income (in USD)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"))


#### Boxplot (Income vs Frequency of Involvement in Religious Service)
ggplot(data = nsyr) +
  geom_boxplot(aes(x = ATTEND1, y = earnings_num, fill = ATTEND1), 
               show.legend = FALSE) +
  xlab("How often do you attend religious services") +
  ylab("Annual Income (in USD)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 7))


#### Scatter Plot (BMI vs Importance of Religious Belief)
ggplot(data = nsyr, aes(x = faith_num, y = as.numeric(bmi))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  xlab("How important do you think religious belief is in your life") +
  ylab("BMI") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"))


#################### Tests #################### 
#### Q1: Education Level vs Belief in God
# Check conditions
# 1. Independence across groups -> Yes, can be assumed
# 2. Large expected cell counts -> Yes, as shown below
chisq.test(nsyr$edu, nsyr$god)$exp

# two-way table for the two variables
table(nsyr$edu, nsyr$god)

# conduct the chi-square test
chisq.test(nsyr$edu, nsyr$god)


#### Q2: Involvement in Religious Services vs Income
# Check conditions
# 1. Independence between groups -> Yes, can be assumed
# 2. Normality -> Yes, both sample sizes exceed 30 

# conduct the two-sample t test
yes <- nsyr$earnings_num[nsyr$attreg=="Yes"]
no <- nsyr$earnings_num[nsyr$attreg=="No"]
t.test(yes, no)


#### Q3: Level of Religious Involvement vs Income
# Check conditions
# 1. Independence across groups -> Yes, can be assumed
# 2. Equal variance across groups -> Yes, as shown below
nsyr %>% group_by(ATTEND1) %>% summarize(std = sd(earnings_num))

# Perform the ANOVA test
res <- aov(nsyr$earnings_num ~ nsyr$ATTEND1)
summary(res)

# Perform pairwise comparisons
pw_res <- data.frame(TukeyHSD(res)[[1]][,1:4])
pw_res$test <- rownames(pw_res)

# Plot the 95% CI for each pairwise comparison
ggplot(data = pw_res, aes(y = test, x = diff)) +
  geom_point() + 
  geom_errorbar(aes(xmin = lwr, xmax = upr)) +
  xlab("Avg Difference in Income") +
  ylab("Pairwise Comparisons") +
  theme(
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.title = element_text(face = "bold")
  )



#### Q4: Level of Religious Involvement vs Income
# Check conditions
# 1. Linearity. Yes, the scatter plot reveals a roughly linear relationship
plot(nsyr$faith_num, nsyr$bmi)
abline(lm(as.numeric(nsyr$bmi) ~ nsyr$faith_num))
# 2. Normally Distributed Residuals. Can be assumed, as shown in the histogram
residuals <- resid(lm(as.numeric(nsyr$bmi) ~ nsyr$faith_num))
hist(residuals)
# 3. Constant variability. Yes, shown below.
residuals <- resid(lm(as.numeric(nsyr$bmi) ~ nsyr$faith_num))
plot(nsyr$faith_num, residuals)
# 4. Independent Observation. Yes, can be reasonably assumed


# Compute summary statistics for the regression model
summary(lm(nsyr$bmi ~ nsyr$faith_num))

# compute confidence intervals for the model
confint(lm(nsyr$bmi ~ nsyr$faith_num))


