

### Load libraries
library("readxl")
library(dplyr)
library(dplyr)
library(tidyverse)
library("gtsummary")
library(ggplot2)
library(Hmisc)

###Set up working directory 

setwd("/Users/jimmyzeng/Desktop/job application documents/practice ")
####Read data into R studio
clinicdata<-read_excel("nesa_data.xls")

#### Explore dataset: 1007 rows and 18 variables
colnames(clinicdata)
dim(clinicdata)

####explore how data was recorded and labeled 
head(clinicdata)
describe(clinicdata)

###Observations: 
## ID: 1005 distinct ID but the total ID number is 1007, suggesting that some IDs are duplicate. 
##need to check if the records are identical for the duplicated IDs. If not, change the ID number, otherwise 
#remove the duplicated row. 
##Age: age range is quite wide, some participants are very young.
##phinfect: data was recorded as 1,2,3. Data dictionary values are 1=NO, 2=Yes, 9= dont know. Check if 3 should be recorded as 9. 
##Height: Some participants are extremely tall: 191, 192, 198 cm. check if these records are correct. 
##weight: impossible weights 167-945 kgs.
##dumg2: 144 outlier check 


# Recode categorical variables 
clinicdata <- clinicdata %>%
  mutate(gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")))

clinicdata <- clinicdata %>%
  mutate(ethnicity = factor(ethnicity, levels = c(1, 2,3,4), labels = c("European", "Maori","Pacific","Asian")))

###Identify and remove duplicate ID 
clinicdata[duplicated(clinicdata$idno) | duplicated(clinicdata$idno, fromLast = TRUE), ]
clinicdata<-clinicdata[-which(clinicdata$idno==2510)[1],]

###Create a new ID
summary(clinicdata$idno)
clinicdata[which(clinicdata$idno==2523)[1],1]<-2951

###Sort by ID

clinicdata<-clinicdata[order(clinicdata$idno),]

###Explore each of the variables.

hist(clinicdata$age,main="Age Distribution")
summary(clinicdata$age)

hist(clinicdata$height)
clinicdata[which(clinicdata$height>190),]
clinicdata[which(clinicdata$height<=120),]

# Find the rows to swap (height ≤ 120, excluding idno 2082 as swapping doesn't make sense)
swap_rows <- which(clinicdata$height <= 120 & clinicdata$idno != 2082)
clinicdata[which(clinicdata$idno == 2082),]
# Swap height and weight for those rows
temp <- clinicdata$height[swap_rows]
clinicdata$height[swap_rows] <- clinicdata$weight[swap_rows]
clinicdata$weight[swap_rows] <- temp
clinicdata[swap_rows,]

####Look at the height distribution again 

hist(clinicdata$height, main ="Height of Patients", xlab="Height (cm)", ylab="",breaks=seq(100,200,20))
hist(clinicdata$height, main ="Height ", xlab="Height (cm)", ylab="")

####Look at frequencies 
ggplot(clinicdata, aes(x = height)) +
  geom_histogram(binwidth = 5) +
  labs(
    title = "Height of Patients",
    x = "Height (cm)",
    y = "Frequency"
  )

###Look at percentages 
ggplot(clinicdata, aes(x = height, y = after_stat(count / sum(count)))) +
  geom_histogram(binwidth = 5) +
  labs(title="Height of Patients",
          x="Height (cm)",
          y = "Percentage")

####Look at the weights 
hist(clinicdata$weight)

clinicdata[which(clinicdata$weight>200),]

###recode impossbile weights to NA 
clinicdata[which(clinicdata$weight>200),]<-NA


##### Categorical variables 

table(clinicdata$fhstone)
table(clinicdata$phinfect)
clinicdata$phinfect[which(clinicdata$phinfect==3)]<-NA
table(clinicdata$ethnicity)



colnames(clinicdata)
clinicdata %>%
  select(age, height, weight, gender, ethnicity,fhstone,phinfect) %>%
  tbl_summary(by = gender, statistic = list(
    all_continuous() ~ "{mean} ({sd})"
  ), missing = "ifany", missing_text="missing")


clinicdata %>%
  select(age, height, weight, gender, ethnicity,fhstone,phinfect) %>%
  tbl_summary(by = gender, statistic = list(
    all_continuous() ~ "{mean} ({sd})"),missing = "no"
  )


#####Answer the research questions
###1) if dietary intake of calcium is different between the males and females.

clinicdata %>%
  group_by(gender) %>%
  summarise(
    mean_calcium = mean(dcalcium, na.rm = TRUE),
    sd_calcium = sd(dcalcium, na.rm = TRUE),
    median_calcium = median(dcalcium, na.rm = TRUE),
    IQR_calcium = IQR(dcalcium, na.rm = TRUE)
  )


calcium_table <- clinicdata %>%
  group_by(gender) %>%
  summarise(
    N = sum(!is.na(dcalcium)),
    `Mean (SD)` = sprintf("%.2f (%.2f)",
                          mean(dcalcium, na.rm = TRUE),
                          sd(dcalcium, na.rm = TRUE)),
    `Median (IQR)` = sprintf("%.2f (%.2f, %.2f)",
                             median(dcalcium, na.rm = TRUE),
                             quantile(dcalcium, 0.25, na.rm = TRUE),
                             quantile(dcalcium, 0.75, na.rm = TRUE))
  )

calcium_table

# Recode gender as a factor with labels if necessary
clinicdata$gender <- factor(clinicdata$gender, levels = c(1, 2), labels = c("Male", "Female"))

# Create histogram to compare calcium levels between males and females


par(mfrow = c(1, 2))

male_data <- clinicdata$dcalcium[clinicdata$gender == "Male"]
hist(male_data, 
     main = "Calcium Levels for Males", 
     xlab = "Calcium Intake", 
     ylab = "Frequency", 
     col = "blue", 
     border = "black", 
     xlim = c(0,50), 
     ylim = c(0,70),
     breaks = 20)

# For Females
female_data <- clinicdata$dcalcium[clinicdata$gender == "Female"]
hist(female_data, 
     main = "Calcium Levels for Females", 
     xlab = "Calcium Intake", 
     ylab = "Frequency", 
     col = "pink", 
     border = "black", 
     xlim = c(0,50), 
     ylim = c(0,70),
     breaks = 20)


par(mfrow = c(1, 2))

male_data <- clinicdata$dcalcium[clinicdata$gender == "Male"]
hist(male_data, 
     main = "Calcium Levels for Males", 
     xlab = "Calcium Intake", 
     ylab = "Percentage", 
     col = "blue", 
     border = "black", 
     xlim = c(0,50), 
     ylim = c(0,0.1),
     breaks = 20,
     probability = TRUE)
    

# For Females
female_data <- clinicdata$dcalcium[clinicdata$gender == "Female"]
hist(female_data, 
     main = "Calcium Levels for Females", 
     xlab = "Calcium Intake", 
     ylab = "Percentage", 
     col = "pink", 
     border = "black", 
     xlim = c(0,50), 
     ylim = c(0,0.1),
     breaks = 20,
     probability = TRUE)

###2) if there is an association between dietary calcium intake and 24-hour urinary calcium levels. 
par(mfrow = c(1, 1))
plot(clinicdata$dcalcium,clinicdata$duca,xlab="Calcium intake (mmol/day)",ylab="24 hour urine calcium (mmol/l)")

# Add the LOWESS line (smoothed curve)
tempdata_clean <- na.omit(clinicdata)

lowess_line <- lowess(tempdata_clean$dcalcium, tempdata_clean$duca)
lines(lowess_line, col = "red", lwd = 2)



