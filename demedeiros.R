# For reading from Excel Formated workbook, we need special reading function that can convert it
library(readxl)
library(dplyr)
library(ggplot2)
library(eeptools)
library(lubridate)
library(tidyverse)

# A dash ("-") indicating that the item was not assessed.
# A caret ("^") indicating that the item was skipped.
# A null (.) indicating that the item is inactive.
# A 99 means that they probably started an assessment and could not complete it.


# Load in the data from the youth employment workbook
data <- read_excel("C:\\Users\\Admin\\Dropbox\\RDirectory\\Practicum\\demedeiros.xlsx")
control <- read_excel("C:\\Users\\Admin\\Dropbox\\RDirectory\\Practicum\\demedeiros_control.xlsx")

# Load quarter 3 data 
data.test.q3.1 <- read_excel("C:\\Users\\Admin\\Dropbox\\RDirectory\\Practicum\\test_data_Q3_1.xlsx")
data.test.q3.2 <- read_excel("C:\\Users\\Admin\\Dropbox\\RDirectory\\Practicum\\test_data_Q3_2.xlsx")
# control 
data.control.q3 <- read_excel("C:\\Users\\Admin\\Dropbox\\RDirectory\\Practicum\\control_data_Q3.xlsx")

# column names that we are interested in 

col_names <- c("DPName", "ResidentID", "AssessmentID", "AssessmentReferenceDate", "ResearchID", 
               "A0310A", "A0310B", "A0310C", "A0800", "C0100", "C0500", "C0600", "D0100", "D0200C2",
               "D0200D2", "D0200E2", "D0300", "D0600", "D0500C2", "D0500D2", "D0500E2", "D0500J2", "E0100A", 
               "E0100B", "E0100Z", "E0200A", "E0200B",  "E0200C", "E1100")


data <- data %>% select(col_names)
control <- control %>% select(col_names)
data.test.q3.1 <- data.test.q3.1 %>% select(col_names)
data.test.q3.2 <- data.test.q3.2 %>% select(col_names)
data.control.q3 <- data.control.q3 %>% select(col_names)


# review the data set 
head(data)
str(data)
names(data)

# compare column headers 
setdiff(colnames(data), colnames(control))
setdiff(colnames(data), colnames(data.test.q3.1))
setdiff(colnames(data), colnames(data.test.q3.2))
setdiff(colnames(data), colnames(data.control.q3))

# data.test.q3.1$A0900 <- as.numeric(data.test.q3.1$A0900)
# hello <- as.Date(data.test.q3.1$A0900, origin = "%Y-%m-%d")
# 
# betterDates <- as.Date(dates,
#                        origin = "%Y-%m-%d")
names(data)[names(data) == "SRIResearchID"] <- "ResearchID"




# select selected columns 
# C0100 - (resident is rarely/never understood No-0 / Yes-1) 0 means skip to C0700. 
# C0500 - Add scores for questions C0200-C0400 and fill in total score (00-15)./ Enter 99 if the resident was unable to complete the interview.
# C0600 - 0. No (resident was able to complete Brief Interview for Mental Status ) 0 means skip to C1310 
# D0100 - 0. No (resident is rarely/never understood) Skip to and complete D0500-D0600, Staff Assessment of Resident Mood
# D0300 - Add scores for all frequency responses in Column 2, Symptom Frequency. Total score must be between 00 and 27. Enter 99 if unable to complete interview (i.e., Symptom Frequency is blank for 3 or more items).
# D0500 - Staff Assessment of Resident Mood. 
# D0600  Total Severity Score. / Add scores for all frequency responses in Column 2, Symptom Frequency. Total score must be between 00 and 30.
# E0100 - Potential Indicators of Psychosis 
# E1100 - How does resident's current behavior status, care rejection, or wandering compare to prior assessment (OBRA or Scheduled PPS)? 0 - Same/ 1 - Improved / 2 - Worse / N/A no prior mds assessment 
# Active Diagnosis I0100 - I8000F
# Health Condition - J0100 Pain Management 
# J1700B - Fall History on Admission/Entry or Reentry. B. Did the resident have a fall any time in the last 2-6 months prior to admission/entry or reentry?
# J1900B. Number of Falls Since Admission/Entry or Reentry or Prior Assessment 
# (OBRA or Scheduled PPS), whichever is more recent. # 0 - None / 1 - One / 2 - Two or more 
# Section N - Medication 

# Section I - Active Diagnosis is still missing , waiting for Dr. Demedeiros 

# col_names <- c("DPName", "ResidentID", "AssessmentID", "AssessmentReferenceDate", "SRIResearchID", 
#            "A0310A", "A0310B", "A0310C", "A0800", "A0900", "C0100", "C0500", "C0600", "D0100", "D0200C2",
#            "D0200D2", "D0200E2", "D0300", "D0600", "D0500C2", "D0500D2", "D0500E2", "D0500J2", "E0100A", 
#            "E0100B", "E0100Z", "E0200A", "E0200B",  "E0200C", "E1100")

# merge all the data sets 

all.data <- rbind(data, control, data.control.q3, data.test.q3.1, data.test.q3.2)


data <- rbind(data, control)
data <- rbind(data, data.control.q3)
data <- rbind(data, data.test.q3.1)
data <- rbind(data, data.test.q3.2)

all.data$D0600 <- as.numeric(all.data$D0600)
all.data$D0500C2 <- as.numeric(all.data$D0500C2)
all.data$D0500D2 <- as.numeric(all.data$D0500D2)
all.data$D0500E2 <- as.numeric(all.data$D0500E2)
all.data$D0500J2 <- as.numeric(all.data$D0500J2)


all.data$D0300 <- as.numeric(all.data$D0300)
all.data$D0200C2 <- as.numeric(all.data$D0200C2)
all.data$D0200D2 <- as.numeric(all.data$D0200D2)
all.data$D0200E2 <- as.numeric(all.data$D0200E2)

# select necessary columns 
all.data <- all.data %>% select(col_names) %>% 
  mutate(D0300=D0300-D0200C2-D0200D2-D0200E2, 
         D0600=D0600-D0500C2-D0500D2-D0500E2-D0500J2) 
 
# data2 <- control %>% select(control_col_names) %>% 
#   mutate(D0300=D0300-D0200C2-D0200D2-D0200E2, 
#          D0600=D0600-D0500C2-D0500D2-D0500E2-D0500J2) 


# Creating Quarter Indicator 
all.data$quarter <- quarter(all.data$AssessmentReferenceDate, with_year = FALSE, fiscal_start = 1)
# Creating Age for residents 
# data1$period <- as.period(interval(as.Date(data$A0900), Sys.Date()),  unit = "year")
data1$age <- data1$period$year
data1$period <- NULL 

data1$Status <- "Test"
names(data)[names(data) == "SRIResearchID"] <- "ResearchID"

# Creating Quarter for Control data 
data2$quarter <- quarter(data2$AssessmentReferenceDate, with_year = FALSE, fiscal_start = 1)
# Creating Age for residents 
data2$period <- as.period(interval(as.Date(data2$A0900), Sys.Date()),  unit = "year")
data2$age <- data2$period$year
data2$period <- NULL 
data2$Status <- "Control"

all.data <- rbind(data1, data2)

test_names <- unique(data1$DPName)
control_names <- unique(data2$DPName)



plot1 <- ggplot()+geom_histogram(data=all.data, aes(x=D0600))
plot2 <- ggplot()+geom_histogram(data=all.data, aes(x=D0300))
plot1
plot2

# ordinal logistic regression 

# Logistic Regression 
# if it says the same, it is not necessaraly bad thing 

# Y=BETA_0 + BETA_1* I + beta_0 

# I = is an indicator variable
# beta_0 = is the random subject effect 







# Count observations for each resident 
resident_names <- data.frame(table(all.data$DPName))
colnames(resident_names) <- c("Resident", "Observations")

resident_names$Status <- ifelse(resident_names$Resident %in% test_names, "test", "control") 
resident_names <- resident_names %>% select(Resident, Status, Observations)

plot0 <- ggplot()+geom_bar(data=resident_names, aes(x=Resident, y=Observations, fill=Status), stat = "identity")+
  labs(x="Community Name",
       y="Number of Observation",
       title="Number of Observations from Each Community",
       caption="Data: Client Dr. de Medeiros data") + 
  theme_classic()

plot0


plot1 <- ggplot() + 
  geom_density(data=all.data, aes(x=age, fill = Status), alpha = 0.4)+
  labs(x="Age",
       y="Density",
       title="Age Distribution Between Test Control",
       caption="Data: Client Dr. de Medeiros data") + 
  theme_classic()
plot1



#Creating number of observations for each resident 
all.data1 <- all.data %>% group_by(DPName, ResidentID) %>%
  select(DPName, ResidentID, Status)%>%
  mutate(UniqueID=paste(DPName, ResidentID, sep = ""), n=n())

data5 <- subset(all.data1, !duplicated(UniqueID))

data5 <- data5 %>%
  group_by(DPName) %>%
  summarise(Status=Status[1],
            Count=n()
            )

plot2 <- ggplot()+geom_bar(data=data5, aes(x=reorder(DPName, -Count), y=Count, fill=Status), stat = "identity")+
  labs(x="Resident Names",
       y="Number of Unique Residents Assessed",
       title="Number of Residents",
       caption="Data: Client Dr. de Medeiros data")+theme_classic() + 
  geom_text(data=data5, aes(x=DPName, y=Count, label=Count), vjust=-0.5)+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

plot2

all.data2 <- all.data %>% 
  group_by(Status) %>%
  summarise(C0500 = mean(C0500, na.rm = T),
            D0300 = mean(D0300, na.rm = T),
            D0600 = mean(D0600, na.rm = T))

colnames(all.data2) <- c("Status", "BIMS Summary Score", "Total Severity Score (Interview)", "Total Severity Score (Staff Assessment)")

all.data3 <- all.data2 %>% gather(Indicator, Measurement, "BIMS Summary Score":"Total Severity Score (Staff Assessment)", factor_key = TRUE)

ggplot() + geom_bar(data=all.data3, aes(x=Indicator, y=Measurement, group=Status, fill=Status),
         position="dodge", stat = "identity") + 
  labs(title="Comparison of Different Indicators between Test and Control Residents",
       x = "Indicators",
       y = "Mean Score",
       caption="Data source: Dr. de Medeiros client, 2016")+
  theme_classic() + 
  


write.csv(data3, "data3.csv")
head(data2)


