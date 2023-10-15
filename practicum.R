library(dplyr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(bslib)
library(janitor)
library(stringr)
library(tibble)
library(readxl) 
library(hablar)

    
multiplesheets <- function(fname) { 
  
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, guess_max=1048576, sheet = x, col_types = c("date",
                                                          "numeric", "text", "text", "text", "text",
                                                          "text", "text", "text", "text", "text",
                                                          "text", "text", "text", "text", "text",
                                                          "text", "text", "text", "text", "text",
                                                          "text", "text", "text", "text", "text", "text"))) 
  data_frame <- lapply(tibble, as.data.frame) 

  names(data_frame) <- sheets 
  print(data_frame)

} 

# specifying the path name 
path <- "survey.xlsx"
data <- multiplesheets(path)
sheet1 <- data$Sheet1
sheet2 <- data$'Sheet 2'

df <- merge(sheet1, sheet2, all=TRUE)
df <- df %>% clean_names()

#Note for all values in data frame where string was "NA", the string was replaced with the NA value.
#I think this is easier to use actual NA values in R than to use string representations of NA.
# In general, I replace invalid data values with a NA value. I assume the invalid data to be typos.
#Other portions of the data may be valid excluding the invalid portions, so the invalid values are removed.

##Clean timestamp column
# Remove timestamps before the year 2014, there are instances of timestamps from the year 1905
# Standardized the format of the timestamp to Year-Month-Day Hour:Min:Sec
df <- df %>% mutate(timestamp = ifelse(timestamp < ymd_hms("2014-01-01 0:00:00"), NA, timestamp)) %>% transform( 
  timestamp = as_datetime(timestamp)) 


# Clean Gender
# Female and Male are Cis-gender Male/Female, all other options are Other (Trans, Nonbinary, etc)
# Replace recognizable typos to corresponding gender and removed values that do not make sense (1,2, etc)

df <- df %>% mutate(across(gender, str_to_upper)) %>% mutate(gender = ifelse(gender == "NA", NA, gender)) %>% mutate(gender = ifelse(gender == "2", NA, gender)) %>% mutate(gender = ifelse(gender == "1", NA, gender))

df['gender'][df['gender'] == "M"] <- "MALE"
df['gender'][df['gender'] == "MAIL"] <- "MALE"
df['gender'][df['gender'] == "MAKE"] <- "MALE"
df['gender'][df['gender'] == "MAILE"] <- "MALE"
df['gender'][df['gender'] == "MAL"] <- "MALE"
df['gender'][df['gender'] == "MALE (CIS)"] <- "MALE"
df['gender'][df['gender'] == "CIS MALE"] <- "MALE"
df['gender'][df['gender'] == "CIS MAN"] <- "MALE"
df['gender'][df['gender'] == "MALR"] <- "MALE"
df['gender'][df['gender'] == "MAN"] <- "MALE"
df['gender'][df['gender'] == "MSLE"] <- "MALE"

df['gender'][df['gender'] == "F"] <- "FEMALE"
df['gender'][df['gender'] == "FEMAKE"] <- "FEMALE"
df['gender'][df['gender'] == "FEMAIL"] <- "FEMALE"
df['gender'][df['gender'] == "FEMALE (CIS)"] <- "FEMALE"
df['gender'][df['gender'] == "WOMAN"] <- "FEMALE"
df['gender'][df['gender'] == "CIS FEMALE"] <- "FEMALE"
df['gender'][df['gender'] == "CIS-FEMALE"] <- "FEMALE"
df['gender'][df['gender'] == "CIS-FEMALE/FEMME"] <- "FEMALE"


shouldBecomeOther<-!(df$gender %in% c("FEMALE", "MALE", NA))
df$gender[shouldBecomeOther]<- "OTHER"
df <- df %>% mutate(across(gender, str_to_title)) 



#Clean Countries and State
#Change countries in abberivations to their Names
df['country'][df['country'] == "US"] <- "United States"
df['country'][df['country'] == "UK"] <- "United Kingdom"


# All states only pertain to United States, if other countries had values in the 
#states column, they were replaced with a NA value. 

df <- df %>% mutate(state = ifelse(country != "United States", NA, state))


# Change State Names into abbreviations
df['state'][df['state'] == "Texas"] <- "TX"
df['state'][df['state'] == "California"] <- "CA"
df['state'][df['state'] == "New York"] <- "NY"


#Clean age
#Make the age data type into an integer
#Valid ages are between 18 and 100
df <- transform( 
  df, age = as.integer(age)) 

df <- df %>% mutate(age = ifelse(age < 18 | age > 100, NA, age))


#Clean Self-employed, family_history, treatment
#Did not see any invalid data, replaced "NA" string with NA value
df <- df %>% mutate(self_employed = ifelse(self_employed == "NA", NA, self_employed))
df <- df %>% mutate(family_history = ifelse(family_history == "NA", NA, family_history))


#Clean treatment
#Replace N with No and Y with Yes
#Replace all other value that are not No or Yes with NA value
df <- df %>% mutate(treatment = ifelse(treatment == "NA", NA, treatment))
df['treatment'][df['treatment'] == "N"] <- "No"
df['treatment'][df['treatment'] == "Y"] <- "Yes"
shouldBecomeOther<-!(df$treatment %in% c("No", "Yes"))
df$treatment[shouldBecomeOther]<- NA

#Clean work_interfere
#Limit acceptable values to "Never", "Often", "Rarely", and "Sometimes"
#All other values are replaced with NA value
shouldBecomeOther<-!(df$work_interfere %in% c("Never", "Often", "Rarely", "Sometimes"))
df$work_interfere[shouldBecomeOther]<- NA

#Clean no_employees (Number of employees)
#Limit acceptable values to "26-100", "100-500", "500-1000", and "More than 1000"
#All other values are replaced with NA value
shouldBecomeOther<-!(df$no_employees %in% c("26-100", "100-500", "500-1000", "More than 1000"))
df$no_employees[shouldBecomeOther]<- NA


#Clean remote_work, tech_company
#Did not see any invalid data, replaced "NA" string with NA value
shouldBecomeOther<-!(df$remote_work %in% c("No", "Yes"))
df$remote_work[shouldBecomeOther]<- NA

shouldBecomeOther<-!(df$tech_company %in% c("No", "Yes"))
df$tech_company[shouldBecomeOther]<- NA


#Clean benefits, care_options, seek_help, $anonymity
#Limit acceptable values to "No", "Yes", "Don't Know", NA
#All other values are replaced with Don't Know value
#All other values not "No", "Yes", "Don't Know", or NA are some version of "Not sure"
shouldBecomeOther<-!(df$benefits %in% c("No", "Yes", "Don't Know", NA))
df$benefits[shouldBecomeOther]<- "Don't Know"


shouldBecomeOther<-!(df$care_options %in% c("No", "Yes", "Don't Know", NA))
df$care_options[shouldBecomeOther]<- "Don't Know"


shouldBecomeOther<-!(df$wellness_program %in% c("No", "Yes", "Don't Know", NA))
df$wellness_program[shouldBecomeOther]<- "Don't Know"


shouldBecomeOther<-!(df$seek_help %in% c("No", "Yes", "Don't Know", NA))
df$seek_help[shouldBecomeOther]<- "Don't Know"

shouldBecomeOther<-!(df$anonymity %in% c("No", "Yes", "Don't Know", NA))
df$anonymity[shouldBecomeOther]<- "Don't Know"

#leave, mental_health_cons looked clean and were left alone. Did not even see "NA" strings


#Clean phys_health_consequence
#Replace N with No and Y with Yes
#Replace all other value that are not No or Yes with NA value
df$phys_health_consequence
df['phys_health_consequence'][df['phys_health_consequence'] == "N"] <- "No"
df['phys_health_consequence'][df['phys_health_consequence'] == "Y"] <- "Yes"

shouldBecomeOther<-!(df$coworkers %in% c("No", "Yes", "Some of them", NA))
df$coworkers[shouldBecomeOther]<- NA

#Supervisor, Mental/Physical Health interview, mental/physical, obs_consequence looks clean
#and were left alone. Did not even see "NA" strings


#Clean comments
# "NA" string were replaced with NA value
#Symbols that represent None such as '-' are replaced with NA value
df['comments'][df['comments'] == "NA"] <- NA
df['comments'][df['comments'] == "-"] <- NA


#Remove rows with a lot of missing data
#There are 27 variables, if there are over 20 missing values then the row should be removed
df <- df[rowSums(is.na(df)) < 20, ]


## Graph variables


#How much work interferes with mental health for each gender
gender_plot <- ggplot(df, aes(gender))
gender_plot + geom_bar(aes(fill = work_interfere))

#How much work interferes with mental health for employees of companies of varying sizes
n_employees <- ggplot(data=subset(df, !is.na(no_employees)), aes(no_employees)) 
n_employees + geom_bar(aes(fill = work_interfere))


write.csv(df, "cleaned_survey.csv", row.names=FALSE)
