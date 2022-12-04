# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

#setwd("/Users/katefong/Library/CloudStorage/OneDrive-PaceUniversity/Fall 2022/ECO 400/Project/Data")
setwd("/Users/katefong/Documents/ECO 400 Project")

library(tidyverse)
library(readxl)
library(dplyr)
library(ipumsr)

install.packages("tidyverse")

ddi <- read_ipums_ddi("usa_00001.xml")
wagedata <- read_ipums_micro(ddi)
save(wagedata, file = "wagedata.Rdata")


names(wagedata) <- tolower(names(wagedata)) #changes the variable names from CAPS to lowercase 

wagedata1 <- wagedata %>%  
    mutate(educ1 = case_when(
      educ >= 0 & educ <=5 ~ 1, #less than high school education
      educ == 6 ~ 2, #high school
      educ >= 7 & educ <= 9 ~ 3, #some college
      educ >= 10 & educ <= 11 ~ 4, #college
    ))

summary(wagedata1,educ1) #summarizing the data

wagedata1$educ1 <- factor(wagedata1$educ1,
                          levels = c(1,2,3,4),
                          labels = c("Less than HS", "High School", "Some College", "College"))

lm(wagedata1$incwage ~ educ1, data=wagedata1) #omitted category is less than high school; the intercept refers to less than high school
                                              #if you have less than a high school degree you make $27,232, if you have a high school degree you make $27,232+$9,654, etc. just add on the coefficients for some college and college

wagedata1 <- wagedata1 %>% filter(incwage>0) #filter function, only keep the observations where the incwage is greater than 0

lm(log(wagedata1$incwage) ~ educ1, data=wagedata1) #high school graduates make 26% more than less than high school; people with college or more make 97% more than those with less than high school 
                      

#Creating categories for race

wagedata1 <- wagedata1 %>%
  mutate(raceeth = case_when(
    hispan == 0 & race == 1 ~ 1,
    hispan == 0 & race == 2 ~ 2,
    hispan == 0 & (race >= 4 & race <= 6) ~ 3,
    hispan == 0 & (race == 3 | race >= 7) ~ 4,
    hispan >= 1 & hispan <= 4 ~5,
  ))

wagedata1$raceeth <- factor(wagedata1$raceeth,
                            levels = c(1,2,3,4,5),
                            labels = c("Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other", "Hispanic"))


lm(log(wagedata1$incwage) ~ educ1 + raceeth , data=wagedata1) #Intercept interpretation we would do e^10; Controlling for race and ethnicity, people with a high school degree
                                                               # make 23% more than less than high school, some college makes 39% more than less than high school
                                                              #Controlling for education, non hispanic blacks make 27% less than whites, Non hispanics asians make 1.2% more than whites

#Make a female variable

wagedata1 <- wagedata1 %>%
  mutate(female = case_when(
    sex == 2 ~ 1,
    sex == 1 ~ 0,
  ))

table(wagedata1$female) #see the number of values in each category
                        #There are 1.76 million men and 1.62 million women

table(wagedata1$female)/nrow(wagedata1) #to see the value as a percentage
                                        #Total # divided by # of rows 
                                        # 52% men, 48% women

wagedata1_fin <- wagedata1 %>% filter(ind1990 >= 700 & ind1990 <=710)

lm(log(incwage) ~ educ1 + raceeth + female + age , data=wagedata1_fin) #women make 42% less than men in the financial sector controlling for education

#Making a variable that is equal to after ; dummy for after 2019 (during 2020) & then will interact with female dummy 
#interact with after to see if 2020 differential is sig dif than other years

wagedata1_fin$after <- ifelse(wagedata1_fin$year>2019,1,0)  #if variable year is greater that 2019 it is a 1
lm(log(incwage) ~ educ1 + raceeth + female*after , data=wagedata1_fin) #regression that log of wage and interacts female * after
      #The effect of being female is less after 2020 than before 

wagedata1_fin <- wagedata1_fin %>%  
  mutate(majocc1990 = case_when(
    occ1990 >=	3	&	occ1990 <=	37	~	1,
    occ1990 >=	43	&	occ1990 <=	199	~	2,
    occ1990 >=	203	&	occ1990 <=	391	~	3,
    occ1990 >=	403	&	occ1990 <=	469	~	4,
    occ1990 >=	473	&	occ1990 <=	499	~	5,
    occ1990 >=	503	&	occ1990 <=	699	~	6,
    occ1990 >=	703	&	occ1990 <=	890	~	7,
  ))

wagedata1_fin$occ1990 <- factor(wagedata1_fin$occ1990) #making is a categorical variable
wagedata1_fin$age <- factor(wagedata1_fin$age)
wagedata1_fin$hinsemp <- factor(wagedata1_fin$hinsemp)
wagedata1_fin$statefip <- factor(wagedata1_fin$statefip)
wagedata1_fin$degfield <- factor(wagedata1_fin$degfield)
                           
lm(log(incwage) ~ educ1 + female*after, data=wagedata1_fin)
lm(log(incwage) ~ educ1 + female*after + age, data=wagedata1_fin)
lm(log(incwage) ~ educ1 + female*after + age + raceeth , data=wagedata1_fin)
lm(log(incwage) ~ educ1 + female*after + age + raceeth + statefip + occ1990 + degfield, data=wagedata1_fin)
lm(log(incwage) ~ educ1 + female*after + age + raceeth + statefip + occ1990 + degfield + nchild + hinsemp , data=wagedata1_fin)
                        #interaction means the effect female is allowed to change after COVID

#5 regressions in the stargazer ouput
#figure out how to specify coefficients I want
#include interaction in all 5
#r2, n in output
  
#only looking at men/women in finance discuss variables and American Community Survey, talk about male/female differential (labor textbooks), 
                        


#should we square age?? non linear
#Should we use statefip or PWState2 variable (place of work)
#should i be using majocc1990 or occ1990 in regressions
#For data visualization AVG Male/Female Wage over time?








