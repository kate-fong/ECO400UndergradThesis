# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

setwd("/Users/katefong/Documents/ECO 400 Project/Updated Data Pull (Includes 2021)")


library(tidyverse)
library(readxl)
library(dplyr)
library(ipumsr)
library(stargazer)
library(estimatr)
library(ggplot2)
#library(sjPlot)
#library(effects)
library(jtools)
library(interactions)
library(modelr)
library(sandwich)
library(plm)
library(lmtest)
library(car)
library(devtools)
library(freqtables)
library(vtable)

ddi <- read_ipums_ddi("usa_00002.xml")
wagedata <- read_ipums_micro(ddi)

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

wagedata1_fin$occ19901 <- factor(wagedata1_fin$occ1990) #making it a categorical variable
wagedata1_fin$age1 <- factor(wagedata1_fin$age)
wagedata1_fin$hinsemp1 <- factor(wagedata1_fin$hinsemp)
wagedata1_fin$statefip1 <- factor(wagedata1_fin$statefip)
wagedata1_fin$degfield1 <- factor(wagedata1_fin$degfield)
wagedata1_fin$marst1 <- factor(wagedata1_fin$marst)

#interaction using * includes all parent terms and interactions already 
ols <- lm(log(incwage) ~ female*after, data=wagedata1_fin)
ols2 <- lm(log(incwage) ~ female*after + educ1 +  age1, data=wagedata1_fin)
ols3 <- lm(log(incwage) ~ female*after + educ1 +  age1 + raceeth , data=wagedata1_fin)
ols4 <- lm(log(incwage) ~ female*after + educ1 +  age1 + raceeth + statefip1 + occ19901 + degfield1, data=wagedata1_fin)
ols5 <- lm(log(incwage) ~ female*after + educ1 +  age1 + raceeth + statefip1 + occ19901 + degfield1 + hinsemp1 + marst1 + nchild, data=wagedata1_fin)
#interaction means the effect female is allowed to change after COVID
summary(ols)

#looking at robust standard errors
coeftest(ols3, vcov = vcovHC)
ct <- coeftest(ols, vcov = vcovHC)
summary(ct)

summary(wagedata1_fin, "hinsemp1")



#regression table output (no Robust standard errors ):
stargazer(ols,ols2, ols3, ols4, ols5,
          type = "html", 
          covariate.labels = c("Female", "After", "Female*After"),
          #covariate.labels = c("Female", "After", "HighSchool", "SomeCollege", "College", "NonHispanicBlack", "NonHispanicAsian", "NonHispanicOther", "Hispanic", "HealthInsurance", "NumChild"),  
          dep.var.labels = "lnWageSalaryIncome", 
          column.labels = c("", "", "","",""),
          omit = c("age1","occ19901","statefip1", "degfield1", "marst1", "educ1", "raceeth","hinsemp1", "nchild"),
          title = "Linear OLS Regressions Predicting Log Wage",
          out= "regoutputs.html") 

#summary stats table output
sumstatsdata <- wagedata1_fin[, c( "age", "incwage", "nchild","female", "educ1", "raceeth")]

summary(sumstatsdata)
   #stargazer(as.data.frame(sumstatsdata), type="text", title="Summary Statistics", out ="/Users/katefong/Documents/ECO 400 Project/Updated Data Pull (Includes 2021)/sumstats.html")                           
 #vtable : https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html
st(sumstatsdata, summ.names = list(c('N', 'Mean', 'SD', 'Min', 'Max')), 
   digits = 2, out = "browser" ) 

st(sumstatsdata, group = 'female', group.test = FALSE, digits = 2, out='browser')

#gender <- sumstatsdata %>%
  #freq_table(female)

#gender_education <- sumstatsdata %>%
  #freq_table(female, educ1)

#gender_raceeth <- sumstatsdata %>%
  #freq_table(female, raceeth)

#Regression for Data Visualization (AVG Female wage and AVG Male wage by age)
# create a factor version of age
wagedata1_fin$age1 <- factor(wagedata1_fin$age)

summary(wagedata1_fin$age)

# estimate the model and store it in model1
model1 <- lm(log(incwage) ~ female*age1, data=wagedata1_fin)
summary(model1)

stargazer(model1,type="text")

# convert female to a factor variable
wagedata1_fin$female <- factor(wagedata1_fin$female)
class(wagedata1_fin$female)

# create a grid of values of age and female
grid <- wagedata1_fin %>%
  data_grid(age1,female)
grid

# add predictions from model1 to the grid of values
grid <- grid %>%
  add_predictions(model1)
grid
summary(grid)

# plot the average of the log of women's and men's wages
malefemalewages <- 
  ggplot(grid, aes(x=age1, y=pred, col = ifelse(female == 0, "Male", "Female"))) + 
  geom_point(size=2.7) +
  labs(x = "Age", y = "Log Wage")  +
  ggtitle("Male vs. Female Wages in the Finance Industry By Age") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +    #  #centers title
  guides(color = guide_legend(title = "Gender", reverse = TRUE)) +  #reverse function changes the order of the legend
  theme(legend.title.align=0.6, legend.position = c(.93, 0.096), legend.box.background = element_rect(color="black", size=.2),
        legend.box.margin = margin(4, 4, 4, 4)) #Saving 8.39 x 6.24 in image

  #scale_y_continuous(expand=c(0, 0), limits=c(0, 12))    <- makes y axis start at 0
  #theme(legend.position = "top")

ggsave("malefemwages_legendinside.png", malefemalewages, dpi = 300) #saving image

#mention this is without any control variables but still shows an interesting relationship
#backups up argument to put age in as a dummy; using dummies allows the relationship to be whatever it is
#Difference in the logs times 100 is the percent difference
# 10.8-10.4 * 100 = 33% difference ; At 25 a male earns 30% more than women
# at age 54 there is a 75% difference

#notice that the wage growth for women goes til 36 (child bearing age) whereas male seems to get wage growth until about 50
#doesn't control for occupation and other variables that would likely shrink the gap

#female coefficient  is for age 25


#summary stats for men and women

