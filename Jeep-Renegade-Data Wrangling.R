# I will be usign this R script to practice data wrangling using dplyr


# Library and data loading ------------------------------------------------

library(dplyr)
library(readxl)
jeepdata <- read_excel("C:/Users/abedi/OneDrive/Documents/R Projects/Jeep Renegade Analysis/Jeep Customer Data.xlsx")


# mutate ----------------------------------------------------------------

str(jeepdata)
# Checking the data frame structure to determine what columns need to be assigned as factors

jeepdata <- jeepdata %>% 
  mutate(CHANNEL = as.factor(CHANNEL),
         FORM_NAME = as.factor(FORM_NAME),
         BROWSER = as.factor(BROWSER),
         COUNTRY = as.factor(COUNTRY),
         VISITOR_TYPE = as.factor(VISITOR_TYPE))
# mutating the above columns to factors

str(jeepdata)
# Verifying the above columns are now factors

jeepdata <- jeepdata %>% 
  mutate(Form_Percentage = `FORM COMPLETES`/`FORM STARTS`)
# Created a new column to get percentage of forms completed


# select & filtering ------------------------------------------------------------------

jeepdata1 <- jeepdata %>% 
  select(CHANNEL, FORM_NAME, BROWSER)
# selecting 3 columns into new dataframe jeepdata1

jeepdata2 <- jeepdata %>% 
  select(-COUNTRY, - VISITOR_TYPE)
# selecting all columns from jeepdata df except COUNTRY & VISITOR_TYPE


## select helper functions ----------------------------------------------

# starts_with

jeepdata3 <- jeepdata %>% 
  select(starts_with("uni"))
# Selects any columns that start only with "uni"
# case insensitivity parameter
# can change if want

# ends_with

jeepdata4 <- jeepdata %>% 
  select(ends_with("tes"))
# Selects columns that end with "tes"
# same rules apply as starts_width just with ending words
# only 2 end in dth

# contains

jeepdata5 <- jeepdata %>% 
  select(contains("f"))
# select columns that contain only letter f whether lower or upper case
# Most business data has trends in naming conventions these last 3 functions are super helpful

# between and including

jeepdata6 <- jeepdata %>% 
  select(UNIQUE_VISITORS:FORM_NAME)
# take jeepdata store in jeepdata6 pipe
# Grabs columns between UNIQUE_VISITORS & FORM_NAME


## filtering ---------------------------------------------------------------

myjeepdata <- jeepdata %>% 
  filter(FORM_NAME == "FORM_A") %>% 
# filter function defines what rows going to be KEPT in data. keeps FORM_NAME only = to FORM_A
   select(-UNIQUE_VISITORS, -COUNTRY)
# pipe over to select function and give all columns except -UNIQUE_VISITORS, -COUNTRY

another_jeep <- jeepdata %>% 
  filter(`FORM STARTS` >= 10 &
           `FORM COMPLETES` >= 10) %>% 
  select(-UNIQUE_VISITORS, -COUNTRY)
# same example as above just with greater than or equal to operator

last_jeep <- jeepdata %>% 
  filter(`FORM STARTS` >= 10 &
           `FORM COMPLETES` >= 10 |
           (UNIQUE_VISITORS <= 100))
# using a mix of & and | aka or operators  

## Sub-setting data ---------------------------------------------------------

my_jeepdata <- jeepdata %>% 
  filter(CHANNEL == "Organic Search") %>% 
  select(starts_with("FORM"))

summary(my_jeepdata)
# summary stats of my_jeepdata

prop.table(table(jeepdata$FORM_NAME))
prop.table(table(jeepdata$COUNTRY))
prop.table(table(jeepdata$CHANNEL))
prop.table(table(jeepdata$BROWSER))
prop.table(table(jeepdata$VISITOR_TYPE))

# making a proportion table of form name, country, channel, browser, visitor type

# Grouping & Summarizing Data ---------------------------------------------

jeep_pivot <- jeepdata %>% 
  mutate(CHANNEL = as.factor(CHANNEL),
         FORM_NAME = as.factor(FORM_NAME),
         BROWSER = as.factor(BROWSER),
         COUNTRY = as.factor(COUNTRY),
         VISITOR_TYPE = as.factor(VISITOR_TYPE)) %>% 
  group_by(VISITOR_TYPE) %>% 
  summarize(Mean.Unique_Visitors = mean(UNIQUE_VISITORS),
            Mean.Form_Starts = mean(`FORM STARTS`),
            Mean.Form_Completes = mean(`FORM COMPLETES`))
# group by visitor_type to get the following mean for new vs returning visitors


jeep_pivot2 <- jeepdata %>% 
  mutate(COUNTRY = as.factor(COUNTRY),
         VISITOR_TYPE = as.factor(VISITOR_TYPE)) %>% 
  group_by(COUNTRY, VISITOR_TYPE) %>% 
  summarize(Mean.Unique_Visitors = mean(UNIQUE_VISITORS),
            Mean.Form_Starts = mean(`FORM STARTS`),
            Mean.Form_Completes = mean(`FORM COMPLETES`))
# grouping by 2 variables instead of 1 

jeep.pivot <- jeepdata %>% 
  group_by(`FORM COMPLETES`) %>% 
  summarize(Mean.Unique.Visitors = mean(UNIQUE_VISITORS))
# grouping by numeric

# Summarize Function ------------------------------------------------------

# summarize function performs calculations based on groupings defined by group_by

# Want single value produced per grouping

# summarize with ifelse function 

jeep_pivot3 <- jeepdata %>% 
  mutate(COUNTRY = as.factor(COUNTRY)) %>% 
  group_by(COUNTRY) %>% 
  summarize(Max.Unique.Visitors = max(UNIQUE_VISITORS),
            Big.Unique.Visitors = ifelse(max(UNIQUE_VISITORS) >= 20, TRUE, FALSE))
# converting country to factor
# grouping by country to get a max summary of the 4 countries in the df
# making Big.Unique.Visitors will return a TRUE/FALSE if the max of unique visitors is greater than 100

# Joining Data ------------------------------------------------------------

newjeepdata <- read_excel("C:/Users/abedi/OneDrive/Documents/R Projects/Jeep Renegade Analysis/New Jeep Customer Data.xlsx")
# I changed the country names in excel prior to loading

newjeepdata <- newjeepdata %>% 
  mutate(CHANNEL = as.factor(CHANNEL),
         FORM_NAME = as.factor(FORM_NAME),
         BROWSER = as.factor(BROWSER),
         COUNTRY = as.factor(COUNTRY),
         VISITOR_TYPE = as.factor(VISITOR_TYPE))
# used mutate to change the above columns to factors as before

# new_jeep_data <- jeepdata %>% 
#   left_join(newjeepdata, by = "FORM_NAME")

# getting this error as result of join cannot allocate vector of size 1.5 Gb

memory.limit()
# limit is at 12049

memory.limit(size = 50000)
# increased memory limit

new_jeep_data <- jeepdata %>% 
  inner_join(newjeepdata, by = "FORM_NAME")
# for some reason my obs are 2M will have to figure this out and join another way


## rbind -------------------------------------------------------------------

rbind_jeepdata <- rbind(jeepdata, newjeepdata)
# number of columns didn't match because of a previous function used before to create new column need to remove that column from original df
str(rbind_jeepdata)

jeepdata <- jeepdata %>% 
  select(-Form_Percentage)
# removed column

rbind_jeepdata <- rbind(jeepdata, newjeepdata)
str(rbind_jeepdata)
View(rbind_jeepdata)
# two dataframes were joined rbind worked because both df's because columns match

# Arranging Data ----------------------------------------------------------

