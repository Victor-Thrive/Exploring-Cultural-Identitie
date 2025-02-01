library(tidyverse)
library(janitor)


#Exploration of the cultural identities of the RIYE community.
#Analysis of the most important cultural identities to people of the RIYE community.
#What are the cultural identities of the RIYE people?
#What cultural factors are most important to determine the cultural identity of RIYE people?
  

df <- read_csv("data/data.csv") # import data

names(df) # check variable names



df <- df %>%
  rename( # rename variable names 
    Age_Group = `Age Group`,
    Gender = Gender,
    Education_Status = `Educational Status`,
    Location = Location,
    Town_Origin = `Town/Place of Origin`,
    Household_Size = `Household Size`,
    Historical_Society_Member = `Member of Historical Society ( A historical society is a non-profit organization dedicated to collecting, preserving, interpreting, and promoting the history of a particular place, group of people, or topic in order to promote historical awareness and understanding by providing a platform for research, education, and public engagement e.g. Historical Society of Nigeria)`,
    Employment_Status = `Employment Status`,
    Ethnic_Group = `Ethnic Group (From the following ethnic groups, which do you most identify with?)`,
    Importance_Traditional_Festivals = `On a scale of 1 to 5, how would you rate the importance of these cultural practices in identifying your cultural identity and ethnicity? (1 = Not important at all, 5 = Very important) [Traditional Festivals]`,
    Importance_Language = `On a scale of 1 to 5, how would you rate the importance of these cultural practices in identifying your cultural identity and ethnicity? (1 = Not important at all, 5 = Very important) [Language (speaking local dialect)]`,
    Importance_Dressing_Fabrics = `On a scale of 1 to 5, how would you rate the importance of these cultural practices in identifying your cultural identity and ethnicity? (1 = Not important at all, 5 = Very important) [Dressing/Fabrics]`,
    Importance_Customs_Traditions = `On a scale of 1 to 5, how would you rate the importance of these cultural practices in identifying your cultural identity and ethnicity? (1 = Not important at all, 5 = Very important) [Customs and traditions (e.g., naming ceremonies)]`,
    Importance_Art_Crafts = `On a scale of 1 to 5, how would you rate the importance of these cultural practices in identifying your cultural identity and ethnicity? (1 = Not important at all, 5 = Very important) [Art and Crafts (e.g., wood carvings, beadwork)]`,
    Importance_Food = `On a scale of 1 to 5, how would you rate the importance of these cultural practices in identifying your cultural identity and ethnicity? (1 = Not important at all, 5 = Very important) [Food]`,
    Importance_Dance = `On a scale of 1 to 5, how would you rate the importance of these cultural practices in identifying your cultural identity and ethnicity? (1 = Not important at all, 5 = Very important) [Dance]`,
    Importance_Music_Songs = `On a scale of 1 to 5, how would you rate the importance of these cultural practices in identifying your cultural identity and ethnicity? (1 = Not important at all, 5 = Very important) [Music/Songs]`,
    Importance_Traditional_Institutions = `On a scale of 1 to 5, how would you rate the importance of these cultural practices in identifying your cultural identity and ethnicity? (1 = Not important at all, 5 = Very important) [Traditional Institutions (e.g., monarchies, chieftaincy systems)]`,
    Participation_Traditional_Festivals = `How frequently do you participate in each of these cultural practices? [Traditional Festivals]`,
    Participation_Language = `How frequently do you participate in each of these cultural practices? [Language (speaking local dialect)]`,
    Participation_Dressing_Fabrics = `How frequently do you participate in each of these cultural practices? [Dressing/Fabrics]`,
    Participation_Customs_Traditions = `How frequently do you participate in each of these cultural practices? [Customs and traditions (e.g., naming ceremonies)]`,
    Participation_Art_Crafts = `How frequently do you participate in each of these cultural practices? [Art and Crafts (e.g., wood carvings, beadwork)]`,
    Participation_Food = `How frequently do you participate in each of these cultural practices? [Food]`,
    Participation_Dance = `How frequently do you participate in each of these cultural practices? [Dance]`,
    Participation_Music_Songs = `How frequently do you participate in each of these cultural practices? [Music/Songs]`,
    Participation_Traditional_Institutions = `How frequently do you participate in each of these cultural practices? [Traditional Institutions (e.g., monarchies, chieftaincy systems)]`
  )

df <- df |>
  clean_names() # clean variable names

names(df) # View the cleaned df column names 

# fix insonsistency in categorical variables
df <- df |>
  mutate(education_status = case_when(
    education_status %in% c("MSc", "Master degree") ~ "Master's Degree", 
    education_status == "University/Polytechnic Degree" ~ "University/Polytechnic Degree",
    TRUE ~ education_status
  )) |>
  mutate(
    town_origin = str_to_lower(town_origin)
  ) |>
  mutate( town_origin = case_when(
    town_origin %in% c("abeokuta/ijero ekiti","abeokuta","iberekodo  ogun") ~ "Abeokuta",
    town_origin %in% c("oyo/ oyo state","ondo state","lagos","kwara","idah, kogi state",
                       "ibadan","aba, abia state","ekiti","igbara-oke, ondo state",
                       "kwara state","osun") ~ "Non_ogun_towns",
    town_origin %in% c("ijebu","ijebu north-east","ijebu ode/ ogun state",
                       "ijebu imushin","ijebu ode. ogun state","ijebu oru") ~ "Ijebu",
    town_origin %in% c("yewa south","ipokia - idiroko","igbogila","ajilete") ~ "Yewa south",
    town_origin == "ogun state" ~ "Unkown_ogun_town",
    TRUE ~ town_origin
   )
  ) |>
  mutate(across(where(is.character), as.factor)) # convert all character columns to factor

