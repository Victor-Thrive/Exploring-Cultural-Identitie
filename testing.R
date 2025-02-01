
df |>
  mutate(education_status = case_when(
  education_status %in% c("MSc", "Master degree") ~ "Master's Degree",
  education_status == "University/Polytechnic Degree" ~ "University/Polytechnic Degree",
  TRUE ~ education_status
)) 

  df <- df %>%
    mutate(location = str_to_title(location)) %>%
    mutate(location = str_replace_all(location, "\\s+", " ")) %>%
    mutate(location = case_when(
      location == "Abeokuta/Ijero Ekiti" ~ "Abeokuta",
      location == "Idah, Kogi State" ~ "Idah, Kogi",
      location == "Igbara-Oke, Ondo State" ~ "Ondo State",
      location == "Ondo State" ~ "Ondo State",
      location == "Ijebu North-East" ~ "Ijebu North-East",
      location == "Oyo/ Oyo State" ~ "Oyo State",
      location == "Ipokia - Idiroko" ~ "Ipokia-Idiroko",
      location == "Aba, Abia State" ~ "Abia State",
      location == "Ijebu Ode. Ogun State" ~ "Ijebu Ode",
      location == "Ijebu Ode/ Ogun State" ~ "Ijebu Ode",
      location == "Ijebu" ~ "Ijebu Ode",
      location == "Igbogila" ~ "Igbogila",
      TRUE ~ location
    )) %>%
    distinct()
  
  df |>
    count(town_origin)
  
levels(df$town_origin)
test <- df |>
  mutate( town_origin = case_when(
    town_origin %in% c("abeokuta/ijero ekiti","abeokuta","iberekodo  ogun") ~ "Abeokuta",
    town_origin %in% c("oyo/ oyo state","ondo state","lagos","kwara","idah, kogi state",
                       "ibadan","aba, abia state","ekiti","igbara-oke, ondo state","kwara state","osun") ~ "Non_ogun_towns",
    town_origin %in% c("ijebu","ijebu north-east","ijebu ode/ ogun state","ijebu imushin","ijebu ode. ogun state","ijebu oru") ~ "Ijebu",
    town_origin %in% c("yewa south","ipokia - idiroko","igbogila","ajilete") ~ "Yewa south",
    town_origin == "ogun state" ~ "Unkown_ogun_town"
  )
  )

test |>
  count(town_origin) |>
  summarize(sum = sum(n))




data(pisaitems)
items29 <- pisaitems[,substr(names(pisaitems), 1,5) == 'ST25Q']
names(items29) <- c("Magazines", "Comic books", "Fiction", 
                    "Non-fiction books", "Newspapers")
l29 <- likert(items29)
summary(l29)
plot(l29)
