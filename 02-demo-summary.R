# Distribution of demographic variables by house hold size
boxplot <- function(df,xv,title,f){
  
  ggplot(df, aes(x = xv, y = household_size, fill = xv)) +
    geom_boxplot() +
    labs(
      title = title,
      x = f,
      y = "Household Size",
      fill = f
    ) +
    theme_minimal()
}

boxplot(df,df$age_group,"Distribution of Household Sizes by Age Group", "Age Group")

table(df$age_group)
table(df$gender)
table(df$ethnic_group)
table(df$employment_status)
table(df$education_status)
table(df$historical_society_member)
table(df$town_origin)
summary(df$household_size)

boxplot(df,df$gender,"Distribution of Household Sizes by Gender", "Gender")

boxplot(df,df$ethnic_group,"Distribution of Household Sizes by Ethnicity", "Ethnic Group")

boxplot(df,df$employment_status,"Distribution of Household Sizes by Employment Status", "Employment status")

boxplot(df,df$education_status,"Distribution of Household Sizes by Education Status", "Education status")

boxplot(df,df$historical_society_member,"Distribution of Household Sizes by Society membership history", "Society membership history")

boxplot(df,df$town_origin,"Distribution of Household Sizes by town of origin", "Town of origin")

