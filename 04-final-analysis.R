library(likert)


new_names <- c("Traditional festivals", "Language", "Dressing fabrics", 
               "Customs traditions", "Art crafts","Food","Dance",
               "Musing songs","Traditional institutions")
df2 <- df_efa

# Determine the cultural factors that are most important to determine the cultural identity
important_cultural_identity <- df2 %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  gather(key = "Cultural_Factor", value = "Importance") %>%
  arrange(desc(Importance))


# Identify the most important cultural identity
most_important_cultural_identity <- df2 %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  gather(key = "Cultural_Factor", value = "Importance") %>%
  arrange(desc(Importance)) %>%
  slice(1)

print(paste("The most important cultural identity is:",
            most_important_cultural_identity$Cultural_Factor))

f2 <- df2 %>%
  mutate_all(as.factor) %>%
  mutate(
    `Traditional festivals` = factor(`Traditional festivals`, levels = c("1", "2", "3", "4", "5"), 
                                     labels = c("1 = Not important at all", 
                                                "2 = Slightly important", 
                                                "3 = Moderately important", 
                                                "4 = Important", 
                                                "5 = Very important")),
    Language = factor(Language, levels = c("1", "2", "3", "4", "5"), 
                      labels = c("1 = Not important at all",
                                 "2 = Slightly important", 
                                 "3 = Moderately important", 
                                 "4 = Important", 
                                 "5 = Very important")),
    `Dressing fabrics` = factor(`Dressing fabrics`, levels = c("1", "2", "3", "4", "5"), 
                                labels = c("1 = Not important at all", 
                                           "2 = Slightly important", 
                                           "3 = Moderately important", 
                                           "4 = Important", 
                                           "5 = Very important")),
    `Customs traditions` = factor(`Customs traditions`, levels = c("1", "2", "3", "4", "5"), 
                                  labels = c("1 = Not important at all",
                                             "2 = Slightly important", 
                                             "3 = Moderately important", 
                                             "4 = Important", 
                                             "5 = Very important")),
    `Art crafts` = factor(`Art crafts`, levels = c("1", "2", "3", "4", "5"), 
                          labels = c("1 = Not important at all", 
                                     "2 = Slightly important", 
                                     "3 = Moderately important", 
                                     "4 = Important", 
                                     "5 = Very important")),
    Food = factor(Food, levels = c("1", "2", "3", "4", "5"), 
                  labels = c("1 = Not important at all", 
                             "2 = Slightly important", 
                             "3 = Moderately important", 
                             "4 = Important", 
                             "5 = Very important")),
    Dance = factor(Dance, levels = c("1", "2", "3", "4", "5"), 
                   labels = c("1 = Not important at all", 
                              "2 = Slightly important", 
                              "3 = Moderately important", 
                              "4 = Important", 
                              "5 = Very important")),
    `Musing songs` = factor(`Musing songs`, levels = c("1", "2", "3", "4", "5"), 
                            labels = c("1 = Not important at all",
                                       "2 = Slightly important", 
                                       "3 = Moderately important", 
                                       "4 = Important", 
                                       "5 = Very important")),
    `Traditional institutions` = factor(`Traditional institutions`, levels = c("1", "2", "3", "4", "5"), 
                                        labels = c("1 = Not important at all",
                                                   "2 = Slightly important", 
                                                   "3 = Moderately important", 
                                                   "4 = Important", 
                                                   "5 = Very important"))
  )

likert_df2 <- likert(data.frame(f2))
summary(likert_df2)
plot(likert_df2, legend.position = "right") +
  ggtitle("") +
  xlab('Percentage') +
  ylab('Cultural Practices') +
  guides(fill = guide_legend(title = "Rating"))



# Exploring respondent participation in different cultural Identities

df3 <- df %>% select(contains("participation"))

names(df3) <- new_names
  
all_levels <- c("Daily", "Monthly", "Never", "Rarely", "Weekly", "Yearly")

df3$`Traditional festivals` <- factor(df3$`Traditional festivals`, levels = all_levels)
 

likert_df3 <- likert(data.frame(df3))
summary(likert_df3)
plot(likert_df3, legend.position = "right")
