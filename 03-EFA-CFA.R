library(psych)

new_names2 <- c("Traditional_festivals", "Language", "Dressing_fabrics", 
               "Customs_traditions", "Art_crafts","Food","Dance",
               "Music_songs","Traditional_institutions")

df_efa <- df %>% select(contains("importance"))

names(df_efa) <- new_names2

# Perform EFA
fa <- fa(df_efa, nfactors = 3, rotate = "varimax")

# Print factor loadings
print(fa$loadings)


library(lavaan)

# Define the CFA model based on EFA results
model <- '
  Factor1 =~ Language + Customs_traditions + Art_crafts + Traditional_institutions
  Factor2 =~ Traditional_festivals
  Factor3 =~ Dressing_fabrics + Food + Dance + Music_songs
'

# Fit the CFA model
fit <- cfa(model, data = df_efa)

# Summary of the CFA model
summary(fit, fit.measures = TRUE, standardized = TRUE)
