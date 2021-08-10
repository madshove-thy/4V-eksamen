############################
### HYPOTHESIS 2 TESTING ###
############################

# Remember to run "00-Preliminaries.R" before this
# Remember to run "03-recoding.R" before this script
# Remember to run "04-topicmodel.R" before this script

#### HYPOTHESIS TESTING ####
## Testing wether amount of money spend on ads correlate with election uncertainty ##
# Bivariat
modelh2_1 <- lm(intrau2 ~ issueo, data = D2)
summary(modelh2_1)

# Adding interaction
modelh2_2 <- lm(intrau2 ~ totalspend + issueo + totalspend*issueo, data = D2)
summary(modelh2_2)

# Adding third variables
modelh2_3 <- lm(intrau2 ~ totalspend + issueo + totalspend*issueo + Partileder + Minister + year_in_parliament, data = D2)
summary(modelh2_3)

## VISUALISE THE RESULTS ##
# Visualise in table format
stargazer(modelh2_1, modelh2_2, modelh2_3,
          covariate.labels = c("Kampagneindsats", "Emneejerskab", "Kampagneindsats*Emneejerskab"),
          column.labels = c("Model I", "Model II", "Model III"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Valgresultat",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          add.lines = list(c("Kontrol for partileder", "Nej", "Nej", "Ja"),
                           c("Kontrol for minister", "Nej", "Nej", "Ja"),
                           c("Kontrol for år i FT", "Nej", "Nej", "Ja")))

#### ROBUSTNESS CHECK ####
## Hypothesis 1 without Mette Frederiksen
Dmf <- D1 %>%
  filter(Navn != "Mette Frederiksen")

modelh1_mf <- lm(intrau2 ~ totalspend + Partileder + Minister + year_in_parliament, data = Dmf)
summary(modelh1_mf)

## First with Carey & Shugart estimate
# Bivariat
robusth2_1 <- lm(intrau1 ~ issueo, data = D2)
summary(robusth2_1)

# Adding interaction
robusth2_2 <- lm(intrau1 ~ totalspend + issueo + totalspend*issueo, data = D2)
summary(robusth2_2)

# Adding third variables
robusth2_3 <- lm(intrau1 ~ totalspend + issueo + totalspend*issueo + Partileder + Minister + year_in_parliament, data = D2)
summary(robusth2_3)

# Output with stargazer
stargazer(robusth2_1, robusth2_2, robusth2_3,
          covariate.labels = c("Kampagneindsats", "Emneejerskab", "Kampagneindsats*Emneejerskab"),
          column.labels = c("Model I", "Model II", "Model III"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Valgresultat",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001),
          add.lines = list(c("Kontrol for partileder", "Nej", "Nej", "Ja"),
                           c("Kontrol for minister", "Nej", "Nej", "Ja"),
                           c("Kontrol for år i FT", "Nej", "Nej", "Ja")))
