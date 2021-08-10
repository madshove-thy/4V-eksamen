############################
### HYPOTHESIS 1 TESTING ###
############################

# Remember to run "00-Preliminaries.R" before this
# Remember to run "01-recoding.R" before this script

#### HYPOTHESIS TESTING ####
## Testing wether amount of money spend on ads correlate with election uncertainty ##
# Bivariat
modelh1_1 <- lm(intrau2 ~ totalspend, data = D1)
summary(modelh1_1)

# Adding third variables
modelh1_2 <- lm(intrau2 ~ totalspend + Partileder + Minister + year_in_parliament, data = D1)
summary(modelh1_2)

h1_2_intercept <- modelh1_2$coefficients[1]
h1_2_slope <- modelh1_2$coefficients[2]

## VISUALISE THE RESULTS ##
# Visualise in table format
stargazer(modelh1_1, modelh1_2,
          covariate.labels = c("Kampagneindsats", "Partileder", "Minister", "Antal år i FT"),
          column.labels = c("Model I", "Model II"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Valgresultat",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))


# Visualise the slope
mult_format <- function() {
  function(x) format(10*x,digits = 2) 
}

VA1 <- D1 %>%
  ggplot(aes(x = totalspend, y = intrau2)) +
  labs(x = "1.000 kr. brugt på Facebook-annoncer", y = "Andel af personlige stemmer i partiet i storkredsen") +
  geom_point(color = "grey62") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black")) +
  geom_abline(intercept = h1_2_intercept, slope = h1_2_slope) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01,
                                   decimal.mark = '.')) +
  scale_x_continuous(labels = mult_format(),
                     breaks = c(5, 10, 20, 30, 40, 50, 60))

pdf("../plots/h1-slope.pdf", height = 8, width = 11)
VA1
dev.off()



#### ROBUSTNESS CHECK ####
## Hypothesis 1 without Mette Frederiksen
Dmf <- D1 %>%
  filter(Navn != "Mette Frederiksen")

modelh1_mf <- lm(intrau2 ~ totalspend + Partileder + Minister + year_in_parliament, data = Dmf)
summary(modelh1_mf)

## First with Carey & Shugart estimate
# Bivariat
robusth1_1 <- lm(intrau1 ~ totalspend, data = D1)
summary(robusth1_1)

# Adding third variables
robusth1_2 <- lm(intrau1 ~ totalspend + Partileder + Minister + year_in_parliament, data = D1)
summary(robusth1_2)

# Output with stargazer
stargazer(robusth1_1, robusth1_2,
          covariate.labels = c("Kampagneindsats", "Partileder", "Minister", "Antal år i FT"),
          column.labels = c("Model I", "Model II"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Valgresultat",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))


## Then with interparty-uncertainty - Enhedslisten included since its interparty competition
robusth1_3 <- lm(interu ~ totalspend, data = DI)
summary(robusth1_3)

robusth1_3 <- lm(interu ~ totalspend + Partileder + Minister + year_in_parliament, data = DI)
summary(robusth1_3)

stargazer(robusth1_3,
          covariate.labels = c("Kampagneindsats", "Partileder", "Minister", "Antal år i FT"),
          column.labels = c("Model I", "Model II"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Valgresultat",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(.05, .01, .001))

#### FOR APPENDIX ####
## Forudsaetningstest
model1_1check <- check_model(modelh1_1)
model1_2check <- check_model(modelh1_2)

pdf("../plots/H1_forudsaetningII.pdf", height = 12, width = 12.5)
model1_2check
dev.off()