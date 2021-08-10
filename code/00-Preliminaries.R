########################
#### PRELIMINARIES ####


# Load packages
library(tidyverse)
library(openxlsx)
library(performance)
library(quanteda)
library(topicmodels)
library(tidytext)
library(stargazer)
library(ggpubr)

### Set standard colors
# Party colors
party.col <- c("#BF0418", # A
               "#E82583", # B
               "#00571F", # C
               "#004450", # D
               "#F04D46", # F
               "#12213f", # I
               "#E7D01E", # O
               "#005392", # V
               "#C21B3E", # Ø
               "#00FF00"  # Å
)

# Blok colors
blok.col <- c("red3", "navy")

# Color-palette
standard.col <- c("blue3", "darkorange2", "green4", "firebrick4")

