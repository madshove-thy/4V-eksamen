#### 4V ####
############

# Remember to run "00-Preliminaries.R" before this

# Load dataframe from other project
D <- readRDS("/Users/Madshove/Dropbox/Statskundskab/4V/data/FB_VOTE_final.rds") # This one is used for a previously coded name variable that is good to have

# Data on mandates per party
M <- read.xlsx("../data/Mandat-fordeling-parti.xlsx") %>%
  mutate(Parti_19 = recode(Parti, "Socialdemokratiet" = "A", "Radikale Venstre" = "B",
                           "Det Konservative Folkeparti" = "C", "Nye Borgerlige" = "D",
                           "SF - Socialistisk Folkeparti" = "F", "Liberal Alliance" = "I",
                           "Dansk Folkeparti" = "O", "Venstre, Danmarks Liberale Parti" = "V",
                           "Alternativet" = "Å", "Enhedslisten" = "Ø")) %>%
  mutate(Parti_15 = recode(Parti, "Socialdemokratiet" = "A", "Radikale Venstre" = "B",
                           "Det Konservative Folkeparti" = "C",
                           "SF - Socialistisk Folkeparti" = "F", "Liberal Alliance" = "I",
                           "Dansk Folkeparti" = "O", "Venstre, Danmarks Liberale Parti" = "V",
                           "Alternativet" = "Å", "Enhedslisten" = "Ø"))

# Data on mandates per constituency
C <- read.xlsx("../data/Mandat-fordeling-storkreds.xlsx") %>%
  mutate(Storkreds_2019 = storkreds)

# Load data on all votes for elections in 2015 and 2019
P <- readRDS("../data/antal_kandidater.rds")

# Load data for Carey & Shugart uncertainty
U15 <- read.xlsx("../data/210506-usikkerhed.xlsx", sheet = "Sheet3") # Kredsmandat uncertainty
U19 <- read.xlsx("../data/210506-usikkerhed.xlsx", sheet = "Sheet1") # Kredsmandat uncertainty

# Data on party leaders and other controls
K <- read.xlsx("../data/Liste_Del_I.xlsx")

# Include variable that informs us on which parliamentary setting the observation is in
D <- D %>%
  mutate(parliament_setting = ifelse(D$date >= "2019-06-04", 1, 0))

#### ADD PARTYLEADER AND MINISTER INFORMATION ####
D <- left_join(D, K [ , c("Partileder", "Navn", "Minister", "Minister_2019", "Minister_2011_2019")], by = "Navn")

# Make NA == 0
D$Minister[is.na(D$Minister)] <- 0
D$Partileder[is.na(D$Partileder)] <- 0

#### Merge with colums on how many mandates there is available per constituency ####
D <- merge(x = D, y = C[ , c("kredsmandatfv19", "tillaegsmandatfv19", "Storkreds_2019")], by = "Storkreds_2019", all.x = TRUE)

#### Join how many mandates parties get ####
M <- M %>%
  mutate(mandater15 = kredsmandat15 + tillaegsmandat15) %>%
  mutate(mandater19 = kredsmandat19 + tillaegsmandat19)

#### Make wrongdoings in earlier coding right ####
# Give Leif Mikkelsen (I) his party and constituency back
D$Parti_19[D$Navn == "Leif Mikkelsen"] <- "I"
D$Storkreds_2019[D$Navn == "Leif Mikkelsen"] <- "Vestjyllands Storkreds"
D$Liste_sideordnet_2019[D$Navn == "Leif Mikkelsen"] <- "Sideordnet"

# Give Jeppe Kofod his party back
D$Parti_19[D$Navn == "Jeppe Kofod"] <- "A"

# "Folketingskandidat Henrik" party is V
D$Parti_19[D$Navn == "Folketingskandidat Henrik"] <- "V"

# Thomas Thomsen year in parliament
D$year_in_parliament[D$Navn == "Thomas Thomsen"] <- 1

# Fix partyleaders
D$Partileder[D$Navn == "Mette Frederiksen"] <- 1
D$Partileder[D$Navn == "Alex Vanopslagh"] <- 0
D$Partileder[D$Navn == "Sofie Carsten"] <- 0
D$Partileder[D$Navn == "Jakob Ellemann"] <- 0

#### Set order of variables ####
# Order of parties
D$Parti_19 = factor(D$Parti_19, levels = c("A", "B", "C", "D", "F", "I", "O", "V", "Ø", "Å"))

#### Make variable for blok ####
D <- D %>%
  mutate(redblok = recode(Parti_19, "A" = TRUE, "B" = TRUE, "C" = FALSE, "D" = FALSE, "F" = TRUE, "I" = FALSE,
                          "O" = FALSE, "V" = FALSE, "Å" = TRUE, "Ø" = TRUE))


################################
###### MAKE CALCULATIONS ######
###### BEFORE ELECTION ######
###############################

#### FILTER ONLY ADS BEFORE ELECTION ####
Db <- D %>%
  filter(date <= "2019-06-04") # We chose to say that election day did count as after election

# Add variable with how many ads each politician have made in total
D1 <- Db %>%
  group_by(page_id) %>%
  mutate(nads = n())

# Add variable with how much money each politician have spend in total
D1 <- D1 %>%
  group_by(page_id) %>%
  mutate(totalspend = sum(spend_upper)/10000)

#### CALCULATE UNCERTAINTY FROM ELECTION RESULT ####
#### MERGE TO CALCULATE UNCERTAINTY ####
## Kredsmandat uncertainty
D1 <- merge(x = D1, y = U19, by = c("Parti_19", "Storkreds_2019")) 

# (carey-)Measurement 1 (from Carey paper)
D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(intrau1 = (PV_2019 - PV_not_elected)/Sum_PV)

# Make above 1 to 1 in carey estimate
# This is neccesary since some politicians get elected even though they don't get the most votes
D1$intrau1[D1$intrau1 > 1] <- 1

## (Hjorth-)Measurement 2 (from Frederik Hjorth paper)
P19 <- P %>%
  filter(Valg == "275")

P19 <- P19 %>%
  group_by(StorKredsNr, Parti) %>%
  mutate(intrau2 = PV/sum(PV))

D1 <- merge(x = D1, y = P19[ , c("Valg", "Navn", "intrau2")])

# Since this is before the election we need to set Ane Halsboe, Peter Hummelgaard and Trine Bramsen to Minister == 0
# These are the only new ministers that are included in the dataset
D1$Minister[D1$Navn == "Ane Halsboe"] <- 0
D1$Minister[D1$Navn == "Peter Hummelgaard"] <- 0
D1$Minister[D1$Navn == "Trine Bramsen"] <- 0

# Remove duplicates from dataset
D1 <- D1[!duplicated(D1$Navn), ]

# Save dataset where Enhedslisten is included
D6 <- D1

# Remove Enhedslisten since they don't have intraparty competition
D1 <- D1 %>%
  filter(!Parti_19 == "Ø")
