############################
### DESCRIPTIVE ANALYSIS ###
############################

# Remember to run "00-Preliminaries.R" before this
# Remember to run "01-recoding.R" before this script

#### VISUALISATION FOR DESCRIPTIVE ANALYSIS ####
#### How many politicians do advertise and how many ads do they have? ####
# Who advertise the most?
ads_most1 <- D1 %>%
  group_by(Navn, Parti_19) %>%
  summarise(nads1 = sum(nads))

ads_most2 <- D1 %>%
  group_by(Navn, Parti_19) %>%
  summarise(nads1 = totalspend*10)

# Reorder the colors
# Party colors
party.col1 <- c("#BF0418", # A
               "#E82583", # B
               "#00571F", # C
               "#F04D46", # F
               "#12213f", # I
               "#005392", # V
               "#C21B3E", # Ø
               "#00FF00",
               "#004450", # D# Å
               "#E7D01E" # O
)

# Party colors
party.col2 <- c("#BF0418", # A
               "#00571F", # C
               "#F04D46", # F
               "#E7D01E", # O
               "#005392", # V
               "#C21B3E", # Ø
               "#00FF00",  # Å
               "#E82583", # B
               "#12213f", # I
               "#004450" # D
)

ads_most1$Navn[ads_most1$Navn == "Lars Chr"] <- "Lars Chr. Lilleholt"
ads_most1$Navn[ads_most1$Navn == "Pia Olsen"] <- "Pia Olsen Dyhr"
ads_most2$Navn[ads_most2$Navn == "Jan E"] <- "Jan E. Jørgensen"
ads_most2$Navn[ads_most2$Navn == "Pia Olsen"] <- "Pia Olsen Dyhr"
ads_most2$Navn[ads_most2$Navn == "Søren Pape"] <- "Søren Pape Poulsen"
ads_most2$Navn[ads_most2$Navn == "Troels Lund"] <- "Troels Lund Poulsen"
ads_most2$Navn[ads_most2$Navn == "Kristian Thulesen"] <- "Kristian Thulesen Dahl"

sort(ads_most1$nads1, decreasing = TRUE) # 99 ads for the 10th most politician
sort(ads_most2$nads1, decreasing = TRUE) # 89.189 kr. for the 10th most politician

ads_most_plot1 <- ads_most1 %>%
  filter(nads1 >= 99) %>%
  ggplot(aes(x = reorder(Navn, nads1), y = nads1, fill = Parti_19)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text = element_text(size = 16, color = "black"),
        text = element_text(size = 20, color = "black"),
        axis.title = element_text(color = "black")) +
  xlab("") +
  ylab("Antal annoncer") +
  scale_fill_manual(values = party.col1) +
  guides(fill = guide_legend(title="Parti")) +
  coord_flip()

ads_most_plot2 <- ads_most2 %>%
  filter(nads1 >= 89.189) %>%
  ggplot(aes(x = reorder(Navn, nads1), y = nads1, fill = Parti_19)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size = 16, color = "black"),
        text = element_text(size = 20, color = "black"),
        axis.title = element_text(color = "black")) +
  xlab("") +
  ylab("Øvre estimat for 1.000 kr. brugt") +
  scale_fill_manual(values = party.col2) +
  guides(fill = guide_legend(title="Parti")) +
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500, 600)) +
  coord_flip() 

# Print PDF
pdf("../plots/politician_ads_most.pdf", height = 10, width = 14, onefile = FALSE)
ggarrange(ads_most_plot1, 
          ads_most_plot2, 
          nrow = 1,
          ncol = 2,
          common.legend = TRUE,
          legend = "bottom",
          widths = c(1, 1))
dev.off()

#### How many ads are released throughout the observed period? ####
date_ads <- B %>%
  ggplot(aes(x = ad_creation_time)) +
  geom_bar(size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("Antal annoncer")# +
#geom_hline(yintercept = meanads2, linetype = "dashed", size = 1)

# Print to PDF
pdf("../plots/date_ads.pdf", height = 6, width = 9)
date_ads
dev.off()

# And just the number for how much money there has been spend in the relevant period
sum(B$spend_lower) # 6.6 mio.
sum(B$spend_upper) # 8.6 mio.

B %>% # 2.153 ads in the last week
  filter(ad_creation_time >= "2019-05-29")

B %>% # 6.234 since campaign starts
  filter(ad_creation_time >= "2019-05-07")

## How many ads do the different politicians have and which side of the parliament are they in
mult_format1 <- function() {
  function(x) format(10*x,digits = 2) 
}

pol_ads <- D1 %>%
  mutate(Blok = factor(recode(as.numeric(redblok), "0" = "Højrefløjen", "1" = "Venstrefløjen"),
                       levels = c("Venstrefløjen", "Højrefløjen"))) %>%
  ggplot(aes(x = reorder(Navn, totalspend), y = totalspend, fill = Blok)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("Annonceforbrug i 1.000 kr.") +
  scale_y_continuous(labels = mult_format1(),
                     breaks = c(1, 5, 10, 20, 30, 40, 50, 60)) +
  scale_fill_manual(values = blok.col) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1)

D1 %>% # 44 out of 123 politicians have used less then 10.000 kr. on ads
  group_by(Navn) %>%
  filter(totalspend<=1)

table(D1$redblok) # 70 blue politicians, 53 red

D1$Navn[D1$totalspend >= 10] # 8 politicians spend more than 100.000 kr. on ads.

pdf("../plots/politician_ads_blok.pdf", height = 5, width = 7.5)
pol_ads
dev.off()


#### How much money does party leaders spend on ads? ####
party.col3 <- c("#BF0418", # A
                "#E82583", # B
                "#00571F", # C
                "#F04D46", # F
                "#E7D01E", # O
                "#005392", # V
                "#C21B3E", # Ø
                "#00FF00",  # Å
                "#12213f", # I
                "#004450" # D
)

D6_1 <- D6 %>%
  mutate(totalspend1 = totalspend*10)

D6_1$Navn[D6_1$Navn == "Søren Pape"] <- "Søren Pape Poulsen"
D6_1$Navn[D6_1$Navn == "Pia Olsen"] <- "Pia Olsen Dyhr"
D6_1$Navn[D6_1$Navn == "Kristian Thulesen"] <- "Kristian Thulesen Dahl"
D6_1$Navn[D6_1$Navn == "Lars Løkke"] <- "Lars Løkke Rasmussen"

plot_partyleader <- D6_1 %>%
  filter(Partileder == 1) %>%
  ggplot(aes(x = reorder(Navn, totalspend1), y = totalspend1, fill = Parti_19)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #legend.title = element_blank(),
        axis.text = element_text(size = 16, color = "black"),
        text = element_text(size = 20, color = "black"),
        axis.title = element_text(color = "black")) +
  xlab("") +
  ylab("Øvre estimat for 1.000 kr. brugt") +
  scale_fill_manual(values = party.col3) +
  guides(fill = guide_legend(title="Parti")) +
  scale_y_continuous(breaks = c(50, 100, 200, 300, 400, 500, 600)) +
  coord_flip()

pdf("../plots/partyleader.pdf", height = 10, width = 14)
plot_partyleader
dev.off()