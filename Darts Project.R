
###Basics of darts
## Throw a dart at the board.
## The board is divided into 20 equal segments, with an outer ring (Double ring) and a smaller inner ring (Treble ring) within each segment. The center of the board has an addiontial two scoring rings.
## The segments are numbered from 1 to 20 (in a non-sequential order). The number represents the base score for that segment if a dart is thrown into that segement.
## Hitting the outer ring (Double ring) doubles the score. For example, landing a dart in the Double ring of segment 16 scores 32 points.
## Hitting the inner ring (Treble ring) triples the score.
## The center has two scoring zones: the Inner Bullseye (50 points) and the outer Bullseye ring (25 points).
## Missing a scoring zone or having a dart fall out of the board results in zero points.
## After throwing three darts, total score from the three darts is subtracted from the remaining score. For example, if you start with 441 and score 100, your new total is 341.
## A leg (single game) starts at 501 points and the aim to reduce your score to exactly zero before your opponent.
## To win the leg, your final dart must land in the Double ring or Inner Bullseye, bringing your score to exactly zero. This can be done in 1, 2, or 3 darts.


### Setting up R for analysis

##Install required libraries if these are not installed
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")

##Load required libraries

library(dplyr)
library(ggplot2)
library(tidyr)

## Set working directory to folder with the Data Scores csv file with setwd()

## Load in Dart Scores into a dataframe
darts_scores <- read.csv("Dart Scores.csv")
darts_scores

## Dataset had 3 columns: 1) The name of the score, 2) the total number of points, and 3) can it be used as a final dart in a checkout attempt ie in Double ring or the Inner Bullesye

### Identify scores from when a dart is thrown
## Have no dart thrown in dataset, will remove this 

Onedartscores <- darts_scores %>%
  filter(Name != "No dart thrown")

### Identify the number of scoring segments (including zero) and possible scores from a single dart throw
length(Onedartscores$Score)
length(unique(Onedartscores$Score))
## Have 63 Scoring segments with 44 unique scores from a single dart

### Score from throwing three darts


## Create dataframe showing possible combinations of scores assuming all darts are thrown
threedarts <- expand.grid(Onedartscores$Name, Onedartscores$Name, Onedartscores$Name)
nrow(threedarts)
## 250047 Possible combinations 

## Add in scores for each throw
threedarts <- threedarts %>%
  mutate(score1 = darts_scores$Score[match(Var1, darts_scores$Name)]) %>%
  mutate(score2 = darts_scores$Score[match(Var2, darts_scores$Name)]) %>%
  mutate(score3 = darts_scores$Score[match(Var3, darts_scores$Name)])

## Count total score of three throws 
threedarts$total <- threedarts$score1 + threedarts$score2 + threedarts$score3

##Find minimum and maximum scores in a players turn with three darts thrown
threedarts[threedarts$total == min(threedarts$total),]
threedarts[threedarts$total == max(threedarts$total),]
## Highest score 180 (3 Treble 20's), Lowest score is zero (3 Miss dart board)



### Create Pie Chart showing distribution of scores from 3 darts
## Create category of scores for Pie Chart

threedartsscorescat <- data.frame(group = c('0-30', '31-60', '61-90', '91-120','121+'),
                 number = 0)

## Find number of scores for each category

threedartsscorescat$number[1] <- sum(threedarts$total <= 30)
threedartsscorescat$number[2] <- sum(threedarts$total >= 31 & threedarts$total<= 60)
threedartsscorescat$number[3] <- sum(threedarts$total >= 61 & threedarts$total<= 90)
threedartsscorescat$number[4] <- sum(threedarts$total >= 91 & threedarts$total<= 120)
threedartsscorescat$number[5] <- sum(threedarts$total >= 121 & threedarts$total<= 180)
threedartsscorescat$group <- factor(threedartsscorescat$group, levels = c('0-30', '31-60', '61-90', '91-120','121+')) #Step ensures legend in pie chart goes in ascending order ie 0-30,31-60

## Plot Pie Chart
ggplot(threedartsscorescat, aes(x = "", y = number, fill = group)) +
  geom_col(color = "black") +
  geom_text(aes(label = number),
            position = position_stack(vjust = 0.4)) +
  coord_polar(theta = "y") +
  theme_void() +
  guides(fill = guide_legend(title = "Score Range"))


### Find Checkouts finshes

## Reminder Doubles and Inner Bullseye are used a final dart in Checkout

Checkoutdart <- darts_scores %>%
  filter(Checkout == "Y")

## Checkouts where only one dart was thrown so can assign other as no dart thrown

Onedartcheckout <- expand.grid(Checkoutdart$Name,"No dart thrown","No dart thrown")

## Two dart checkout where two darts were thrown 

Twodartcheckout <- expand.grid(Onedartscores$Name, Checkoutdart$Name,"No dart thrown")

## Three dart checkout

Threedartcheckout <- expand.grid(Onedartscores$Name, Onedartscores$Name,Checkoutdart$Name)

##Combine all checkouts into a single dataframe
Allcheckout <- bind_rows(Onedartcheckout,Twodartcheckout,Threedartcheckout)


## Add in scores for each throw
Allcheckout <- Allcheckout %>%
  mutate(score1 = darts_scores$Score[match(Var1, darts_scores$Name)]) %>%
  mutate(score2 = darts_scores$Score[match(Var2, darts_scores$Name)]) %>%
  mutate(score3 = darts_scores$Score[match(Var3, darts_scores$Name)])

## Count total score of checkout scores 
Allcheckout$total <- Allcheckout$score1 + Allcheckout$score2 + Allcheckout$score3

## Plot histogram of which show number of valid ways to checkout score

ggplot(Allcheckout, aes(x=total)) +
  geom_histogram(binwidth = 0.7, fill = "blue") +
  labs(x="Checkout Score",y="Ways to Checkout Score") +
  theme(axis.line = element_line(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#### Extra work

## How many checkouts involve any dart hitting the inner bulleye (scoring dart and/or last dart in checkout)

bulleyecheckout <- Allcheckout %>%
  filter(Var1 == "Inner Bullseye" | Var2 == "Inner Bullseye"  | Var3 == "Inner Bullseye")


nrow(bulleyecheckout)

## 6553 checkout finishes involve at least one dart hitting the inner bulleye

## List the unqiue 100 or higher checkout scores

sort(unique(Allcheckout$total[Allcheckout$total >= 100]))

## How many checkouts do NOT involve any dart thrown scoring more than 22

twentytwoscoredartcheckouts <- Allcheckout %>%
  filter(score1 <= 22 & score2 <=22 & score3 <= 22)

nrow(twentytwoscoredartcheckouts)

## 17171 checkouts have darts where each dart thrown individually scores 22 points or less

## What is the smallest and highest checkout score where all darts were thrown, there was no darts that missed the board, no darts were scored in  the Inner Bullseye and a treble 20 was hit at least once

nomissnobulleyetrebletwentycheckout <- Allcheckout %>%
  filter(Var1 != "Inner Bullseye" & Var2 != "Inner Bullseye"  & Var3 != "Inner Bullseye") %>%
  filter(Var1 != "No dart thrown" & Var2 != "No dart thrown"  & Var3 != "No dart thrown") %>%
  filter(Var1 != "Miss" & Var2 != "Miss"  & Var3 != "Miss") %>%
  filter(Var1 == "Treble 20" | Var2 == "Treble 20")

min(nomissnobulleyetrebletwentycheckout$total)
max(nomissnobulleyetrebletwentycheckout$total)

## 63 is the lowest score and 160 is the highest score
