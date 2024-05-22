setwd("C:/Users/admin/OneDrive/Computational Social Science/Masteruppsats/Culture and political identification/SCB graf")

library(readxl)
library(tidyverse)
library(ggplot2)

Votes <- read_excel("Votes.xlsx")

DF_votes <- Votes


DF_votes <- DF_votes |> 
  rename("Year" = "År")

DF_votes$M <- as.character(DF_votes$M)

DF_votes <- DF_votes%>%
  mutate(across(c(M:SD), as.character)) 

DF_votes$`as.character("M")` <- NULL
DF_votes$NYD <- NULL

df_long <- pivot_longer(DF_votes, cols = -Year, names_to = "Party", values_to = "Percentage")


df_long$Percentage <- as.numeric(df_long$Percentage)

#Change colors to reflect parties
df_long <- df_long |> 
  mutate(colorparty = case_when(
    Party == "M" ~ "lightblue",
    Party == "MP" ~ "lightgreen", 
    Party == "V" ~ "darkred",
    Party == "S" ~ "red",
    Party == "KD" ~ "darkblue",
    Party == "L" ~ "blue",
    Party == "SD" ~ "yellow",
    Party == "C" ~ "darkgreen",
    TRUE ~ "black"))

#Filter out years of voting before general elections
#df_long <- df_long[df_long$Year >= 1921 & df_long$Year <= 2022, ]


scbplot <- ggplot(data = df_long, aes(x = Year, y = Percentage, group = Party, color = Party)) +
  geom_line(size = 1) + 
  geom_vline(xintercept = 1921, linetype = "dotted", color = "grey", size = 1)+
  scale_x_continuous(limits = c(1911, 2022), 
                     breaks = seq(1910, 2022, by = 10)) +
  scale_y_continuous(limits = c(0, 55), 
                     breaks = seq(0, 60, by = 10)) +
  scale_color_manual(values = setNames(df_long$colorparty, df_long$Party)) +
  theme_classic() +
  labs(title = "Percentage of votes per election for the Swedish national parliament",
       x = "Year",
       y = "Percentage of votes",
       color = "Party",
       caption = "Data from Statistics Sweden (2024)")
scbplot

ggsave("scbplot.pdf", scbplot, width = 6.31, height = 3.77)
