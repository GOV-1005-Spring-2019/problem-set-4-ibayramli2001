library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot)

elections <- read_csv("ps_4_elections-poll-nc09-3.csv")

dem_support_count <- filter(elections, 
                            response == "Dem") %>% 
  count()

diff_rep_und <- filter(elections,
                       response %in% c("Rep", "Und")) %>%
  group_by(response) %>%
  summarize(n = n()) %>% 
  summarize(diff = n[1] - n[2])

gender_diff <- filter(elections, 
                      gender != gender_combined) %>% 
  nrow()