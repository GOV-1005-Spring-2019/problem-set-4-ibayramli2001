library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(devtools)
library(gt)


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

white_ambig <- filter(elections, 
                      race_eth == "White" & file_race_black != race_eth) %>%
  nrow()

time_diff <- elections %>% 
  filter(response %in% c("Dem", "Rep")) %>%
  select(response, timestamp) %>% 
  group_by(response) %>%
  summarize(first = minute(min(timestamp))) %>%
  summarize(diff = first[2] - first[1]) sum

# I noticed that in the table given to us, the percentages do not add up to 100% for White, Black, and Other. 
# This sounds unreasonable given that the numbers represent proportions of race groups.
# There must be some White, Black, and Other people voting for parties other than Dem, Und, and Rep
# This code determines the possible answers given during the survey

elections %>% distinct(response)

# This is to see if there are enough people voting for 3 to distort the dataset

elections %>% filter(response == "3") %>% nrow()

# We get 18 which is can potentially distory our data given that it has only 495 rows. 
# So, we will filter response == 3.

# There are two columns recording race, so I analyzed them and cam to the conclusion that 
# I should use the `file_race` because its numbers match those of the graph

elections %>% filter(response != "3") %>% group_by(race_eth) %>% summarize(n = n())

elections %>% filter(response != "3") %>% group_by(file_race) %>% summarize(n = n())

elections %>% 
  
  # I think it would be reasonable to not consider [DO NOT READ] ones since the respondents don't want to be considered in racial discussions
  # and put percentages in terms of the remaining data
  filter(race_eth != "[DO NOT READ] Don't know/Refused") %>% 
  select(race_eth, response, final_weight) %>%
  group_by(race_eth, response)  %>%
  summarize(total_weight = sum(final_weight)) %>%
  spread(response, total_weight) %>% 
  mutate(Und = replace(Und, is.na(Und), 0),
         `3` = replace(`3`, is.na(`3`), 0),
         Total = Dem + Rep + Und +`3`,
         Dem = Dem/Total, 
         Rep = Rep/Total, 
         Und = Und/Total,
         Total = Total*100)  %>%
  ungroup() %>%
  mutate( race_eth = factor(race_eth, 
                 levels = c("White",
                            "Black", 
                            "Hispanic", 
                            "Asian", 
                            "Other"))) %>%
  select(-`3`, -Total) %>%
  arrange(factor(race_eth)) %>%  
  gt() %>% 
  tab_header(
    title = "Polling Results from North Carolina’s 9th Congressional District") %>% 
  
  cols_label(
    race_eth = "Race/Ethnicity",
    Dem = "Democrats",
    Rep = "Republicans",
    Und = "Undecided"
  ) %>%
  
  fmt_percent(columns = vars(Dem, Rep, Und), 
              decimals = 0
              ) %>% 
  
  # This little pipe is that incantation to take this pretty table, turn it
  # into html, and send it to the md file we are creating. Future versions of
  # gt will probably have a better way of doing this. Indeed, does anyone know
  # of one?
  
  as_raw_html() %>% as.character() %>% cat() 

  
  
elections %>% 
  filter(educ != "[DO NOT READ] Refused") %>%
  
  ggplot(aes(factor(educ, levels = c("Grade school",
                                     "High school",
                                     "Some college or trade school",
                                     "Bachelors' degree",
                                     "Graduate or Professional Degree")), final_weight)) +
  geom_violin() +
  geom_jitter(alpha = 0.3) +
  coord_flip() +
  ylab("Weight Given to Respondent in Calculating Poll Results") +
  xlab("") + 
  labs(title = "More Educated Matter Less in North Carolina 9th", 
       subtitle = "Poll gives more weight to people who are less likely to participate in polls",
       caption = "New York Times Upshot/Siena College 2018 live polls")

  
elections %>%
  filter( ager != "[DO NOT READ] Refused") %>%
  select(ager, timestamp, gender) %>%
  mutate(timestamp = as.character(timestamp)) %>%
  separate(timestamp, into = c("ymd", "hms"), sep = " ", remove = TRUE) %>%
  mutate(ymd = ymd(ymd), hms = hms(hms), hour = hour(hms), minute = minute(hms), second = second(hms)) %>%
  group_by(ager, gender) %>% 
  summarize(median_ymd = median(ymd), median_hour = median(hour), median_minute = median(minute), median_second = median(second)) %>%
  mutate(median_hms = (paste(median_hour, median_minute, median_second, sep = ":"))) %>%

  ggplot() +
  geom_col(aes(ager, median_hms, fill = gender), position = "dodge2") +
  labs(title = "Median Response Time by Gender and Age") +
  xlab("Age group") +
  ylab("Median response time")

 
 
 # elections %>%
 #   filter( ager != "[DO NOT READ] Refused") %>%
 #   select(ager, timestamp) %>%
 #   mutate(timestamp = as.character(timestamp)) %>%
 #   separate(timestamp, into = c("ymd", "hms"), sep = " ", remove = TRUE) %>%
 #   mutate(ymd = ymd(ymd), hms = hms(hms)) %>%
 #   group_by(ager) %>% 
 #   summarize(mean_ymd = mean(ymd), mean_hms = mean(hms))



