library(tidyverse)
library(ggplot2)

OFS <- read.csv("APP2021Mar_ALL.csv", stringsAsFactors = TRUE)

OFS %>% filter(SPLITTYPE == "FSMEligibility") %>%
  filter(LIFECYCLESTAGE == "Access") %>%
  filter(POPULATION == "Entrants*") %>%
  filter(LEVEL_OF_STUDY == "All undergraduates") %>%
  filter(SPLIT1 == "EligibleForFSM") %>%
  filter(MEASURETYPE == "INDICATOR") %>%
  filter(MODE_OF_STUDY == "Full-time or apprenticeship") %>%
  select(c("PROVIDER_NAME","YEAR5")) %>%
  mutate(YEAR5 = as.numeric(as.character(YEAR5))) %>%
  drop_na() %>%
  mutate(YEAR5 = YEAR5/100) %>%
  mutate(jitter = runif(n())) -> df ## Manual jittering for gghighlight.


g <- ggplot(data=df) + geom_point(aes(x=YEAR5, y= jitter)) +
  scale_y_continuous(breaks=NULL,labels=NULL) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(y="",
       x="Proportion of Full-time Undergraduates entitled to Free School Meals at Key Stage 4",
       title = "Free School Meal eligibility in the Higher Education sector (OFS 2018-19 data)")

g + geom_label(data = df %>% filter(YEAR5>0.4),
               aes(x=YEAR5,
                   y =jitter,
                   label = paste(PROVIDER_NAME, scales::percent(YEAR5, accuracy = 0.1))),
               hjust = "left",
               nudge_x = 0.005)




g + geom_histogram(aes(x=YEAR5,y=..density../max(..density..)), fill = "blue", alpha=0.2)
