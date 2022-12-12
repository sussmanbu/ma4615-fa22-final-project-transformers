library(tidyverse)
library(knitr)

## CLEANING THE ABORTION DATA
abortionData <- read_csv(here::here("dataset", "NationalAndStatePregnancy_PublicUse.csv"))

# Tidying the data using pivot longer
abortionDataClean <- abortionData %>% select(state:population40plus)

abortionDataClean <- 
  abortionDataClean %>% 
  pivot_longer(cols = pregnancyratelt15:pregnancyrate40plus, names_to = "temp", values_to = "PregnancyRate", names_repair = "minimal") %>%
  separate(temp, c("PR","Age"), sep = 13) %>%
  select(-PR)

abortionDataClean <- 
  abortionDataClean %>% 
  pivot_longer(cols = abortionratelt15:abortionrate40plus, names_to = "temp", values_to = "AbortionRate", names_repair = "minimal") %>% 
  separate(temp, c("AR","ARAge"), sep = 12) %>% 
  filter(ARAge==Age) %>% 
  select(-AR, -ARAge)

abortionDataClean <- 
  abortionDataClean %>% 
  pivot_longer(cols = birthratelt15:birthrate40plus, names_to = "temp", values_to = "BirthRate", names_repair = "minimal") %>% 
  separate(temp, c("BR","BRAge"), sep = 9) %>% 
  filter(BRAge==Age) %>% 
  select(-BR, -BRAge)

abortionDataClean <- 
  abortionDataClean %>% 
  pivot_longer(cols = abortionratiolt15:abortionratio40plus, names_to = "temp", values_to = "AbortionRatio", names_repair = "minimal") %>% 
  separate(temp, c("AR","ARAge"), sep = 13) %>% 
  filter(ARAge==Age) %>% 
  select(-AR, -ARAge)

abortionDataClean <- 
  abortionDataClean %>% 
  pivot_longer(cols = pregnancieslt15:pregnancies40plus, names_to = "temp", values_to = "Pregnancies", names_repair = "minimal") %>% 
  separate(temp, c("P","PAge"), sep = 11) %>% 
  filter(PAge==Age) %>% 
  select(-P, -PAge)

abortionDataClean <- 
  abortionDataClean %>% 
  pivot_longer(cols = abortionslt15:abortions40plus, names_to = "temp", values_to = "Abortions", names_repair = "minimal") %>% 
  separate(temp, c("A","AAge"), sep = 9) %>% 
  filter(AAge==Age) %>% 
  select(-A, -AAge)

abortionDataClean <- 
  abortionDataClean %>% pivot_longer(cols = birthslt15:births40plus, names_to = "temp", values_to = "Births", names_repair = "minimal") %>% 
  separate(temp, c("B","BAge"), sep = 6) %>% 
  filter(BAge==Age) %>% 
  select(-B, -BAge)

abortionDataClean <- 
  abortionDataClean %>% 
  pivot_longer(cols = miscarriageslt15:miscarriages40plus, names_to = "temp", values_to = "Miscarriages", names_repair = "minimal") %>% 
  separate(temp, c("M","MAge"), sep = 12) %>% 
  filter(MAge==Age) %>% 
  select(-M, -MAge)

abortionDataClean <- 
  abortionDataClean %>% 
  pivot_longer(cols = populationlt15:population40plus, names_to = "temp", values_to = "Population", names_repair = "minimal") %>% 
  separate(temp, c("P","PAge"), sep = 10) %>% 
  filter(PAge==Age) %>% 
  select(-P, -PAge)

# Updating the Age column
abortionDataClean <- 
  abortionDataClean %>% 
  mutate(Age = str_c(substr(abortionDataClean$Age, 1, 2), substr(abortionDataClean$Age, 3, 4), sep = "-"))

abortionDataClean <- 
  abortionDataClean %>% 
  mutate(Age = str_replace(abortionDataClean$Age, "lt-15", "<15"))

abortionDataClean <- 
  abortionDataClean %>% 
  mutate(Age = str_replace(abortionDataClean$Age, "40-pl", "40>"))

abortionDataClean <- 
  abortionDataClean %>% 
  filter(Age!="15-19", Age!="lt-20")

# Saving abortionDataClean to dataset folder

write_csv(abortionDataClean, file = here::here("dataset", "abortionDataClean.csv"))

save(abortionDataClean, file = here::here("dataset/abortionDataClean.RData"))


## CLEANING THE PRESIDENTIAL DATA

stateAff_data_clean <- 
  read_csv(here::here("dataset", "1976-2020-president.csv"), 
           skip = 1, 
           col_types = cols_only(year = col_integer(),
                                 state = col_character(),
                                 state_po = col_character(),
                                 candidatevotes = col_integer(),
                                 totalvotes = col_integer(),
                                 party_simplified = col_character())) %>% 
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>% 
  mutate(Vote_Rate = candidatevotes/totalvotes) 

stateAff_data_clean <- 
  stateAff_data_clean %>% 
  filter(Vote_Rate > 0.001) %>% 
  select(year:state_po, party_simplified:Vote_Rate) 

stateAff_data_clean <- 
  stateAff_data_clean %>%
  group_by(party_simplified) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = party_simplified, values_from = Vote_Rate) %>%
  select(-row)

stateAff_data_clean <- 
  stateAff_data_clean %>% 
  mutate(Affiliation = ifelse((abs(stateAff_data_clean$DEMOCRAT - stateAff_data_clean$REPUBLICAN)) > 0.05, ifelse(stateAff_data_clean$DEMOCRAT > stateAff_data_clean$REPUBLICAN, "DEMOCRAT", "REPUBLICAN"), "DIVIDED"))

stateAff_data_clean <- 
  stateAff_data_clean %>% 
  select(-DEMOCRAT, -REPUBLICAN)

stateAff_data_clean <- 
  stateAff_data_clean %>% 
  rename("stateFull" = "state") %>% 
  rename("state" = "state_po")

stateAff_data_clean <-
  stateAff_data_clean %>% 
  distinct(year, state, Affiliation, .keep_all = TRUE)

write_csv(stateAff_data_clean, file = here::here("dataset", "stateAffData.csv"))

save(stateAff_data_clean, file = here::here("dataset/stateAff.RData"))

## Creating a new State Affiliation Data set

MaxStateAff <- 
  stateAff_data_clean %>% 
  group_by(state, Affiliation) %>% 
  summarise(Count = n()) %>% 
  ungroup()

MaxStateAff <- 
  MaxStateAff %>% 
  group_by(state) %>% 
  arrange(desc(Count)) 

MaxStateAff <- 
  MaxStateAff[1:50,] %>% 
  select(-Count)

write_csv(MaxStateAff, file = here::here("dataset", "MaxStateAffData.csv"))

save(MaxStateAff, file = here::here("dataset/MaxStateAff.RData"))
