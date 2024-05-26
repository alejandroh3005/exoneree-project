#' Title: "Feature engineering script"
#' Project: "Exoneree Project"
#' Author: "Alejandro Hernandez"
#' Date: "May 24, 2024"

rm(list = ls()) # clear workspace
library(dplyr)
library(lubridate)

# load data
exonerees <- read.csv("data/publicspreadsheet-may2024.csv",
                      na.strings = c("NA", "Don't Know", ""))


### ============================= ###
### Manipulate existing variables
### ============================= ###

exonerees <- exonerees %>%
  # create ID
  dplyr::mutate(ID = dplyr::row_number()) %>%
  dplyr::select(ID, everything()) %>%
  # convert and rename Year of Crime
  mutate(Date.of.Crime.Year = as.numeric(gsub(",", "", Date.of.Crime.Year))) %>%
  dplyr::rename(Year.of.Crime = Date.of.Crime.Year) %>%
  # rename Age at Crime
  rename(Age.at.Crime = Age)


### ============================= ###
### Conviction Tags
### ============================= ###

tags_conviction <- exonerees %>%
  select(ID, Tags) %>%
  # Split TAGS into individual words and create a long-format data frame
  tidyr::separate_rows(Tags, sep = ";#") %>%
  # Add a column with value 1 for each tag
  mutate(value = 1) %>%
  # Spread the tags into separate columns
  tidyr::pivot_wider(names_from = Tags, values_from = value,
                     values_fill = list(value = 0))

# attached separated conviction tag columns to exoneration data
new_contags <- names(tags_conviction %>% select(-ID, -"NA"))
exonerees <- cbind(exonerees, tags_conviction %>% select(-ID))


### ============================= ###
### Exoneration Tags
### ============================= ###

tags_exoneration <- exonerees %>%
  select(ID, OM.Tags) %>%
  # Split TAGS into individual words and create a long-format dataframe
  separate_rows(OM.Tags, sep = ";#") %>%
  # Add a column with value 1 for each tag
  mutate(value = 1) %>%
  # Spread the tags into separate columns
  pivot_wider(names_from = OM.Tags, values_from = value, 
              values_fill = list(value = 0))

# attach existing exoneration tag columns to these new tags columns
existing_extags <- c("F.MFE", "FC", "ILD", "P.FA", "DNA", "MWID", "OM")
# replace existing tag column values as "1" if not NA and 0 otherwise
exonerees <- exonerees %>% 
  mutate(across(existing_extags, ~ ifelse(is.na(.), 0, 1)))
# attach existing to new tags columns
# VERIFY that columns are attached correctly!! 
tags_exoneration <- cbind(tags_exoneration, 
                          exonerees %>% select(-ID, existing_extags))

# currently, the separated exoneration tag columns are not attached to the 
# exoneration data because we do not have their meanings


### ============================= ###
### Time and ages between events 
### ============================= ###

# convert character to Date objects
exonerees <- exonerees %>%
  mutate(Date.of.1st.Conviction = lubridate::mdy(Date.of.1st.Conviction),
         Date.of.Release = mdy(Date.of.Release),
         Date.of.Exoneration = mdy(Date.of.Exoneration),
         Date.of.Posting = mdy(Posting.Date))

# extract the year of events
exonerees <- exonerees %>%
  mutate(
    # Year of conviction
    Year.of.1st.Conviction = lubridate::year(Date.of.1st.Conviction),
    # Year of release
    Year.of.Release = year(Date.of.Release),
    # Year of exoneration
    Year.of.Exoneration = year(Date.of.Exoneration),
    # Year of posting
    Year.of.Posting = year(Date.of.Posting))

# derive time intervals between events
exonerees <- exonerees %>%
  mutate(
    # time spent incarcerated (between conviction and release)
    Months.Incarcerated = lubridate::interval(Date.of.1st.Conviction,
                                              Date.of.Release) %/% months(1),
    Years.Incarcerated = floor(Months.Incarcerated / 12),
    # time spent convicted (between conviction and exoneration)
    Months.Convicted = interval(Date.of.1st.Conviction, 
                                Date.of.Exoneration) %/% months(1),
    Years.Convicted = floor(Months.Convicted / 12))
    # todo: time served in prison


### ============================= ###
### Derive new demographics
### ============================= ###

exonerees <- exonerees %>%
  mutate(
    # Age at conviction  = (Age at crime) + (Years between crime year and conviction year)
    Age.at.Conviction = Age.at.Crime + Year.of.1st.Conviction - Year.of.Crime,
    # Age at release = (Age at conviction) + (Years incarcerated)
    Age.at.Release = Age.at.Conviction + Years.Incarcerated,
    # Age at exoneration = (Age at conviction) + (Years convicted)
    Age.at.Exoneration = Age.at.Conviction + Years.Convicted,
    )


### ============================= ###
### Validate new variables
### ============================= ###

# verify age at crime is never less than age at conviction (and therefore less
# than age at subsequent events) 
exonerees %>%
  select(Age.at.Crime, Age.at.Conviction) %>%
  filter(Age.at.Crime > Age.at.Conviction) %>% 
  nrow(.) == 0


### ================ ###
### Final steps
### ================ ###

# separate demographic information
exonerees_demographics <- exonerees %>% 
  select(Last.Name, First.Name, 
         Race, Sex, 
         Age.at.Crime, Age.at.Conviction, Age.at.Release, Age.at.Exoneration)

# separate conviction information
exonerees_conviction <- exonerees %>%
  select(State, County, Worst.Crime.Display, 
         Year.of.Crime, Year.of.1st.Conviction)

# separate exoneration information
exonerees_exoneration <- exonerees %>%
  select(Sentence,
         Years.Incarcerated, Years.Convicted,
         Year.of.Release, Year.of.Exoneration, Year.of.Posting)

# select what to keep for total data set
exonerees <- exonerees %>% select(
  # demographics
  ID, Last.Name, First.Name, Race, Sex, 
  # crime details
  State, County, Worst.Crime.Display, Sentence,
  # conviction and exoneration tags
  existing_extags, new_contags,
  # time served
  Months.Incarcerated, Months.Convicted, Years.Incarcerated, Years.Convicted,
  # year of events
  Year.of.Crime, Year.of.1st.Conviction, Year.of.Release, Year.of.Exoneration,
  # age at events
  Age.at.Crime, Age.at.Conviction, Age.at.Release, Age.at.Exoneration)


### ================ ###
### Saving to CSV
### ================ ###

# tags
write.csv(tags_conviction, file = "data/exonerees-tags-conviction-may2024.csv", row.names = FALSE)
write.csv(tags_exoneration, file = "data/exonerees-tags-exoneration-may2024.csv", row.names = FALSE)
# information subsets
write.csv(exonerees_demographics, file = "data/exonerees-demographics-may2024.csv", row.names = FALSE)
write.csv(exonerees_conviction, file = "data/exonerees-conviction-may2024.csv", row.names = FALSE)
write.csv(exonerees_exoneration, file = "data/exonerees-exoneration-may2024.csv", row.names = FALSE)
# everything (selected above)
write.csv(exonerees, file = "data/exonerees-may2024.csv", row.names = FALSE)
