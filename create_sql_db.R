library(tidyverse)
library(RSQLite)

stops_df <- readRDS("../data/statewide/2002_2021/statewide_2002_21.rds") %>%
  # Rename columns to remove spaces
  rename(agency_code = `Agency Code`,
         agency = `Issuing Agency`,
         officer = `Officer Id`,
         citation = `Citation #`,
         type = Type,
         hour = `Time-HH`,
         minute = `Time-MM`,
         am_pm = `AM-PM`,
         loc_code = `Location ID`,
         loc = `Location Name`,
         offense = Offense,
         offense_desc = `Offense Description`,
         race = Race,
         gender = `Gender`
  ) %>%
  # Remove duplicate date column
  select(-`Event/Written Date`) %>%
  # Reduce time columns to just two (no am/pm)
  mutate_at(vars(hour, minute), as.numeric) %>%
  mutate(type = str_to_title(type), # Also format type in Title Case
         hour_24 = ifelse(am_pm == "PM", hour + 12, hour),
         date = as.Date(date)) %>% 
  select(-hour, -am_pm) %>%
  # Whiddle down to fewer columns
  select(agency, officer, citation, type, date, hour_24, minute, loc, offense=offense_desc, race, gender)  %>%
  # Group race and gender categories
  mutate_at(vars(race, gender), tolower) %>%
  mutate(race = case_when(
    race %in% c("black", "africa", "capver") ~ "Black",
    race %in% c("unk", "unknwn") ~ "Unknown",
    race %in% c("cau", "white") ~ "White",
    race %in% c("native", "indian", "americ") ~ "Native American",
    race %in% c("asian", "aspac", "pacifc") ~ "Asian",
    T ~ str_to_title(race)
  ),
  gender = case_when(
    gender %in% c("np", "non-binary") ~ "Other",
    gender %in% c("unk", NA) ~ "Unknown",
    T ~ str_to_title(gender)
  )) %>%
  # Group agencies
  mutate(agency = case_when(
    grepl("State Police", agency) ~ "Massachusetts State Police (MSP)",
    grepl("Boston Police", agency) ~ "Boston Police Dept.",
    T ~ agency)) %>%
  # Apply order to race and gender
  mutate(race = case_when(
    is.na(race) ~ "Unknown",
    race == "Hisp" ~ "Hispanic/Latinx",
    race == "Midest" ~ "Middle Eastern",
    T ~ race
  ),
  race = factor(race, levels = c("White", "Black", "Hispanic/Latinx", "Asian", "Middle Eastern", "Native American", "Unknown")),
  gender = factor(gender, levels = c("Male", "Female", "Other", "Unknown"))) %>%
  # Get rid of locations that don't vibe with what's in the MassGIS shapefile
  filter(!loc %in% c("New Hampshire", "Not Assigned", "Other")) %>%
  mutate(loc = case_when(
    loc == "Manchester" ~ "Manchester-By-The-Sea",
    loc %in% c("Allston", "Brighton", "Charlestown", "Dorchester", 
               "E Boston", "Forest Hills", "Hyde Park", "Jamaica Plain", 
               "Mattapan", "Roslindale", "Roxbury", "S Boston", "W Roxbury") ~ "Boston",
    T ~ loc
  ))

dbConnect(SQLite(), 
          dbname="app/data/statewide_2002_21_ed.sqlite") %>%
dbWriteTable(name =  "statewide_2002_21", 
             collect(stops_df),
             row.names=FALSE, overwrite=TRUE,
             append=FALSE, 
             field.types=c(agency="text", officer="text", 
                           citation="text", type="text", date="integer", hour_24="integer", 
                           minute="integer", loc="text", offense="text",
                           race="text", gender="text"))

