library(tidyverse)
library(haven)

office_visits <- list()
outpatient_visits <- list()
for (year in 2020:2022){
  file_name <- paste0('F:/data/MEPS/Outpatient/Outpatient', year, '.dta')
  df <- read_dta(file_name)
  outpatient_visits[[year]] <- df
  
  
}


for (year in 2020:2022){
  file_name <- paste0('F:/data/MEPS/OfficeBased/OfficeBased', year, '.dta')
  df <- read_dta(file_name)
  office_visits[[year]] <- df
  
  
}


outpatient_visits <- compact(outpatient_visits)
office_visits <- compact(office_visits)

for (i in seq_along(outpatient_visits)){
  year <- 2020 + i - 1
  
  outpatient_visits[[i]]$year <- year
}


for (i in seq_along(office_visits)){
  year <- 2020 + i - 1
  
  office_visits[[i]]$year <- year
}




outpatient_visits <- bind_rows(outpatient_visits)
office_visits <- bind_rows(office_visits)

outpatient_visits$outpatient_visit <- 1
outpatient_visits$office_visit <- 0

office_visits$outpatient_visits <- 0
office_visits$office_visit <- 1


outpatient_visits_derm <- outpatient_visits %>% 
  filter(DRSPLTY_M18 == 4)

office_visits_derm <- office_visits %>% 
  filter(DRSPLTY_M18 == 4)



derm_visits <- bind_rows(office_visits_derm, outpatient_visits_derm)

derm_visits <- derm_visits %>% 
  mutate(perwt = ifelse(!is.na(PERWT20F), PERWT20F, PERWT21F))

derm_visits <- derm_visits %>% 
  mutate(outpatient_sf_20 = OPFSF20X + OPDSF20X,
         outpatient_sf_21 = OPFSF21X + OPDSF21X,
         outpatient_sf_22 = OPFSF22X + OPDSF22X,
         outpatient_xp_20 = OPFXP20X + OPDXP20X,
         outpatient_xp_21 = OPFXP21X + OPDXP21X,
         outpatient_xp_22 = OPFXP22X + OPDXP22X)

derm_visits <- derm_visits %>% 
  mutate(sf = coalesce(OBSF20X, OBSF21X, OBSF22X, outpatient_sf_20, outpatient_sf_21, outpatient_sf_22),
         xp = coalesce(OBXP20X, OBXP21X, OBXP22X, outpatient_xp_20, outpatient_xp_21, outpatient_xp_22))

summary(derm_visits$sf)
summary(derm_visits$xp)

derm_visits <- derm_visits %>% 
  mutate(sf = case_when(year == 2021 ~ sf * 0.97582,
                                       year == 2020 ~ sf * 0.956494,
                        TRUE ~ sf),
         xp = case_when(year == 2021 ~ xp * 0.97582,
                                       year == 2020 ~ xp * 0.956494,
                        TRUE ~ xp))

summary(derm_visits$sf)
summary(derm_visits$xp)


derm_visits <- derm_visits %>% 
  mutate(tele_sf = ifelse(derm_visits$TELEHEALTHFLAG == 1, derm_visits$sf, 0),
         tele_xp = ifelse(derm_visits$TELEHEALTHFLAG == 1, derm_visits$xp, 0))

derm_visits$visit <- 1

derm_visits$telehealth <- ifelse(derm_visits$TELEHEALTHFLAG == 1, 1, 0)


derm_visits_subset <- derm_visits %>% 
  select(DUPERSID, year, sf, xp, visit, telehealth, tele_sf, tele_xp) %>% 
  group_by(DUPERSID, year) %>% 
  summarize(total_visits = sum(visit),
            total_telehealth_visits = sum(telehealth),
            oop = sum(sf),
            total_spend = sum(xp),
            tele_oop = sum(tele_sf),
            tele_total_spend = sum(tele_xp))

write_dta(derm_visits, "F:/projects/telederm/data/derm_visits.dta")

write_dta(derm_visits_subset, "F:/projects/telederm/data/summarized_derm_visits.dta")
