library(tidyverse)
library(haven)

clink <- list()
for (year in 2020:2021){
  file_name <- paste0('F:/data/MEPS/clink/clink', year, '.dta')
  df <- read_dta(file_name)
  clink[[year]] <- df
  
  
}

clink <- compact(clink)

for(i in seq_along(clink)){
  year <- 2020 + i - 1
  clink[[i]]$year <- year 
}

clink <- bind_rows(clink)

# Filter CLNK file to only office-based visits --------------------------------
#  EVENTYPE:
#   1 = "Office-based"
#   2 = "Outpatient" 
#   3 = "Emergency room"
#   4 = "Inpatient stay"
#   7 = "Home health"
#   8 = "Prescribed medicine" 

clink_filtered <- clink %>% 
  filter(EVENTYPE == 1 | EVENTYPE == 2)





derm_visits <- read_dta("F:/projects/telederm/data/derm_visits.dta") %>% 
  mutate(count = 1, 
         domain = 1)


FYC_dfs <- read_dta("F:/projects/telederm/data/FYC_dfs.dta") %>% 
  select(-VARSTR, -VARPSU, -PERWT20F, -PERWT21F, -perwt, -DUID, -PID, -PANEL)

#merge datasets 

derm_FYC <- full_join(derm_visits, FYC_dfs, by = c('DUPERSID', 'year'))



write_dta(derm_FYC, "F:/projects/telederm/data/ml_ready/derm_FYC.dta")
