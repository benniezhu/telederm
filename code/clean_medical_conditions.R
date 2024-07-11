library(tidyverse)
library(haven)

#load data

Medical_Conditions_dfs <- list()

for (year in 2020:2021){
  file_name <- paste0('F:/data/MEPS/Medical_Conditions/MedicalConditions', year, '.dta')
  df <- read_dta(file_name)
  Medical_Conditions_dfs[[year]] <- df
}

Medical_Conditions_dfs <- compact(Medical_Conditions_dfs)

for (i in seq_along(Medical_Conditions_dfs)){
  year <- 2020 + i - 1
  Medical_Conditions_dfs[[i]]$year <- year
}

Medical_Conditions_dfs <- bind_rows(Medical_Conditions_dfs)

table(Medical_Conditions_dfs$year)


#Regarding clinical scenarios that are best for video visits, follow up visits for acne, eczema and psoriasis 
#are well suited from both a patient and dermatologist perspective. Skin exams, 
#examination of pigmented lesions and hair loss are conditions that are less well suited clinically
#for teledermatology.

#icd codes

acne_icd <- c('L70', '706')

eczema_icd <- c('L20', '691', '692')

psoriasis_icd <- c('L40' , '696')

hair_loss_icd <- c('L63', 'L64', 'L65', 'L66', '704')

icd_10_fx <- function()
{
  
  L20  Atopic dermatitis
  L21  Seborrheic dermatitis
  L22  Diaper dermatitis
  L23  Allergic contact dermatitis
  L24  Irritant contact dermatitis
  L25  Unspecified contact dermatitis
  L26  Exfoliative dermatitis
  L27  Dermatitis due to substances taken internally
  L28  Lichen simplex chronicus and prurigo
  L29  Pruritus
  L30  Other and unspecified dermatitis
  
  
  L40  Psoriasis
  L41  Parapsoriasis
  L42  Pityriasis rosea
  L43  Lichen planus
  L44  Other papulosquamous disorders
  L45  Papulosquamous disorders in diseases classified elsewhere
  
  L63  Alopecia areata
  L64  Androgenic alopecia
  L65  Other nonscarring hair loss
  L66  Cicatricial alopecia [scarring hair loss]
  L67  Hair color and hair shaft abnormalities
  L68  Hypertrichosis
  L70  Acne
  
}

icd_9_fx <- function()
{
  690 Erythematosquamous dermatosis
  691 Atopic dermatitis and related conditions
  692 Contact dermatitis and other eczema
  693 Dermatitis due to substances taken internally
  694 Bullous dermatoses
  695 Erythematous conditions
  696 Psoriasis and similar disorders
  697 Lichen
  698 Pruritus and related conditions
  
  700 Corns and callosities
  701 Other hypertrophic and atrophic conditions of skin
  702 Other dermatoses
  703 Diseases of nail
  704 Diseases of hair and hair follicles
  705 Disorders of sweat glands
  706 Diseases of sebaceous glands
  707 Chronic ulcer of skin
  708 Urticaria
  709 Other disorders of skin and subcutaneous tissue
  
}



