# Lees het 1CHO in en maak een cohortbestand (één regel per student)
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

library(tidyverse)
jaar = 2025


##### Data inlezen #####

Basisbestand1CHO = read_csv(paste0("../", jaar, "/Output CEDA tool/EV07GR25_combined.csv"), locale=locale(encoding="UTF-8"))


## Maak bestand instroomcohort (1 regel per student)
# Selecteer op hoofdinschrijving
# Selecteer op inschrijvingen actief op 1 oktober
# Selecteren op verblijfsjaar==1 (nieuwe instromers) 


Cohorten_Instroom = Basisbestand1CHO %>% 
  
  # Selecteer alleen hbo 
  filter(soort_hoger_onderwijs %in% c("hoger beroepsonderwijs", "hbo")) %>%  
  
  # Selecteer op hoofdinschrijving en op eerste jaar bij de instelling
  filter(
    soort_inschrijving_actuele_instelling_label == "hoofdinschrijving binnen het domein actuele instelling",
    verblijfsjaar_actuele_instelling == 1) %>% 
  mutate(eerstejaar_instelling = inschrijvingsjaar)




# Foutmelding als er dubbele studentnummers in het instroomcohortbestand zitten
if(any(duplicated(Cohorten_Instroom$persoonsgebonden_nummer))){
  stop("Dubbele studentnummers in het instroomcohortbestand gevonden!")
}



#Instroombestand en basisbestand opslaan
message("INSTROOM --- Cohortbestand wordt opgeslagen")
saveRDS(Cohorten_Instroom, file = paste0("Output/", jaar, "/Instroom_cohorten_", jaar, ".RDS"))
saveRDS(Basisbestand1CHO, file = paste0("Output/", jaar, "/Basisbestand1CHO_", jaar, ".RDS"))



