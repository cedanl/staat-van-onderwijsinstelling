# Lees 1CHO in en maak diplomabestand 
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

library(tidyverse)
jaar = 2025

# Basisbestand inlezen
if(!"Basisbestand1CHO" %in% ls()){
  Basisbestand1CHO <- readRDS(paste0("Output/", jaar, "/Basisbestand1CHO_", jaar, ".RDS"))
}

#### OPMERKING: diploma-labels niet goed gekoppeld (alleen code 13, 15 en 99 komen mee, niet 1 t/m 5)
# Hoofddiploma ad en hoofddiploma postinitiele master (99 is een foutcode)
# Voor nu: niet selecteren op labels maar op cijfers



{
  ## Lijst van mogelijke diploma's excl. propedeusediploma's
diplomas = c("Hoofd-bachelor-diploma binnen de actuele instelling",
             "Neven-bachelor-diploma binnen de actuele instelling",
             "Hoofd-master-diploma binnen de actuele instelling",
             "Neven-master-diploma binnen de actuele instelling",
             "Hoofd-doctoraal-diploma binnen de actuele instelling",
             "Neven-doctoraal-diploma binnen de actuele instelling",
             "Hoofddiploma beroepsfase/voortgezet binnen de actuele instelling",
             "Nevendiploma beroepsfase/voortgezet binnen de actuele instelling",
             "Hoofddiploma associate degree binnen de actuele instelling",
             "Nevendiploma associate degree binnen de actuele instelling",
             "Hoofddiploma postinitiele master binnen de actuele instelling",
             "Nevendiploma postinitiele master binnen de actuele instelling")

} 
message("\nRENDEMENT --- Diplomabestand wordt aangemaakt")


# Selecteren op diploma behaald bij Avans, m.u.v. propedeusediploma, groeperen per Persnr, eerste diplomajaar op eerste rij plaatsen voor elk Persnr, eerste rij selecteren per Persnr, benodigde variabelen selecteren en evt hernoemen
Diploma_behaald = Basisbestand1CHO %>% 
  
  # Selecteer juiste diploma's (niet propedeuse)
  filter(soort_diploma_instelling %in% c(3,4,5,13,15)) %>% 
  
  # Sorteer per persoonsnummer en diplomajaar, behoud alleen eerstbehaalde diploma
  mutate(diplomajaar = na_if(diplomajaar, 0)) %>% 
  group_by(persoonsgebonden_nummer) %>% 
  arrange(diplomajaar, .by_group = TRUE) %>% 
  distinct(persoonsgebonden_nummer, .keep_all = TRUE) %>% 
  ungroup() %>% 
  
  # Geef extra diplomagegevens mee
  mutate(jaar_eerste_diploma = diplomajaar,
         verblijfsjaar_eerste_diploma = verblijfsjaar_actuele_instelling,
         diploma = "Diploma behaald (excl. propedeuse)") %>% 
  select(persoonsgebonden_nummer, jaar_eerste_diploma, verblijfsjaar_eerste_diploma, diploma)


# Foutmelding als er dubbele studentnummers in het diplomabestand zitten
if(any(duplicated(Diploma_behaald$persoonsgebonden_nummer))){
  stop("Dubbele studentnummers in het diplomabestand gevonden!")
}



# Diplomabestand opslaan, omdat dit bestand bij uitval nodig is
saveRDS(Diploma_behaald, file = paste0("Output/", jaar, "/Diploma_instelling_", jaar, ".RDS"))



###### Koppeling met instroomcohorten ###### 

#Om het diplomabestand te kunnen koppelen aan het instroombestand, eerst het instroombestand inlezen en dan koppelen
message("RENDEMENT --- Diplomabestand wordt opgeslagen")

if(!"Cohorten_Instroom" %in% ls()){
  Cohorten_Instroom = readRDS(paste0("Output/", jaar, "/Instroom_cohorten_", jaar, ".RDS"))
}

message("RENDEMENT --- Rendementsbestand wordt aangemaakt")

# Rendementsbestand: combineer cohortbestand en diplomabestand
Rendement_indicatoren = Cohorten_Instroom %>% 
  left_join(Diploma_behaald, by="persoonsgebonden_nummer") %>% 
  select(persoonsgebonden_nummer, eerstejaar_instelling, jaar_eerste_diploma, verblijfsjaar_eerste_diploma, diploma) %>% 
  mutate(

    #eerstejaar_instelling is gebaseerd op verblijfsjaaractueleinstelling, dus voor na-inschrijvers is dit het eerste gehele studiejaar.
    #Bij verblijfsjaar_diploma worden alleen de jaren geteld met een actieve inschrijving
    #Bij de berekende variabele hieronder voor studieduur worden alle tussenliggende jaren geteld (ook tussenjaren meegeteld)
    # Dit zijn aantal kalenderjaren, niet verblijfsjaren
    rendement_xjaar = jaar_eerste_diploma - eerstejaar_instelling + 1,
    
    rendement_3jr = case_when(rendement_xjaar <= 3 ~ "Diploma binnen 3 jaar",
                              rendement_xjaar > 3 ~ "Diploma na 3 jaar",
                              is.na(jaar_eerste_diploma) ~ "Geen diploma",
                              jaar_eerste_diploma < eerstejaar_instelling ~ "Onbekend, want diplomajaar ligt voor eerste jaar bij Avans"),
  
  rendement_5jr = case_when(rendement_xjaar <= 5 ~ "Diploma binnen 5 jaar",
                            rendement_xjaar > 5 ~ "Diploma na 5 jaar",
                            is.na(jaar_eerste_diploma) ~ "Geen diploma",
                            jaar_eerste_diploma < eerstejaar_instelling ~ "Onbekend, want diplomajaar ligt voor eerste jaar bij Avans"),
  
  rendement_8jr = case_when(rendement_xjaar <= 8 ~ "Diploma binnen 8 jaar",
                            rendement_xjaar > 8 ~ "Diploma na 8 jaar",
                            is.na(jaar_eerste_diploma) ~ "Geen diploma",
                            jaar_eerste_diploma < eerstejaar_instelling ~ "Onbekend, want diplomajaar ligt voor eerste jaar bij Avans")
  
  
  ) %>% 
  mutate(across(starts_with("rendement"), as.factor))
  
  
# Rendementbestand opslaan
message("RENDEMENT --- Rendementsbestand wordt opgeslagen")
saveRDS(Rendement_indicatoren, file = paste0("Output/", jaar, "/Rendement_", jaar, ".RDS"))



