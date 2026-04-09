# Lees 1CHO in en maak uitvalbestand 
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021


library(tidyverse)
jaar = 2025

##### Data inlezen #####

# Basisbestand inlezen
if(!"Basisbestand1CHO" %in% ls()){
  Basisbestand1CHO <- readRDS(paste0("Output/", jaar, "/Basisbestand1CHO_", jaar, ".RDS"))
}


# Laatste rij selecteren per persnr, omdat dit het laatste jaar van inschrijving is
# Nieuwe variabele aanmaken om laatste jaar van inschrijving aan te geven
Uitstroom_instelling <- Basisbestand1CHO %>% 
  group_by(persoonsgebonden_nummer) %>% 
  arrange(desc(inschrijvingsjaar), soort_inschrijving_actuele_instelling, .by_group = TRUE) %>% 
  distinct(persoonsgebonden_nummer, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(laatste_jaar_inschrijving = case_when(
    
    # Zet laatste_jaar_inschrijving naar NA voor studenten die nog studeren en als inschrijvingsjaar niet bekend is. Anders: laatste_jaar_inschrijving is het inschrijvingsjaar
    inschrijvingsjaar == jaar-1 ~ NA_real_,
    is.na(inschrijvingsjaar) ~ NA_real_,
    TRUE ~ as.numeric(inschrijvingsjaar))) %>% 
  
  select(persoonsgebonden_nummer, inschrijvingsjaar, laatste_jaar_inschrijving)



# Foutmelding als er dubbele studentnummers in het diplomabestand zitten
if(any(duplicated(Uitstroom_instelling$persoonsgebonden_nummer))){
  stop("Dubbele studentnummers in het uitvalbestand gevonden!")
}



# Status-kolom: VH maakt onderscheid tussen studenten die diploma hebben behaald, studenten die uitgevallen zijn en 
# studenten die nog student zijn en dus nog ingeschreven staan (Elkaar uitsluitende categorieën). 
# Studenten die een diploma behalen in het lopend studiejaar en daarna verder studeren, worden in de kolom status 
# op Diploma behaald gezet. Zij kunnen daarna niet meer uitvallen en worden niet meer meegeteld als zittende student. 
# De studenten die nog ingeschreven staan worden onderverdeeld in studenten aan dezelfde studie 
# of studiewisselaars. Om de uitval te bepalen dus het diplomabestand koppelen aan het uitstroombestand
# en uitvalbestand aanmaken.
if(!"Diploma_instelling" %in% ls()){
  Diploma_instelling <- readRDS(paste0("Output/", jaar, "/Diploma_instelling_", jaar, ".RDS"))
}

if(!"Cohorten_Instroom" %in% ls()){
  # Om het uitvalbestand te kunnen koppelen aan het instroombestand, eerst het instroombestand inlezen en dan koppelen
  Cohorten_Instroom <- readRDS(file = paste0("Output/", jaar, "/Instroom_cohorten_", jaar, ".RDS"))
}

message("\nUITVAL --- Uitvalbestand wordt aangemaakt")

# Koppel berekende eerste jaar bij Avans uit het instroombestand om de uitvalvariabele te kunnen berekenen
Eerste_jaar_instelling = Cohorten_Instroom %>% 
  select(persoonsgebonden_nummer, eerstejaar_instelling)
# Waarom gebruik maken van eerstejaar_Avans en niet eerstejaardezeactinstelling? Deze hoeven niet gelijk te zijn, want
# voor eerstejaardezeactinstelling kan het eerste inschrijvingsrecord ook een na-inschrijving betreffen. Het eerstejaar_Avans
# is gebaseerd op verblijfsjaar == 1, dus is het eerste volledige studiejaar bij na-inschrijving. Keuze is gemaakt bij het
# aanmaken van het instroombestand om variabele verblijfsjaar = 1 te gebruiken. Dit houdt in dat we voor na-inschrijvers het 
# eerstvolgende studiejaar gebruiken als jaar van instroom. Dus hiermee is tevens bepaald dat we eerstejaar_Avans gebruiken als jaar van instroom.


Uitval_indicatoren = Uitstroom_instelling %>% 
  left_join(Diploma_behaald, by="persoonsgebonden_nummer") %>% 
  left_join(Eerste_jaar_instelling, by="persoonsgebonden_nummer") %>% 
  # Bestand Eerstejaar heeft minder regels dan Uitstroom bestand, omdat er bij eerstejaar via de variabele verblijfsjaaractinstelling == 1
  # geselecteerd wordt op actief op peildatum 1 okt. Dit doen we niet bij uitval, dus studenten die wel een uitvalregel hebben
  # voor een opleiding, maar geen actieve inschrijving bij instroom hebben verschijnen wel in het bestand, maar met NA voor eerstejaar_Avans,
  # bv 000010488558
  
  
  # Maak kolom status met drie opties: diploma behaald, zittend, of uitgevallen
  mutate(status = case_when(diploma == "Diploma behaald (excl. propedeuse)" ~ "Diploma behaald",
                            
                            inschrijvingsjaar == jaar-1 ~ "Zittend",
                            
                            TRUE ~ "Uitgevallen")) %>% 
  mutate(status = as.factor(status)) %>% 
  
  #Variabelen aanmaken voor uitgevallen na x jaar, na 1 jaar en na 3 jaar.
  mutate(uitval_xjr = case_when(status == "Uitgevallen" ~ laatste_jaar_inschrijving + 1 - eerstejaar_instelling),
         # Studenten die geen eerstejaar Avans hebben, omdat ze toen geen actieve inschrijving hadden, hebben in
         #het uitvalbestand voor uitvalxjr NA, dus moeten ook NA voor uitval1jr en uitval3jr krijgen.
         #Bij de koppeling met het instroombestand vallen deze studenten er sowieso uit, want instroombestand 
         #is gebaseerd op verblijfsjaaractinst == 1 en dat gaat uit van actief op 1 okt.
         uitval_1jr = case_when(
                                # is.na(uitval_xjr) ~ NA_character_,
                               uitval_xjr == 1 ~ "Uitgevallen binnen 1 jaar",
                               TRUE ~ "Na 1 jaar nog ingeschreven of diploma behaald"),
         
         uitval_3jr = case_when(
                                # is.na(uitval_xjr) ~ NA_character_,
                               uitval_xjr <= 3 ~ "Uitgevallen binnen 3 jaar", 
                               TRUE ~ "Na 3 jaar nog ingeschreven of diploma behaald")) %>% 
  # Omzetten naar factors
  mutate(uitval_1jr = factor(uitval_1jr),
         uitval_3jr = factor(uitval_3jr)) %>% 
  
  select(persoonsgebonden_nummer, laatste_jaar_inschrijving, diploma, status, starts_with("uitval"))


# Check: Indicatie aantal zittende studenten
table(Uitval_indicatoren$status)
# Check of alle statussen zijn ingevuld
if(any(is.na(Uitval_indicatoren$status))){
  stop("Niet alle statussen zijn gevuld")
}



# Uitvalbestand opslaan
message("UITVAL --- Uitvalbestand wordt opgeslagen")
saveRDS(Uitval_indicatoren, file = paste0("Output/", jaar, "/Uitval_", jaar, ".RDS"))


