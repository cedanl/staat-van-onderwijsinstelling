# Lees de bestanden voor rendement, uitval en studiewissel in en combineer in één bestand 
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

# TO DO: Bij de aanpassingen aan opleidingsniveau en rendement ook ad meenemen, want dat gebeurt nu in de vervolgscripts.


library(tidyverse)
jaar = 2025

##### Data inlezen #####

## Bestand Rendement, Uitval en Switch inlezen 
Rendement_indicatoren <- readRDS(paste0("Output/", jaar, "/Rendement_", jaar, ".RDS"))
Uitval_indicatoren <- readRDS(paste0("Output/", jaar, "/Uitval_", jaar, ".RDS"))
Studiewissel_indicatoren <- readRDS(paste0("Output/", jaar, "/Studiewissel_", jaar, ".RDS"))

# # Avanssectoren inlezen (interessegebieden)
# # Lian maakt update van document Avanssectoren 
# Avans_sector = read_csv2("Avanssectoren_2024.csv") %>%
#   mutate(oplactequivalent = as.factor(oplactequivalent)) 
# 
# # Code nog aan te passen na update document Clusters_academies
# # Lian maakt update bestand Clusters_academies
# clusters = read.csv2("Clusters_academies.csv") %>% 
#   select(opleidingscode = croho_actueel, 
#          opleidingsvorm = opleidingsvorm3, 
#          locatie, academie, cluster, cluster_naam) %>% 
#   mutate(locatie = ifelse(str_detect(locatie, "Hertogenbosch"), "Den Bosch", locatie),
#          opleidingsvorm = ifelse(opleidingsvorm=="Duaal", "duaal", tolower(opleidingsvorm))) %>% 
#   mutate_all(as.factor) %>% 
#   select(-academie) %>% 
#   distinct() %>% 
#   rename(academie = cluster, academie_naam = cluster_naam)

# # Relevante kolommen selecteren
# Uitval_kolommen <- Uitval %>% 
#   select(persnr, status:uitval3jr)
# 
# Switch_kolommen <- Switch %>% 
#   select(persnr, studiewissel1jr:HBOsector_na_switch3jr) 


message("\n\nSTAAT VAN AVANS --- Voeg alle data samen")


# Kolommen Uitval en Switch toevoegen aan bestand Rendement
Data_1cHO_indicatoren = Cohorten_Instroom  %>%
  left_join(Rendement_indicatoren) %>% 
  left_join(Uitval_indicatoren, by="persoonsgebonden_nummer") %>% 
  left_join(Studiewissel_indicatoren, by="persoonsgebonden_nummer") %>%
  # left_join(Avans_sector, by="oplactequivalent") %>% # Avans-sectoren toevoegen aan bestand
  
  
  # Locatie toevoegen
  mutate(locatie = case_when(vestigingsnummer_actueel == 0 ~ "Breda",
                             vestigingsnummer_actueel == 4 ~ "Tilburg",
                             vestigingsnummer_actueel == 6 ~ "Den Bosch",
                             vestigingsnummer_actueel == 7 ~ "Roosendaal"
                             )) %>%
  
  # Volgorde kolommen en namen wijzigen  
  select(inschrijvingsjaar, 
         geslacht = geslacht_label,
         locatie,
         opleidingscode = opleiding_actueel_equivalent,
         # opleidingsnaam = isat,
         opleidingsvorm = opleidingsvorm_label,
         opleidingsniveau = type_hoger_onderwijs_binnen_soort_hoger_onderwijs,
         # vooropleiding,
         int_student = indicatie_internationale_student_label,
         indicatie_EER = indicatie_eer_actueel_label,
         # MBOdomein,
         # VOprofielCM:VOprofielNT,
         HBOsector = croho_onderdeel_actuele_opleiding_label,
         # avans_sector,
         leeftijd_bij_instroom = leeftijd_per_peildatum_1_oktober,
         # leeftijdgroep_instroom = leeftijdgroep_instroom1okt,
         postcode4_student_1okt = postcodecijfers_student_op_1_oktober,
         postcode4_vooropleiding_voorHO = postcodecijfers_van_de_hoogste_vooropl._voor_het_ho,
         status, 
         # jaar_eerste_diploma_avans = firstdiplomajaar1, 
         soortdiploma = soort_diploma_instelling_label,
         rendement_3jr:rendement_8jr, 
         uitval_xjr:HBOsector_na_switch3jr) %>% 
  
  
  # Kolommen omzetten naar factor
  mutate(opleidingscode = factor(opleidingscode),
         # avans_sector = factor(avans_sector),
         locatie = factor(locatie),
         opleidingsvorm = factor(opleidingsvorm),
         postcode4_student_1okt = factor(postcode4_student_1okt), 
         postcode4_vooropleiding_voorHO = factor(postcode4_vooropleiding_voorHO)) %>% 
  
  # Variabelen opschonen
  mutate(
    # vooropleiding = fct_recode(vooropleiding, 
    #                                 "mbo" = "mbo 4",
    #                                 NULL = "mbo overig",
    #                                 "onbekend / overig" = "vooropleiding onbekend",
    #                                 "onbekend / overig" = "overig"),
         
         opleidingsvorm = fct_recode(opleidingsvorm, 
                                     "duaal" = "coop-student of duaal onderwijs (vanaf het studiejaar 1998-1999)"),
         
         HBOsector = fct_recode(HBOsector, 
                                "gedrag & maatschappij" = "gedrag en maatschappij",
                                "taal & cultuur" = "taal en cultuur"),
         
         # MBOdomein = fct_recode(MBOdomein, 
         #                        NULL = "vooropleiding anders dan mbo",
         #                        NULL = "mbo algemeen"),
         
         postcode4_student_1okt = fct_recode(postcode4_student_1okt, 
                                             NULL = "0010",
                                             NULL = "0020",
                                             NULL = "0030",
                                             NULL = "0040"
         ),
         
         postcode4_vooropleiding_voorHO = fct_recode(postcode4_vooropleiding_voorHO, 
                                                     NULL = "0010",
                                                     NULL = "0020",
                                                     NULL = "0030",
                                                     NULL = "0040"
         ),
         
         opleidingsniveau = fct_recode(opleidingsniveau, 
                                       NULL = "postinitiele master",
                                       bachelor = "ba",
                                       master = "ma"),
         
         # leeftijdgroep_instroom = fct_collapse(leeftijdgroep_instroom,
         #                                       "30+ jaar" = c("30-34 jaar", "35-39 jaar", "40-44 jaar", "45 jaar of ouder"))
         
  ) %>% 
  
  # Gecombineerde kolommen
  mutate(uitval = case_when(uitval_xjr == 1 ~ "Uitgevallen binnen 1 jaar",
                            uitval_xjr %in% 2:3 ~ "Uitgevallen in 2e of 3e jaar",
                            uitval_xjr > 3 ~ "Uitgevallen na 3 jaar",
                            TRUE ~ "Niet uitgevallen"),
         
         studiewissel = case_when(studiewissel_1jr == "Gewisseld binnen 1 jaar" ~ "Gewisseld binnen 1 jaar",
                                  studiewissel_3jr == "Gewisseld binnen 3 jaar" ~ "Gewisseld in het 2e of 3e jaar",
                                  TRUE ~ "Niet gewisseld"),
         
         rendement = case_when(rendement_5jr == "Diploma binnen 5 jaar" ~ "Diploma binnen 5 jaar",
                               rendement_8jr == "Diploma binnen 8 jaar" ~ "Diploma binnen 5-8 jaar",
                               rendement_8jr == "Diploma na 8 jaar" ~ "Diploma na 8 jaar",
                               rendement_8jr == "Geen diploma" ~ "Geen diploma")
         
  ) #%>% 


# # Voeg clusters toe
# left_join(clusters) 


# Samengevoegd bestand opslaan
message("STAAT VAN AVANS --- Sla gecombineerde data op")
saveRDS(Data_1cHO_indicatoren, file = paste0("Output/", jaar, "/Indicatoren_1cHO_", jaar, ".RDS"))
