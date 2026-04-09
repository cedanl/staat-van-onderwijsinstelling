# Lees 1CHO in en maak switchbestand 
# Veerle van Son & Damiëtte Bakx-van den Brink
# mei 2021

## CHECK: Studenten die een AD-diploma en doorgaan met een bachelor -> niet als switcher aanmerken
# persnr 000011600998

# Studieonderbrekers? --> de nieuwe variabele voor verblijfsjaar (verblijfsjaaractueleinstelling) telt door, 
# dus kun je niet zien of iemand een studieonderbreker is. Voorheen werden studieonderbrekers niet meegenomen in 
# het switchbestand, omdat het eerder berekende verblijfsjaar was afgeleid van inschrijvingsjaar min eerstejaarinstelling. 
# Bij een studieonderbreking komen bepaalde verblijfsjaren dan niet voor. 

# Diplomabestand --> Bevat studenten die in verblijfsjaar 0 al hun diploma halen. Hoe kan dat? Antwoord: verblijfsjaar = 0 als de inschrijving niet actief is op 1 okt
# dus een student die bij nainschrijving een diploma haalt, heeft verblijfsjaar=0.

# Switch voortaan ook alleen bekijken bij voltijd bachelor en voltijd ad (grafieken). Voor deeltijd zijn er teveel ongeregeldheden met diploma en studieduur 
# (bijv diploma gekregen voor eerste actieve studiejaar bij Avans). Deeltijd studenten hebben vaak dubbel inschrijvingen, waarbij niet duidelijk is 
# welke de hoofd- en welke de neveninschrijving is. Vrijstellingen? Losse modules? 

# Switch binnen 3 jaar is incl switch binnen 1 jaar, maar het verschil tussen switch 1 jr en switch 3 jaar is niet de switch  in het 2de of 3de jaar tov het eerste jaar,
# want het wordt alleen op de zittende studenten bepaald. De groepen zijn niet precies hetzelfde, doordat er meer studenten zijn 
# die zijn uitgevallen of diploma behaald in het 2de en 3de jaar. 

library(tidyverse)
jaar = 2025

##### Data inlezen #####

# Basisbestand inlezen
if(!"Basisbestand1CHO" %in% ls()){
  Basisbestand1CHO <- readRDS(paste0("Output/", jaar, "/Basisbestand1CHO_", jaar, ".RDS"))
}

# Lees diplomabestand in, zodat studenten met een diploma niet als switchers worden aangemerkt
if(!"Diploma_behaald" %in% ls()){
  Diploma_behaald <- readRDS(paste0("Output/", jaar, "/Diploma_instelling_", jaar, ".RDS"))
}

# Lees instroombestand in
if(!"Cohorten_Instroom" %in% ls()){
  Cohorten_Instroom <- readRDS(file = paste0("Output/", jaar, "/Instroom_cohorten_", jaar, ".RDS"))
}




# Uitval binnen 1 en 3 jaar (om deze studenten eruit te kunnen halen)
uitval_1_3_jaar = Uitval_indicatoren %>% 
  select(persoonsgebonden_nummer, uitval_xjr) %>% 
  mutate(uitval_een_drie = case_when(uitval_xjr == 1 ~ "binnen 1 jaar", # uitval_xjr kan niet kleiner dan 1 zijn , want is gebaseerd op: case_when(status == "Uitgevallen" ~ laatste_jaar_inschrijving + 1 - eerstejaar_Avans)
                                     uitval_xjr > 1 & uitval_xjr <= 3 ~ "binnen 2- en 3de jaar",
                                     TRUE ~ "niet uitgevallen eerste drie jaar"
  ),
  uitval_een_drie = factor(uitval_een_drie))




# Cohortbestand en diplomabestand koppelen
# Lijst maken voor wie switch moet worden bepaald (studenten die binnen 1 / 3 jaar niet hun diploma hebben behaald)
zittend_per_cohort = Cohorten_Instroom %>% 
  select(-soort_diploma_instelling_label) %>% 
  left_join(Diploma_behaald) %>% 
  select(persoonsgebonden_nummer, inschrijvingsjaar, eerstejaar_instelling, jaar_eerste_diploma, opleidingsvorm_label) %>% #opleidingsfase_label
  # eerstejaar_Avans is gebaseerd op verblijfsjaaractinst == 1, dus na-inschrijvers tellen mee vanaf het eerste volledige studiejaar,
  # firstdiplomajaar1 is gebaseerd op studiejaar waarin diploma is behaald,
  # opleidingsvorm en opleidingsfase zijn gebaseerd op jaar van instroom.
  
  # filter(opleidingsvorm == "voltijd") %>% 
  
  # Bereken het verblijfsjaar waarin het diploma werd behaald
  mutate(diploma_na_x_jaar = jaar_eerste_diploma - eerstejaar_instelling + 1,
         diploma_een_drie = case_when(diploma_na_x_jaar <= 1 ~ "binnen 1 jaar of daarvoor", # diploma_na_x_jaar kan ook negatief of nul zijn als diploma bv behaald is bij een Ad inschrijving
                                      # die niet actief is op 1 okt en daar diploma in dat jaar behaald en daarna actief op 1 okt instroomt bij ba.
                                      # Hier wel <=1 aanhouden, want studenten die in een inschrijvingsjaar dat niet actief op 1 okt is , maar wel een diploma, bv Ad-diploma
                                      # halen in dat jaar en daarna instromen bij de ba, kunnen niet meer aangemerkt worden als switcher omdat zij een diploma behaald hebben.
                                      # bv 000198605961
                                      diploma_na_x_jaar > 1 & diploma_na_x_jaar <= 3 ~ "binnen 2- en 3de jaar",
                                      TRUE ~ "geen diploma in eerste drie jaar"
         ),
         diploma_een_drie = factor(diploma_een_drie)) %>% 
  left_join(uitval_1_3_jaar) 
#Omdat de variabele uitval na x jaar al berekend is in het uitvalbestand, kunnen we hier niet in dezelfde lijn als bij diploma 
#uitval_1_3_jaar koppelen. Daarom uitvalbestand met uitval na 1 en 3 jaar eerder in een apart bestand aangemaakt.

zittend_1jr = zittend_per_cohort %>% 
  # Haal studenten die binnen 1 jaar hun diploma halen eruit
  filter(diploma_een_drie != "binnen 1 jaar of daarvoor") %>% 
  filter(uitval_een_drie != "binnen 1 jaar")


zittend_3jr = zittend_per_cohort %>% 
  # Haal studenten die binnen 3 jaar hun diploma halen eruit, evenals uitval binnen 3 jaar
  filter(diploma_een_drie != "binnen 1 jaar of daarvoor") %>% 
  filter(diploma_een_drie != "binnen 2- en 3de jaar") %>% 
  filter(uitval_een_drie != "binnen 1 jaar") %>% 
  filter(uitval_een_drie != "binnen 2- en 3de jaar")


##### Bestand aanmaken voor studiewissel binnen 1 jaar##########

# Selecteer eerste en tweede jaar uit basisbestand
Basisbestand_filter = Basisbestand1CHO %>% 
  
  # Alleen studenten uit het instroombestand bekijken die niet binnen 1 jaar hun diploma hebben behaald of zijn uitgevallen binnen 1 jaar
  filter(persoonsgebonden_nummer %in% zittend_1jr$persoonsgebonden_nummer) %>% 
  filter(verblijfsjaar_actuele_instelling %in% c(1,2) & 
           soort_inschrijving_actuele_instelling_label == "hoofdinschrijving binnen het domein actuele instelling")


# Van instroom weten we dat voorgaande selecties leiden tot 1 regel per student voor verblijfsjaar 1. Verblijfsjaar 2 zou ook 1 regel per student moeten zijn.
# Check: Aantal regels per student mag dus niet meer dan 2 zijn, anders foutmelding.

Freq_tabel_persnr = Basisbestand_filter %>% 
  count(persoonsgebonden_nummer) 

if(any(Freq_tabel_persnr$n>2)){
  stop("Meer dan twee regels per student in het switchcohortbestand gevonden!")
}



# Van studenten met maar 1 regel in het switchbestand is geen switch te bepalen, dus alleen studenten met 2 regels selecteren.
Persnr_met_2verblijfsjaren = Freq_tabel_persnr %>% 
  filter(n==2)




# In het bewerkte basisbestand alleen de studenten selecteren met 2 regels.

message("\n\nSTUDIEWISSEL --- Switch binnen 1 jaar")

Studiewissel_1jr_dubbeleregels = Basisbestand_filter %>% 
  filter(persoonsgebonden_nummer %in% Persnr_met_2verblijfsjaren$persoonsgebonden_nummer) 

# Check om te bepalen of aantal verblijfsjaar = 1 gelijk is aan aantal verblijfsjaar = 2
if(sum(Studiewissel_1jr_dubbeleregels$verblijfsjaar_actuele_instelling == 1) != sum(Studiewissel_1jr_dubbeleregels$verblijfsjaar_actuele_instelling == 2)){
  stop("Aantal verblijfsjaren 1 en 2 is niet gelijk")
}


Studiewissel_1jr_instelling = Studiewissel_1jr_dubbeleregels %>%    
  #Eerst sorteren op persnr en verblijfsjaar om eventuele wissel te kunnen bepalen.
  arrange(persoonsgebonden_nummer, verblijfsjaar_actuele_instelling) %>% 
  
  #variabele aanmaken voor studiewissel binnen 1 jaar
  # variabele aanmaken voor opleiding (code en naam) na switch
  # variabele aanmaken voor opleidingsvorm na switch
  # variabele aanmaken voor opleidingsniveau na switch
  mutate(persnr_dubbel = ifelse(persoonsgebonden_nummer==lag(persoonsgebonden_nummer),"zelfde student", "andere student"),
         opl_dubbel = ifelse(opleiding_actueel_equivalent==lag(opleiding_actueel_equivalent),"zelfde opleiding", "andere opleiding"),
         studiewissel_1jr = ifelse(verblijfsjaar_actuele_instelling == 2 & persnr_dubbel == "zelfde student" & opl_dubbel == "andere opleiding", "Gewisseld binnen 1 jaar", "Niet gewisseld binnen 1 jaar"),
         verschil_kalenderjaren = inschrijvingsjaar - lag(inschrijvingsjaar),
         opleidingscode_na_switch1jr = ifelse(studiewissel_1jr == "Gewisseld binnen 1 jaar", 
                                              as.character(opleiding_actueel_equivalent), NA),
         # opleidingsnaam_na_switch1jr = ifelse(studiewissel_1jr == "Gewisseld binnen 1 jaar", 
         #                                      as.character(opleiding_actueel_equivalent), NA),
         opleidingsvorm_na_switch1jr = ifelse(studiewissel_1jr == "Gewisseld binnen 1 jaar", 
                                              as.character(opleidingsvorm), NA),
         opleidingsniveau_na_switch1jr = ifelse(studiewissel_1jr == "Gewisseld binnen 1 jaar", 
                                                as.character(type_hoger_onderwijs_binnen_soort_hoger_onderwijs), NA),
         HBOsector_na_switch1jr = ifelse(studiewissel_1jr == "Gewisseld binnen 1 jaar", as.character(croho_onderdeel_actuele_opleiding), NA),
         # opleidingsfase_na_switch1jr = ifelse(studiewissel_1jr == "Gewisseld binnen 1 jaar", as.character(opleidingsfase), NA)
  ) %>% 
  
  # Omzetten naar factor
  mutate(studiewissel_1jr = factor(studiewissel_1jr),
         opleidingscode_na_switch1jr = factor(opleidingscode_na_switch1jr),
         # opleidingsnaam_na_switch1jr = factor(opleidingsnaam_na_switch1jr),
         opleidingsvorm_na_switch1jr = factor(opleidingsvorm_na_switch1jr),
         opleidingsniveau_na_switch1jr = factor(opleidingsniveau_na_switch1jr),
         HBOsector_na_switch1jr = factor(HBOsector_na_switch1jr)
  ) %>% 
  
  select(persoonsgebonden_nummer,
         verschil_kalenderjaren,
         studiewissel_1jr, 
         opleidingscode_na_switch1jr, 
         # opleidingsnaam_na_switch1jr, 
         opleidingsvorm_na_switch1jr, 
         opleidingsniveau_na_switch1jr,
         HBOsector_na_switch1jr) %>% 
  
  # alleen switch bepalen voor aaneensluitende kalenderjaren, dus switch na studieonderbreking niet meenemen 
  filter(studiewissel_1jr == "Gewisseld binnen 1 jaar", verschil_kalenderjaren == 1)



##### Bestand aanmaken voor studiewissel binnen 3 jaar##########


Basisbestand1CHO_bewerkt = Basisbestand1CHO %>% 
  
  # #Variabele Verblijfsjaar aanmaken om instromers te kunnen selecteren
  # mutate(verblijfsjaar = ifelse(indicatieactiefoppeildatum == "actief op peildatum 1 oktober (inschrijving)", 
  #                               inschrijvingsjaar - eerstejaardezeactinstelling + 1,
  #                               NA)) %>% 
  
  #Om interne switch na 3 jaar te kunnen bepalen verblijfsjaar 1 en 4 selecteren (studieonderbrekers die na onderbreking switchen, 
  #worden niet meegeteld op deze manier, evenzo wisselaars die vaker switchen dan 1 keer) en hoofdinschrijving-> om een bestand te krijgen met 1 regel per student per verblijfsjaar.
  
  # Alleen studenten uit het instroombestand bekijken die niet binnen 3 jaar hun diploma hebben behaald
  filter(persoonsgebonden_nummer %in% zittend_3jr$persoonsgebonden_nummer) %>% 
  
  filter(verblijfsjaar_actuele_instelling %in% c(1,4) & soort_inschrijving_actuele_instelling_label == "hoofdinschrijving binnen het domein actuele instelling")

# Van instroom weten we dat voorgaande selecties leiden tot 1 regel per student voor verblijfsjaar 1. Verblijfsjaar 4 zou ook 1 regel per student moeten zijn.
# Check: Aantal regels per student mag dus niet meer dan 2 zijn, anders foutmelding.

Freq_tabel_persnr = Basisbestand1CHO_bewerkt %>% 
  count(persoonsgebonden_nummer) 

if(any(Freq_tabel_persnr$n>2)){
  stop("Meer dan twee regels per student in het switchcohortbestand gevonden!")
}

# Van studenten met maar 1 regel in het switchbestand is geen switch te bepalen, dus alleen studenten met 2 regels selecteren.
Persnr_met_2verblijfsjaren = Freq_tabel_persnr %>% 
  filter(n==2)


# In het bewerkte basisbestand alleen de studenten selecteren met 2 regels.
message("STUDIEWISSEL --- Switch binnen 3 jaar")

Studiewissel_3jr_dubbeleregels = Basisbestand1CHO_bewerkt %>% 
  
  filter(persoonsgebonden_nummer %in% Persnr_met_2verblijfsjaren$persoonsgebonden_nummer) 

# Check om te bepalen of aantal verblijfsjaar = 1 gelijk is aan aantal verblijfsjaar = 4
if(sum(Studiewissel_3jr_dubbeleregels$verblijfsjaar_actuele_instelling == 1) != sum(Studiewissel_3jr_dubbeleregels$verblijfsjaar_actuele_instelling == 4)){
  stop("Aantal verblijfsjaren 1 en 4 is niet gelijk")
}


Studiewissel_3jr_instelling = Studiewissel_3jr_dubbeleregels %>%    
  #Eerst sorteren op persnr en verblijfsjaar om eventuele wissel te kunnen bepalen.
  arrange(persoonsgebonden_nummer, verblijfsjaar_actuele_instelling) %>% 
  
  #variabele aanmaken voor studiewissel binnen 3 jaar
  # variabele aanmaken voor opleiding (code en naam) na switch
  # variabele aanmaken voor opleidingsvorm na switch
  # variabele aanmaken voor opleidingsniveau na switch
  mutate(persnr_dubbel = ifelse(persoonsgebonden_nummer==lag(persoonsgebonden_nummer), "zelfde student", "andere student"),
         opl_dubbel = ifelse(opleiding_actueel_equivalent==lag(opleiding_actueel_equivalent), "zelfde opleiding", "andere opleiding"),
         studiewissel_3jr = ifelse(verblijfsjaar_actuele_instelling == 4 & persnr_dubbel == "zelfde student" & opl_dubbel == "andere opleiding","Gewisseld binnen 3 jaar", "Niet gewisseld binnen 3 jaar"),
         verschil_kalenderjaren = inschrijvingsjaar - lag(inschrijvingsjaar),
         opleidingscode_na_switch3jr = ifelse(studiewissel_3jr == "Gewisseld binnen 3 jaar", as.character(opleiding_actueel_equivalent), NA),
         # opleidingsnaam_na_switch3jr = ifelse(studiewissel_3jr == "Gewisseld binnen 3 jaar", as.character(isat), NA),
         opleidingsvorm_na_switch3jr = ifelse(studiewissel_3jr == "Gewisseld binnen 3 jaar", as.character(opleidingsvorm), NA),
         opleidingsniveau_na_switch3jr = ifelse(studiewissel_3jr == "Gewisseld binnen 3 jaar", as.character(type_hoger_onderwijs_binnen_soort_hoger_onderwijs), NA),
         HBOsector_na_switch3jr = ifelse(studiewissel_3jr == "Gewisseld binnen 3 jaar", as.character(croho_onderdeel_actuele_opleiding), NA)) %>% 
  
  # Omzetten naar factor
  mutate(studiewissel_3jr = factor(studiewissel_3jr),
         opleidingscode_na_switch3jr = factor(opleidingscode_na_switch3jr),
         # opleidingsnaam_na_switch3jr = factor(opleidingsnaam_na_switch3jr),
         opleidingsvorm_na_switch3jr = factor(opleidingsvorm_na_switch3jr),
         opleidingsniveau_na_switch3jr = factor(opleidingsniveau_na_switch3jr),
         HBOsector_na_switch3jr = factor(HBOsector_na_switch3jr)
  ) %>% 
  
  select(persoonsgebonden_nummer,
         verschil_kalenderjaren,
         studiewissel_3jr, 
         opleidingscode_na_switch3jr, 
         # opleidingsnaam_na_switch3jr, 
         opleidingsvorm_na_switch3jr, 
         opleidingsniveau_na_switch3jr,
         HBOsector_na_switch3jr) %>% 
  filter(studiewissel_3jr == "Gewisseld binnen 3 jaar", verschil_kalenderjaren == 3)

# # De twee switchbestanden aan het instroombestand koppelen
# if(!"Cohorten_InstroomAvans" %in% ls()){
#   Cohorten_InstroomAvans <- readRDS(file = paste0("../", jaar, "/Instroom_cohorten_", jaar, ".RDS"))
# }

Studiewissel_indicatoren = Cohorten_Instroom %>% 
  left_join(Studiewissel_1jr_instelling, by="persoonsgebonden_nummer") %>% 
  left_join(Studiewissel_3jr_instelling, by="persoonsgebonden_nummer") %>%
  
  mutate(studiewissel_1jr = case_when(!(persoonsgebonden_nummer %in% zittend_1jr$persoonsgebonden_nummer) ~ "Geen switch bepaald",
                                     TRUE ~ studiewissel_1jr),
         studiewissel_3jr = case_when(!(persoonsgebonden_nummer %in% zittend_3jr$persoonsgebonden_nummer) ~ "Geen switch bepaald",
                                     TRUE ~ studiewissel_3jr)) %>%
  
  # Labels voor niet-wisselaars toevoegen
  mutate(studiewissel_1jr = fct_na_value_to_level(studiewissel_1jr, "Niet gewisseld binnen 1 jaar"),
         studiewissel_3jr = fct_na_value_to_level(studiewissel_3jr, "Niet gewisseld binnen 3 jaar")) %>% 
  
  select(persoonsgebonden_nummer, starts_with("studiewissel"), contains("switch"))




# Switchbestand opslaan
message("STUDIEWISSEL --- Switchbestand wordt opgeslagen")
saveRDS(Studiewissel_indicatoren, file = paste0("Output/", jaar, "/Studiewissel_", jaar, ".RDS"))




