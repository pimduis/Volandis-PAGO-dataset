 #Comment; Laden van de data-------------------------------------------------
+ library(readxl)
+ new_data <- read_excel("C:\\Users\\Duis0001\\Desktop\\PAGO\\PAGO.xlsx")

-----------------------------------------------------------------------------
#Comment; Rename variables:----------------------------------
names(new_data)[1] = 'PAGO-ID'
names(new_data)[2] = 'ID'
names(new_data)[3] = 'Date'
names(new_data)[4] = 'Company'
names(new_data)[5] = 'Sex'
names(new_data)[6] = 'Birthdate' 
names(new_data)[7] = 'Education'
names(new_data)[8] = 'AO-indicator'
names(new_data)[9] = 'CAO'
names(new_data)[10] = 'Occupation'
names(new_data)[15] = 'Werkplaats'
names(new_data)[16] = 'Jaren in Bedrijfstak'
names(new_data)[17] = 'Jaren in Functie'
names(new_data)[18] = 'Jaren in huige bedrijf'

-------------------------------------------------------------------------------
#Comment: Gefilterde dataset zonder kolommen die er niet toe doen-------------------
columns_to_exclude <- c("PAGO-ID", "A007a", "A007b", "A007c", "A007d", "G022", 
                        "G026", "G028", "G047", "G047b", "W049", "W056", "W057", 
                        "W058", "W068", "W070", "W073")
filtered_data <- new_data[, !names(new_data) %in% columns_to_exclude]  

---------------------------------------------------------------------------------
  
#Comment: Aantal mannen/vrouwen---------------------------------------------------
sex_percentages <- prop.table(sex) * 100
print(sex_percentages)
  
#Comment; Aantal keren een 'ID' voorkomt in de database:-------------------------
Number_ID <- table(new_data$ID)
Freq_Number_ID <- table(Number_ID)
View(Freq_Number_ID)

---------------------------------------------------------------------------------

#Comment; Filtering van unieke IDs (verwijdering van de dubbele, driedubbele enz.)-----
unique_ID <- new_data[!duplicated(new_data$ID),]
View(unique_ID)
length(unique_ID$ID)

---------------------------------------------------------------------------------
#Comment; Identificeren van mensen die vaker bemeten zijn:
Freq_ID>2 <- names(Number_ID[Number_ID > 2])
Freq_ID>3 <-names(Number_ID[Number_ID > 3])
Freq_ID>4 <-names(Number_ID[Number_ID > 4])  
-------------------------------------------------------------------------------

#Comment; Aantal verschillende bedrijven---------------------------------------- 
Unique_companies <- unique(new_data$Company)
Num_unique_companies <- length(Unique_companies)
print(Num_unique_companies)

-------------------------------------------------------------------------------
#Comment; Aantal keren een uniek persoon met 'Occupation' voorkomt in de database:-----------------
unique_ID[["Occupation"]][is.na(unique_ID[["Occupation"]])] <- "Missing"
Occ_freq <- factor(unique_ID$Occupation, levels = c(2,65,247,824,853,893,2110,2140,3313,5329,7021,7022,7023,7025,7113,7134,8209,8331,8457,8552,8712,8713,8722,8724,8743,9311,9312,9313,9397,9510,9511,9512,9513,9514,9515,9516,9517,9518,9520,9521,9522,9523,9524,9525,9526,9528,9529,9530,9531,9536,9537,9541,9544,9546,9547,9548,9549,9550,9551,9552,9553,9554,9555,9561,9562,9574,9583,9584,9590,9591,9593,9594,9595,9596,9598,9599,9714,9741,9742,9746,9747,9855,9903,9906,9910,9911,9912,9913,9914,9915,9916,9917,9918,9919,9920,9921,9922,9999,"Missing"), labels = c("Miscoded", "Miscoded", "Miscoded", "Miscoded", "Miscoded", "Miscoded", "Directeur", "Stafpersoneel", "Administratief", "Huishoud", "UitvoerderBU", "UitvoerderGWW", "UitvoerderAfbouw", "Werfbaas", "Bronbemaler", "Sondeerder", "Natuursteenbewerker", "Stelleur", "Onderhoudsmonteur", "Elektriciën", "Loodgieter", "CV-monteur", "Metaalbewerker", "Lasser", "Gevelmonteur", "Schilder", "Schilder", "Schilder", "Wegmarkeerder", "Metselaar_renov", "Metselaar_nieuw", "Ovenbouwer","Tegelzetter", "Straatmaker", "Steenzetter", "Voeger", "Blokkensteller_afbouw", "Blokkensteller_ruwbouw", "Sleuvenfrezer", "Betonboorder", "Betontimmmerman", "Betonstaalvlechter", "Betonreparateur", "Terrazzowerker", "Betonstorter", "Koppensneller", "Vloerenlegger", "Monteur_prefabbeton", "Dakdekker_Riet", "Dakdekker_Pan", "Dakdekker_Leisteen", "Timmerman_Renov", "Houtbewerker", "Timmerman_Nieuwb", "Timmerman_Metselaar","Uitzetter", "Modellenmaker_Prefabbeton", "Spackspuiter", "Stukadoor", "Vloerenlegger_Giet", "Sleuvenfrezer", "Vloerenlegger_Epox", "Rioolreparateur", "Isoleerder", "Kitter","Kassenbouwer", "Kozijnmonteur", "Maatvoerder", "Spanmonteur", "Rioleerder", "Steigerbouwer", "Sloper", "Kabel-Buizenlegger", "Asbestsaneerder", "Plafond-Wandmonteur", "Rijswerker", "Magazijnwerker", "Machinist-MK", "Machinist-TK", "Machinist-GWW", "Betonmolencentralewerker", "Chauffeur", "Bodemsaneerder", "Cultuurtechnisch_Mw", "Opperman_straat", "Spoorlegger", "Opperman_metsel", "Grondwerker", "Vakman-GWW", "Funderingswerker", "Baggeraar", "Landmeter", "Asfaltwerker", "Bouwvakhelper", "Opperman_Vloer", "Machinist-Fundering-Gr", "Machinist-Fundering-kl","Miscoded", "Missing"))
table(Occ_freq)
Table_Occ <- table(Occ_freq)
View(Table_Occ)

--------------------------------------------------------------------------------
#Comment; Aantal personen per 'Werkplaats'----------------------------- 
Table_Workplace <- table(new_data$A007_Werkplaats)
View(Table_Workplace)

--------------------------------------------------------------------------------
#Comment; Freqency of 'Education'------------------------------------------------
Edu_freq <- factor(unique_ID$Education, levels = c(0,1,2,3,4,5,6,7), labels = c("Onbekend", "Geen", "Basisonderwijs", "VMBO/MAVO", "HAVO/VWO/MBO", "HBO/Bachelor", "PostHBO/Master", "WO"))
Table_Edu <- table(Edu_freq)

#Comment; Age--------------------------------------------------------------------- 
library(lubridate)
Unique_age <- floor(interval(unique_ID$Birthdate, Sys.Date()) / years(1))
print(Unique_age)

Age <- floor(interval(new_data$Birthdate, Sys.Date()) / years(1))
print(Age)

-----------------------------------------------------------------------------------
#Comment; Years in function, job, industry---------------------------------------
years <- cbind(new_data$A008_Jaren_in_huidige_bedrijfstak, new_data$A009_Jaren_in_huidig_functie)
colnames(years) <- c("jaren_in_bedrijfstak", "jaren_in_functie")

#Comment; Table Years in function, job, industry, age----------------------------
library(summarytools)

YII <- new_data$`Jaren in Bedrijfstak`
YIF <- new_data$`Jaren in Functie`
YIC <- new_data$`Jaren in huige bedrijf`
summary_table_years <- descr(cbind(Age,YIC,YIF,YII))
colnames(summary_table_years) <- c("Age", "years in company", "years in function", "years in industry")

#Zelfgerapporteerde gezondheid----------------------------------------------------
Table_health <-  table(new_data$G015_Algemene_gezondheid)
health_percentages <- prop.table(Table_health) * 100
print(health_percentages)

freq(unique_ID$G015_Algemene_gezondheid)

#Exposure------------------------------------------------------------------------
Table_dust <- table(new_data$W056_Blootstelling_aan_stof)
Dust_percent <- prop.table(Table_dust) * 100
print(Dust_percent)



#Maatregelen-----------------------------------------------------------------------
freq_table_measures <- new_data %>% 
count(W068_Werk_handschoenen, W070_Adembescherming, W073_Technische_organisatorische_maatregelen)
freq(freq_table_measures)

---------------------------------------------------------------------------------
  
#Comment; Roken------------------------------------------------------------------  
freq((new_data$G047_Roken))

#Wanneer roken: aantal sigaretten--------------------------------------------------
Aantal_Sigaret <- na.omit(new_data$G047a)
descr(Aantal_Sigaret)
Rokers <- 
#Comment; to do---------------------------------    
! Tabellen maken en exporteren naar excel
! Reinigen database:
- Schilders verwijderen (9311, 9312, 9313)
- Checken op onzinnige codes --> verwijderen
! Kolommen verwijderen die er niet toe doen 
