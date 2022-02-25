# code to prepare "ForestZoneVolumeParametersTable" from "VolumeParametersTable":
# = Add the forests linked to these zones in French Guiana.

data("VolumeParametersTable") # The volume parameters data in the global environment

ForestZoneVolumeParametersTable <- data.frame(
  Forest = c("Acarouany","BAFOG","Kaw","Laussat","Montagne Plomb",
             "Montagne Tortue","Nouragues","Organabo","Paracou",
             "Régina St Georges","Risquetout","Tibourou", "Trésor", "Trinité"), # Guyafor forests
  Zone = NA) %>% # Volume formules zones

  mutate(Zone = ifelse(Forest == "Acarouany"| Forest =="BAFOG" |Forest =="Laussat", "West", NA)) %>% # West zone
  mutate(Zone = ifelse(Forest == "Montagne Plomb"| Forest =="Organabo"|
                         Forest =="Paracou"|Forest =="Risquetout"|Forest =="Trinité", "Central", Zone)) %>% # Central zone
  mutate(Zone = ifelse(Forest == "Kaw"| Forest =="Montagne Tortue"|Forest =="Nouragues"|
                         Forest =="Régina St Georges"|Forest =="Tibourou"|Forest =="Trésor", "East", Zone)) %>% # East zone
  left_join(VolumeParametersTable,  by = "Zone")


# usethis::use_data(ForestZoneVolumeParametersTable, overwrite = TRUE)
