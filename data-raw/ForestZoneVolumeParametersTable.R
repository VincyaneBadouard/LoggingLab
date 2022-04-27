# code to prepare "ForestZoneVolumeParametersTable" from "VolumeParametersTable":
# = Add the forests linked to these zones in French Guiana.

data("VolumeParametersTable") # The volume parameters data in the global environment

ForestZoneVolumeParametersTable <- data.frame(
  Forest = c("Acarouany","BAFOG","Kaw","Laussat","Montagne_Plomb",
             "Montagne_Tortue","Nouragues","Organabo","Paracou",
             "Regina_St_Georges","Risquetout","Tibourou", "Tresor", "Trinite"), # Guyafor forests
  Zone = NA) %>% # Volume formules zones

  mutate(Zone = ifelse(Forest == "Acarouany"| Forest =="BAFOG" |Forest =="Laussat", "West", NA)) %>% # West zone
  mutate(Zone = ifelse(Forest == "Montagne_Plomb"| Forest =="Organabo"|
                         Forest =="Paracou"|Forest =="Risquetout"|Forest =="Trinite", "Central", Zone)) %>% # Central zone
  mutate(Zone = ifelse(Forest == "Kaw"| Forest =="Montagne_Tortue"|Forest =="Nouragues"|
                         Forest =="Regina_St_Georges"|Forest =="Tibourou"|Forest =="Tresor", "East", Zone)) %>% # East zone
  left_join(VolumeParametersTable,  by = "Zone")


usethis::use_data(ForestZoneVolumeParametersTable, overwrite = TRUE)
