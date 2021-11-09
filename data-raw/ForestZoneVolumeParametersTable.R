# code to prepare "ForestZoneVolumeParametersTable" from "VolumeParametersTable":
# = Add the forests linked to these zones in French Guiana.

data("VolumeParametersTable") # The volume parameters data in the global environment

ForestZoneVolumeParametersTable <- data.frame (Forest = c("Acarouany","BAFOG","Kaw","Laussat","Montagne Plomb","Montagne Tortue","Nouragues","Organabo","Paracou","Régina St Georges","Risquetout","Tibourou"), # Guyafor forests
                                               Zone = c("East", "West", "Central", "FrenchGuiana")) %>% # Volume formules zones
  mutate(Zone = ifelse(Forest == "Acarouany"| Forest =="BAFOG" |Forest =="Laussat", "West", NA)) %>% # West zone
  mutate(Zone = ifelse(Forest == "Montagne Plomb"| Forest =="Organabo"|Forest =="Paracou"|Forest =="Risquetout", "Central", Zone)) %>% # Central zone
  mutate(Zone = ifelse(Forest == "Kaw"| Forest =="Montagne Tortue"|Forest =="Nouragues"|Forest =="Régina St Georges"|Forest =="Tibourou", "East", Zone)) %>% # East zone
  left_join(VolumeParametersTable)
