#' .ONFGuyafortaxojoin
#'
#' @description Joins a commercial criteria table to a forest inventory
#' @param inventory (data.frame)
#' @param speciescriteria mettre le lien vers le doc de la table (data.frame)
#'
#' @return The forest inventory with economic informations (economic name, rank, mimimum/maximum falling diameter, upped mimimum falling diameter)
#' per individuals (data.frame)
#' @export
#'
#' @examples
#' .ONFGuyafortaxojoin(Paracou6_2016, SpeciesCriteria)
#'
.ONFGuyafortaxojoin <- function(
  inventory,
  speciescriteria = SpeciesCriteria
)
{
  genuscriteria <- speciescriteria %>% # for economic names that relate to entire genus
    filter(Species == "spp")

  speciescriteria <- speciescriteria %>%
    filter(Species != "spp")

  inventoryA <- inventory %>%
    left_join(genuscriteria, by = "Genus", suffix = c("", ".genus")) %>%
    select(-Species.genus) %>%
    left_join(speciescriteria, by = c("Genus","Species"), suffix = c(".genus", ".species")) %>%
    dplyr::rename(VernName = VernName.genus) %>%
    mutate(ONFName = ifelse(is.na(VernName.species), VernName.genus.genus, VernName.species)) %>%
    select(-VernName.species, -VernName.genus.genus) %>%
    mutate(Commercial = ifelse(is.na(Commercial.species), Commercial.genus, Commercial.species)) %>%
    mutate(MinFD = ifelse(is.na(MinFD.species), MinFD.genus, MinFD.species)) %>%
    mutate(UpMinFD = ifelse(is.na(UpMinFD.species), UpMinFD.genus, UpMinFD.species)) %>%
    mutate(MaxFD = ifelse(is.na(MaxFD.species), MaxFD.genus, MaxFD.species)) %>%
    select(-Commercial.species, -Commercial.genus, -MinFD.species, -MinFD.genus, -UpMinFD.species, -UpMinFD.genus, -MaxFD.species, -MaxFD.genus)

  ConcernedRows <- which(inventoryA$ONFName == "maho rouge" & (inventoryA$ScientificName == "Lecythis_poiteaui"| inventoryA$ScientificName == "Lecythis_praeclara" | inventoryA$ScientificName == "Lecythis_holcogyne" | inventoryA$ScientificName == "Lecythis_pneumatophora"| inventoryA$ScientificName == "Lecythis chartacea"| inventoryA$ScientificName == "Lecythis zabucajo"))
  if(length(ConcernedRows) > 0){
    inventoryA$ONFName[ConcernedRows] <- NA
    inventoryA$Commercial[ConcernedRows] <- NA
    inventoryA$MinFD[ConcernedRows] <- NA
    inventoryA$UpMinFD[ConcernedRows] <- NA
    inventoryA$MaxFD[ConcernedRows] <- NA
  }

  ConcernedRows <- which(inventoryA$ONFName == "balata blanc" & (inventoryA$ScientificName == "Micropholis_melinoniana"| inventoryA$ScientificName == "Micropholis_egensis" | inventoryA$ScientificName == "Micropholis_cayennensis" | inventoryA$ScientificName == "Micropholis_obscura"))
  if(length(ConcernedRows) > 0){
    inventoryA$ONFName[ConcernedRows] <- NA
    inventoryA$Commercial[ConcernedRows] <- NA
    inventoryA$MinFD[ConcernedRows] <- NA
    inventoryA$UpMinFD[ConcernedRows] <- NA
    inventoryA$MaxFD[ConcernedRows] <- NA
  }

}
