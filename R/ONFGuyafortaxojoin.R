#' ONFGuyafortaxojoin
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
#' ONFGuyafortaxojoin(Paracou6_2016, SpeciesCriteria)
#'
ONFGuyafortaxojoin <- function(
  inventory,
  speciescriteria = SpeciesCriteria
)
{

  # Arguments check

  if(!any(unlist(lapply(list(inventory, speciescriteria), inherits, "data.frame"))))
    stop("The function arguments must be data.frames") # any() don't take a list

  # Function
  genuscriteria <- speciescriteria %>% # for economic names that relate to entire genus
    filter(Species == "spp")

  speciescriteria <- speciescriteria %>%
    filter(Species != "spp")

  inventory <- inventory %>%
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
    select(-Commercial.species, -Commercial.genus, -MinFD.species, -MinFD.genus,
           -UpMinFD.species, -UpMinFD.genus, -MaxFD.species, -MaxFD.genus)

  ConcernedRows <- which(inventory$ONFName == "maho rouge" & (inventory$ScientificName == "Lecythis_poiteaui"| inventory$ScientificName == "Lecythis_praeclara" | inventory$ScientificName == "Lecythis_holcogyne" | inventory$ScientificName == "Lecythis_pneumatophora"| inventory$ScientificName == "Lecythis chartacea"| inventory$ScientificName == "Lecythis zabucajo"))
  if(length(ConcernedRows) > 0){
    inventory$ONFName[ConcernedRows] <- NA
    inventory$Commercial[ConcernedRows] <- NA
    inventory$MinFD[ConcernedRows] <- NA
    inventory$UpMinFD[ConcernedRows] <- NA
    inventory$MaxFD[ConcernedRows] <- NA
  }

  ConcernedRows <- which(inventory$ONFName == "balata blanc" & (inventory$ScientificName == "Micropholis_melinoniana"| inventory$ScientificName == "Micropholis_egensis" | inventory$ScientificName == "Micropholis_cayennensis" | inventory$ScientificName == "Micropholis_obscura"))
  if(length(ConcernedRows) > 0){
    inventory$ONFName[ConcernedRows] <- NA
    inventory$Commercial[ConcernedRows] <- NA
    inventory$MinFD[ConcernedRows] <- NA
    inventory$UpMinFD[ConcernedRows] <- NA
    inventory$MaxFD[ConcernedRows] <- NA
  }

  ConcernedRows <- which(inventory$ONFName == "kimboto" & (inventory$ScientificName == "Pradosia_cochlearia"| inventory$ScientificName == "Pradosia_huberi"))
  if(length(ConcernedRows) > 0){
    inventory$ONFName[ConcernedRows] <- NA
    inventory$Commercial[ConcernedRows] <- NA
    inventory$MinFD[ConcernedRows] <- NA
    inventory$UpMinFD[ConcernedRows] <- NA
    inventory$MaxFD[ConcernedRows] <- NA
  }

  inventory <- inventory %>%
    mutate(Commercial = ifelse(is.na(Commercial), "0", Commercial)) %>%
    mutate(Commercial = factor(as.character(Commercial)))

  return(inventory)
}
