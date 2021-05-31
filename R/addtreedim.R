#' Compute tree dimensions (Height, diameter, harvestable volume, including crown)
#'
#' @param inventory (data.frame)
#' @param crowndiameterparameters (data.frame)
#' @param volumeparameters (data.frame)
#'
#' @return inventory (data.frame) with additional variables (TreeHeight, TreeHarvestableVolume, TrunkHeight, CrownHeight, CrownDiameter)
#' @export
#'
#' @examples
#' data(Paracou6_2016)
#' data("ForestZoneVolumeParametersTable") # The volume parameters data in the global environment
#' load(system.file("extdata", "ParamCrownDiameterAllometry.rda", package = "Maria"))
#'
#' if (!("DBH" %in% names(Paracou6_2016))) {add_column(Paracou6_2016, DBH = NA) #if DBH doesn't exist create it
#' Paracou6_2016$DBH = Paracou6_2016$CircCorr/pi} # and compute it
#'
#' addtreedim(inventory = Paracou6_2016)
addtreedim <- function(
  inventory,
  crowndiameterparameters = ParamCrownDiameterAllometry,
  volumeparameters = ForestZoneVolumeParametersTable

){
  # Crown diameter allometry parameters data preparation:

spParamCrownDiameter <- crowndiameterparameters %>% #parameters at species scale
  filter(Taxo == "sp") %>%
  select(-Family)
genParamCrownDiameter <- crowndiameterparameters %>% #parameters at genus scale
  filter(Taxo == "gen") %>%
  select(-Family, -ScientificName, -Species)
famParamCrownDiameter <- crowndiameterparameters %>% #parameters at family scale
  filter(Taxo == "fam") %>%
  select(-Genus, -ScientificName, -Species)

# Applicate the model (where stocked?) to retrieve trees heights YOUHOOOOOOOOOOOOOOOOOOOOOOOOOOOO
# TreeHeightEstimation <- retrieveH(
#   D = inventory$DBH,
#   model = ...
# )

# Variables computation:

inventory <- inventory %>%

  # TreeHarvestableVolume (m3)
  left_join(volumeparameters) %>%
  mutate(TreeHarvestableVolume = aCoef + bCoef * (DBH/100)^2) %>%  # Volume formula = a + b*DBH2 (a and b depend on the forest location)(DBH in cm, in m in the formula)

  # TrunkHeight (m)
  mutate(TrunkHeight = TreeHarvestableVolume/(pi*(((DBH/100)/2)^2))) %>%  # compute the trunk height (CylinderVolume= π(DBH/2)² x H) (DBH in cm, in m in the formula)

  # TreeHeight (m) EN ATTENTE
  # mutate(TreeHeight = TreeHeightEstimation$H) %>% #' @importFrom BIOMASS retrieveH #' @importFrom BIOMASS modelHD


  # CrownHeight (m)
  # mutate(CrownHeight = TreeHeight - TrunkHeight) %>%  # compute the crown height TREEHEIGHT EN ATTENTE


  # CrownDiameter (m)
  # Add crown diameter allometry parameters at the inventory:
  unite(Genus, Species, col = "ScientificName", sep = "_", remove = F) %>% # create ScientificName column in the inventory
  left_join(spParamCrownDiameter, by = c("ScientificName","Genus","Species")) %>% # at species scale
  left_join(genParamCrownDiameter, by = "Genus", suffix = c(".species", ".genus")) %>% # at genus scale
  left_join(famParamCrownDiameter, by = "Family") %>% # at family scale
  rename(Taxo.family = Taxo, alpha.family = alpha, beta.family = beta) %>%
  # manage the different scales parameters Species > Genus > Family:
  # alpha:
  mutate(alpha = alpha.species, beta = beta.species, Taxo = Taxo.species) %>% # create the future & unique alpha,beta,taxo columns
  mutate(alpha = ifelse(is.na(alpha), alpha.genus, alpha)) %>% #if species parameters are absent, take genus parameters
  mutate(alpha = ifelse(is.na(alpha), alpha.family, alpha)) %>% #if species&genus parameters are absent, take family parameters
  mutate(alpha = ifelse(is.na(alpha), mean(alpha, na.rm =T), alpha)) %>% #if species,genus&family parameters are absent, take parameters mean
  select(-alpha.species, -alpha.genus, -alpha.family) %>% #remove obsolete columns
  # beta:
  mutate(beta = ifelse(is.na(beta), beta.genus, beta)) %>% #if species parameters are absent, take genus parameters
  mutate(beta = ifelse(is.na(beta), beta.family, beta)) %>% #if species&genus parameters are absent, take family parameters
  mutate(beta = ifelse(is.na(beta), mean(beta, na.rm =T), beta)) %>% #if species,genus&family parameters are absent, take parameters mean
  select(-beta.species, -beta.genus, -beta.family) %>% #remove obsolete columns
  # scale:
  mutate(Taxo = ifelse(is.na(Taxo), Taxo.genus, Taxo)) %>% #if species parameters are absent, take genus parameters
  mutate(Taxo = ifelse(is.na(Taxo), Taxo.family, Taxo)) %>% #if species&genus parameters are absent, take family parameters
  mutate(Taxo = ifelse(is.na(Taxo), "mean", Taxo)) %>% #if species,genus&family parameters are absent, take parameters mean
  select(-Taxo.species, -Taxo.genus, -Taxo.family) #remove obsolete columns
  # compute the crown diameter
# mutate(CrownDiameter = exp((
  # (log(DBH)-alpha-error)/beta)
  # )/TreeHeight) # Crown Diameter (CD) (ln(D) = 𝜶+ 𝜷 ln(H*CD) + 𝜺, with 𝜺~N(0,σ^2) and meanσ^2 = 0.0295966977. (Mélaine's allometries))
  # Mettre finalement l'objet créé dans le zzz.R


  return(inventory)
}

#TreeHeight
#TreeHarvestableVolume
#TrunkHeight
#CrownHeight
#CrownDiameter

