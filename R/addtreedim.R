#' Compute tree dimensions (Height, diameter, harvestable volume, including crown)
#'
#' @param inventory (data.frame)
#' @param crowndiameterparameters (data.frame)
#' @param volumeparameters (data.frame)
#' @param otherloggingparameters (list)
#'
#' @return inventory (data.frame) with additional variables (TreeHeight, TreeHarvestableVolume, TrunkHeight, CrownHeight, CrownDiameter)
#' @export
#' @import dplyr
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate

#'
#' @examples
#' data(Paracou6_2016)
#' data(ForestZoneVolumeParametersTable) # The volume parameters data in the global environment
#' load(system.file("extdata", "ParamCrownDiameterAllometry.rda", package = "Maria"))
#'
#' if (!("DBH" %in% names(Paracou6_2016))) {add_column(Paracou6_2016, DBH = NA) #if DBH doesn't exist create it
#' Paracou6_2016$DBH = Paracou6_2016$CircCorr/pi} # and compute it
#'
#' addtreedim(inventory = Paracou6_2016)
addtreedim <- function(
  inventory,
  crowndiameterparameters = ParamCrownDiameterAllometry,
  volumeparameters = ForestZoneVolumeParametersTable,
  otherloggingparameters = loggingparameters()

){
  # Arguments check
  if(!any(lapply(list(inventory, crowndiameterparameters, volumeparameters), inherits, "data.frame")))
    stop("The function arguments must be data.frames")

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


  # Variables computation:

  inventory <- inventory %>%

    # TreeHarvestableVolume (m3)
    left_join(volumeparameters) %>%
    mutate(TreeHarvestableVolume = otherloggingparameters$TreeHarvestableVolumeAllometry(aCoef, bCoef, DBH)) %>%

    # TrunkHeight (m)
    mutate(TrunkHeight = otherloggingparameters$TrunkHeightAllometry(DBH, TreeHarvestableVolume)) %>%

    # TreeHeight (m)
    mutate(TreeHeight = otherloggingparameters$TreeHeightAllometry(DBH)) %>%


    # CrownHeight (m)
    mutate(CrownHeight = TreeHeight - TrunkHeight)


    # # CrownDiameter (m)
    # # Add crown diameter allometry parameters at the inventory:
    # unite(Genus, Species, col = "ScientificName", sep = "_", remove = F) %>% # create ScientificName column in the inventory
    # left_join(spParamCrownDiameter, by = c("ScientificName","Genus","Species")) %>% # at species scale
    # left_join(genParamCrownDiameter, by = "Genus", suffix = c(".species", ".genus")) %>% # at genus scale
    # left_join(famParamCrownDiameter, by = "Family") %>% # at family scale
    # dplyr::rename(Taxo.family = Taxo, alpha.family = alpha, beta.family = beta) %>%
    # # manage the different scales parameters Species > Genus > Family:
    # # alpha:
    # mutate(alpha = alpha.species, beta = beta.species, Taxo = Taxo.species) %>% # create the future & unique alpha,beta,taxo columns
    # mutate(alpha = ifelse(is.na(alpha), alpha.genus, alpha)) %>% #if species parameters are absent, take genus parameters
    # mutate(alpha = ifelse(is.na(alpha), alpha.family, alpha)) %>% #if species&genus parameters are absent, take family parameters
    # mutate(alpha = ifelse(is.na(alpha), mean(alpha, na.rm =T), alpha)) %>% #if species,genus&family parameters are absent, take parameters mean
    # select(-alpha.species, -alpha.genus, -alpha.family) %>% #remove obsolete columns
    # # beta:
    # mutate(beta = ifelse(is.na(beta), beta.genus, beta)) %>% #if species parameters are absent, take genus parameters
    # mutate(beta = ifelse(is.na(beta), beta.family, beta)) %>% #if species&genus parameters are absent, take family parameters
    # mutate(beta = ifelse(is.na(beta), mean(beta, na.rm =T), beta)) %>% #if species,genus&family parameters are absent, take parameters mean
    # select(-beta.species, -beta.genus, -beta.family) %>% #remove obsolete columns
    # # scale:
    # mutate(Taxo = ifelse(is.na(Taxo), Taxo.genus, Taxo)) %>% #if species parameters are absent, take genus parameters
    # mutate(Taxo = ifelse(is.na(Taxo), Taxo.family, Taxo)) %>% #if species&genus parameters are absent, take family parameters
    # mutate(Taxo = ifelse(is.na(Taxo), "mean", Taxo)) %>% #if species,genus&family parameters are absent, take parameters mean
    # select(-Taxo.species, -Taxo.genus, -Taxo.family) %>% #remove obsolete columns
    # # compute the crown diameter
    # mutate(CrownDiameter = otherloggingparameters$CrownDiameterAllometry(DBH, TreeHeight, alpha, beta))


  return(inventory)
}

#TreeHeight
#TreeHarvestableVolume
#TrunkHeight
#CrownHeight
#CrownDiameter

