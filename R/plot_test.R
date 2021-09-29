# ggplot() +
#   geom_sf(data = sf::st_as_sf(inventory, coords = c("Xutm", "Yutm"))) +
#   # geom_sf(data = sf::st_as_sf(Treefall, coords = c("Xutm", "Yutm")), aes(fill = "A")) +
#   # geom_sf(data = getgeometry (inventory, TreePolygon), # cuted trees
#   #         alpha = 0.5, fill = "forestgreen") +
#   geom_sf(data = sf::st_as_sf(Selected, coords = c("Xutm", "Yutm")), aes(colour = "Selected"), show.legend = "point") +
#   geom_sf(data = sf::st_as_sf(Reserve, coords = c("Xutm", "Yutm")), aes(colour = "Reserve"), show.legend = "point", size = 4) +
#   geom_sf(data = sf::st_as_sf(Future, coords = c("Xutm", "Yutm")), aes(colour = "Future"), show.legend = "point", size = 4) +
#   scale_colour_manual(values = c("Future" = "orange","Reserve" = "purple","Selected" = "red"))
#
#
#
#
# NewInventory <- sf::st_as_sf(NewInventory, coords = c("Xutm", "Yutm")) # sf object
#
# NewInventory$LoggingStatus <- factor(NewInventory$LoggingStatus,
#                                      levels = c("non-harvestable","future","reserve","harvestable","harvestableUp"))
#
# ggplot(NewInventory) +
#   geom_sf(aes(color = LoggingStatus), show.legend = "point", size = 3) +
#   # scale_color_manual(values = c("grey","orange","purple","skyblue","blue")) +
#   geom_sf(data = sf::st_as_sf(Selected, coords = c("Xutm", "Yutm")), colour = "red",
#           aes(color = "selected")) +
#   scale_color_manual(values = c("selected" = "red", "grey","orange","purple","skyblue","blue"))
#
#
