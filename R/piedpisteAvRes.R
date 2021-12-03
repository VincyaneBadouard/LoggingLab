#
# TreefallOrientation <- as.numeric(sample(c(0.1: 179.9), size = 1)) # no particular angle to orientate to the trail, only to orientate the tree foot towards the trail
# OppAng <- 180-(90 + CrownTreefallOrientation) # the angle between the closest position to the trail (90°) and the desired position (desired angle to the trail)
#
# ## Right-hand trail
# if(TrailPt[1] >= Foot[1]){ # x trail > x foot
#   # Foot oriented
#   Aangle <- round(as.numeric(180 + OppAng + theta), digits = 0)
#   Bangle <- round(as.numeric(180 - OppAng + theta), digits = 0)
#
#   ## Left-hand trail
# }else if(TrailPt[1] < Foot[1]){ # x trail < x foot
#   # Foot oriented
#   Aangle <- round(as.numeric(360 - OppAng + theta), digits = 0)
#   Bangle <- round(as.numeric(theta + OppAng), digits = 0)
# }
#
#
#   # Calculate the two possible crown configurations
#   ACrown <- rotatepolygon(Crown, angle = Aangle, fixed = Foot) # turned crown
#   BCrown <- rotatepolygon(Crown, angle = Bangle, fixed = Foot) # turned crown
#
#   # Test the best to pull the tree back to the main trail (farthest crown from the main trail)
#   ADist <- st_distance(ACrown, MainTrail)[1,1] #matrix to value
#   BDist <- st_distance(BCrown, MainTrail)[1,1]
#
#   if(max(ADist, BDist) == ADist){
#
#     FallenTree <- AFallenTree <- st_difference(st_union( # A configuration
#       rotatepolygon(Trunk, angle = Aangle, fixed = Foot), # turned trunk
#       ACrown # turned crown
#     ))
#     BFallenTree <- NULL
#
#   }else{
#
#     FallenTree <-  BFallenTree <- st_difference(st_union( # B configuration
#       rotatepolygon(Trunk, angle = Bangle, fixed = Foot), # turned trunk
#       BCrown # turned crown
#     ))
#     AFallenTree <- st_sfc(st_point(c(0,0))) # "null" sfc object to compare after
#
#   }
#
#
#   # Check intersection with future/reserve trees
#   FRintersect <- sf::st_intersects(FallenTree, FutureReserveCrowns)
#
#   if(lengths(FRintersect) > 0) { # if there is an intersection
#
#     FallenTree <- BFallenTree <- st_difference(st_union( # B configuration (]0;180[)
#       rotatepolygon(Trunk, angle = CrownBangle, fixed = Foot), # turned trunk
#       BCrown # turned crown
#     ))
#
#     # check intersection for this new configuration
#     FRintersect <- sf::st_intersects(BFallenTree, FutureReserveCrowns)
#     if(lengths(FRintersect) > 0) { # if there is an intersection. If not FallenTree stay BFallenTree
#       FallenTree <- AFallenTree # we prefer 1st configuration (the best for winching)
#     }
#   }
