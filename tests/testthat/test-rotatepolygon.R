test_that("rotatepolygon", {

  # Test data

  NotAPol <- matrix(c(286503, 583134,
                      286503, 583240,
                      286507, 583240,
                      286507, 583134,
                      286503, 583134)
                    ,ncol=2, byrow=TRUE)
  Pol <- st_polygon(list(NotAPol))


  Numeric <- 10
  Integer <- 10L

  Point <- st_point(c(286505,583134))
  NotAPoint <- c(1,2)

  Rslt <- rotatepolygon(p = Pol, angle = Numeric, fixed = Point)


  # Check the function arguments
  expect_error(rotatepolygon(p = NotAPol, angle = Numeric, fixed = Point),
               regexp = "The 'p' argument of the 'rotatepolygon' function must be a sf, a sfc_POLYGON or a POLYGON")

  expect_error(rotatepolygon(p = Pol, angle = Integer, fixed = Point),
               regexp = "The 'angle' argument of the 'rotatepolygon' function must be numeric")

  expect_error(rotatepolygon(p = Pol, angle = Numeric, fixed = NotAPoint),
               regexp = "The 'fixed' argument of the 'rotatepolygon' function must be a POINT")



  # result is a polygon
  expect_s3_class(Rslt, "sfc_POLYGON")


  # check the angle and the fixed point
  # = Compute the angle between the starting position of the polygon and its final position
  Top <- sf::st_centroid(Pol)
  Arrival <- st_point(as.numeric(unlist( # sfc to sfg
    sf::st_centroid(Rslt))))

  expect_equal(as.numeric(matlib::angle(c(Top[1] - Point[1], Top[2] - Point[2]),
                                    c(Arrival[1] - Point[1], Arrival[2] - Point[2]),
                                    degree = TRUE)), Numeric)


})


# check args errors
# result is a polygon
# check the angle
# and the fixed point
