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

  Point <- st_point(c(1,2))
  NotAPoint <- c(1,2)

  Rslt <- rotatepolygon(p = Pol, angle = Numeric, fixed = Point)


  # Check the function arguments
  expect_error(rotatepolygon(p = NotAPol, angle = Numeric, fixed = Point),
               regexp = "The 'p' argument of the 'rotatepolygon' function must be a POLYGON or a sfc_POLYGON")

  expect_error(rotatepolygon(p = Pol, angle = Integer, fixed = Point),
               regexp = "The 'angle' argument of the 'rotatepolygon' function must be numeric")

  expect_error(rotatepolygon(p = Pol, angle = Numeric, fixed = NotAPoint),
               regexp = "The 'fixed' argument of the 'rotatepolygon' function must be a POINT")



  # result is a polygon
  expect_s3_class(Rslt, "sfc_POLYGON")


  # check the angle

  # and the fixed point

})


# check args errors
# result is a polygon
# check the angle
# and the fixed point
