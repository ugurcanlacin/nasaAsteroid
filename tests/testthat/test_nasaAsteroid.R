context("nasaAsteroid")

test_that("Hazardous asteroid is correct", {
     nasa <- nasaAsteroid$new("tYWfgxjr4fPL3KYfmtzWGQvmLcxe7fCciJ3hZjuz")
  
    expect_output(nasa$hazardousAsteroids(), "Total asteroid is( )*[0-9]*")  
    expect_output(nasa$hazardousAsteroids(), "Total hazardous asteroid number is( )*[0-9]*")
  })

test_that("Wrong input throws an error.", {
  
  expect_error(nasaAsteroid(100) )
  expect_error(nasaAsteroid(TRUE) )
  expect_error(nasaAsteroid(asasa)) 
})
 
test_that("Summary output is correct", {
  nasa <- nasaAsteroid$new("tYWfgxjr4fPL3KYfmtzWGQvmLcxe7fCciJ3hZjuz")
  
  expect_output(nasa$summary(), "absolute_magnitude_h mean for asteroids is( )*[0-9]*")  
})