#' @title Nasa Asteroid Data Analysis
#' @description You can have data analysis on Asteroids data from NASA Open API
#' @field formula Formula
#' @export nasaAsteroid
#' @exportClass nasaAsteroid
nasaAsteroid <- setRefClass("nasaAsteroid",
                      fields = list(api_key = "character",near_earth_objects = "data.frame"),
                      methods = list(
                        initialize = function(api_key){
                          # check if you have saved data before
                          # if not fetch data
                          library(httr)
                          asteroids <- GET("https://api.nasa.gov/neo/rest/v1/neo/browse", 
                                           query = list(api_key = api_key)
                          )
                          # near_earth_objects <<- list()
                          # result <- content(asteroids)
                          # totalPage <- result$page$total_pages
                          df <- data.frame(Name=character(),
                                           absolute_magnitude_h=numeric(),
                                           estimated_diameter_kilometers_min=numeric(),
                                           estimated_diameter_kilometers_max=numeric(),
                                           is_potentially_hazardous_asteroid=logical(),
                                           close_approach_data_date=as.Date(character()),
                                           orbital_period=numeric(),stringsAsFactors=FALSE
                          ) 
                          for (page in 1:5){
                            nthPage <- GET("https://api.nasa.gov/neo/rest/v1/neo/browse",
                                              query = list(api_key = api_key,page = page)
                            )
                            result <- content(nthPage)
                            for(i in 1:20){
                              asteroid <- result$near_earth_objects[[i]]
                              Name <- asteroid$name
                              absolute_magnitude_h <- asteroid$absolute_magnitude_h
                              estimated_diameter_kilometers_min <- asteroid$estimated_diameter$kilometers$estimated_diameter_min
                              estimated_diameter_kilometers_max <- asteroid$estimated_diameter$kilometers$estimated_diameter_max
                              is_potentially_hazardous_asteroid <- asteroid$is_potentially_hazardous_asteroid
                              close_approach_data_date <- asteroid$orbital_data$orbit_determination_date
                              orbital_period <- asteroid$orbital_data$orbital_period
                              df <- rbind(df,list(
                                Name,
                                absolute_magnitude_h,
                                estimated_diameter_kilometers_min,
                                estimated_diameter_kilometers_max,
                                is_potentially_hazardous_asteroid,
                                close_approach_data_date,
                                orbital_period
                              ),stringsAsFactors=FALSE)
                            }
                          }
                          # result <- content(asteroids)
                          # print(result$near_earth_objects[[20]]$orbital_data$mean_motion)
                          colnames(df) <- c("Name",
                                            "absolute_magnitude_h",
                                            "estimated_diameter_kilometers_min",
                                            "estimated_diameter_kilometers_max",
                                            "is_potentially_hazardous_asteroid",
                                            "close_approach_data_date",
                                            "orbital_period")
                          near_earth_objects <<- df
                          print(df)
                        }
                      ))



# nasa <- nasaAsteroid$new("tYWfgxjr4fPL3KYfmtzWGQvmLcxe7fCciJ3hZjuz")

