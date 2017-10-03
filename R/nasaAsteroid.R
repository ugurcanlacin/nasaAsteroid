#' @title Nasa Asteroid Data Analysis
#' @description Data analysis on nasa asteroid data source NASA Open Api
#' @field formula Formula
#' @export nasaAsteroid
#' @exportClass nasaAsteroid
nasaAsteroid <- setRefClass("nasaAsteroid",
                      fields = list(api_key = "character",near_earth_objects = "data.frame"),
                      methods = list(
                        initialize = function(api_key){
                          if(class(api_key) != "character"){
                            stop()
                          }
                          # TODO
                          # check if you have saved data before
                          # if not fetch data
                          
                          # TODO 2
                          # Add more field to data frame
                          
                          # TODO 3
                          # Think about calculations with those values. Standart deviation, mean, etc.
                          
                          # TODO 4
                          # Answer some questions by implementing functions, such as, how many of them is above of avarage as diameter,
                          # how many of them dangerous, how many of them have close approach last 6 monts, etc.
                          
                          # Import httr library to fetch data
                          library(httr)
                          
                          # Fetch example
                          # asteroids <- GET("https://api.nasa.gov/neo/rest/v1/neo/browse", 
                          #                  query = list(api_key = api_key)
                          # )
                          # result <- content(asteroids)
                          # totalPage <- result$page$total_pages
                          
                          # Create a empty data frame
                          df <- data.frame(Name=character(),
                                           absolute_magnitude_h=numeric(),
                                           estimated_diameter_kilometers_min=numeric(),
                                           estimated_diameter_kilometers_max=numeric(),
                                           is_potentially_hazardous_asteroid=logical(),
                                           close_approach_data_date=as.Date(character()),
                                           orbital_period=numeric(),
                                           minimum_orbit_intersection=numeric(),
                                           epoch_osculation=numeric(),
                                           aphelion_distance=numeric(),
                                           perihelion_time=numeric(),
                                           mean_anomaly=numeric(),
                                           mean_motion=numeric(),
                                           stringsAsFactors=FALSE
                          ) 
                          
                          # Fetch 5 pages data from api
                          for (page in 1:5){
                            nthPage <- GET("https://api.nasa.gov/neo/rest/v1/neo/browse",
                                              query = list(api_key = api_key,page = page)
                            )
                            result <- content(nthPage)
                            # Iterate 20 time for each page to take asteroid information.
                            for(i in 1:20){
                              asteroid <- result$near_earth_objects[[i]]
                              Name <- asteroid$name
                              absolute_magnitude_h <- asteroid$absolute_magnitude_h
                              estimated_diameter_kilometers_min <- asteroid$estimated_diameter$kilometers$estimated_diameter_min
                              estimated_diameter_kilometers_max <- asteroid$estimated_diameter$kilometers$estimated_diameter_max
                              is_potentially_hazardous_asteroid <- asteroid$is_potentially_hazardous_asteroid
                              close_approach_data_date <- asteroid$orbital_data$orbit_determination_date
                              orbital_period <- asteroid$orbital_data$orbital_period
                              
                              minimum_orbit_intersection <- asteroid$orbital_data$minimum_orbit_intersection
                              epoch_osculation <- asteroid$orbital_data$epoch_osculation
                              aphelion_distance <- asteroid$orbital_data$aphelion_distance
                              perihelion_time <- asteroid$orbital_data$perihelion_time
                              mean_anomaly <- asteroid$orbital_data$mean_anomaly
                              mean_motion <- asteroid$orbital_data$mean_motion
                              df <- rbind(df,list(
                                Name,
                                absolute_magnitude_h,
                                estimated_diameter_kilometers_min,
                                estimated_diameter_kilometers_max,
                                is_potentially_hazardous_asteroid,
                                close_approach_data_date,
                                orbital_period,
                                minimum_orbit_intersection,
                                epoch_osculation,
                                aphelion_distance,
                                perihelion_time,
                                mean_anomaly,
                                mean_motion
                              ),stringsAsFactors=FALSE)
                            }
                          }
                          colnames(df) <- c("Name",
                                            "absolute_magnitude_h",
                                            "estimated_diameter_kilometers_min",
                                            "estimated_diameter_kilometers_max",
                                            "is_potentially_hazardous_asteroid",
                                            "close_approach_data_date",
                                            "orbital_period",
                                            "minimum_orbit_intersection",
                                            "epoch_osculation",
                                            "aphelion_distance",
                                            "perihelion_time",
                                            "mean_anomaly",
                                            "mean_motion")
                          near_earth_objects <<- df
                          # print(df)
                        },
                        getAsteroidsAsDataFrame = function(){
                          "Return asteroids as data frame"
                          return(near_earth_objects)
                        },
                        hazardousAsteroids = function(){
                          "Calculates how many asteroids are there and how many of them are hazardous"
                            hazardous <- near_earth_objects[near_earth_objects['is_potentially_hazardous_asteroid'] == TRUE,]
                            cat("Total asteroid is",nrow(near_earth_objects))
                            cat("\n")
                            cat("Total hazardous asteroid number is",nrow(hazardous))
                        },
                        meanSummary = function(){
                          "Calculates mean for some columns"
                          absolute_magnitude_h <- near_earth_objects['absolute_magnitude_h']
                          estimated_diameter_kilometers_max <- near_earth_objects['estimated_diameter_kilometers_max']
                          estimated_diameter_kilometers_min <- near_earth_objects['estimated_diameter_kilometers_min']
                          cat("absolute_magnitude_h mean for asteroids is",apply(absolute_magnitude_h,2,mean))
                          cat("\n")
                          cat("estimated_diameter_kilometers_max mean for asteroids is",apply(estimated_diameter_kilometers_max,2,mean))
                          cat("\n")
                          cat("estimated_diameter_kilometers_min mean for asteroids is",apply(estimated_diameter_kilometers_min,2,mean))
                          cat("\n")
                        },
                        medianSummary = function(){
                          "Calculates median for some columns"
                          absolute_magnitude_h <- near_earth_objects['absolute_magnitude_h']
                          estimated_diameter_kilometers_max <- near_earth_objects['estimated_diameter_kilometers_max']
                          estimated_diameter_kilometers_min <- near_earth_objects['estimated_diameter_kilometers_min']
                          cat("absolute_magnitude_h median for asteroids is",apply(absolute_magnitude_h,2,median))
                          cat("\n")
                          cat("estimated_diameter_kilometers_max median for asteroids is",apply(estimated_diameter_kilometers_max,2,median))
                          cat("\n")
                          cat("estimated_diameter_kilometers_min median for asteroids is",apply(estimated_diameter_kilometers_min,2,median))
                          cat("\n")
                        }
                      ))


# nasa <- nasaAsteroid$new("tYWfgxjr4fPL3KYfmtzWGQvmLcxe7fCciJ3hZjuz")
# df <- nasa$getAsteroidsAsDataFrame()
# nasa$hazardousAsteroids()
# nasa$meanSummary()
# nasa$medianSummary()
