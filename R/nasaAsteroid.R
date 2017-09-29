#' @title Nasa Asteroid Data Analysis
#' @description Data analysis on nasa asteroid data source NASA Open Api
#' @field formula Formula
#' @export nasaAsteroid
#' @exportClass nasaAsteroid
nasaAsteroid <- setRefClass("nasaAsteroid",
                      fields = list(api_key = "character",near_earth_objects = "data.frame"),
                      methods = list(
                        initialize = function(api_key){
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
                                           orbital_period=numeric(),stringsAsFactors=FALSE
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
                          colnames(df) <- c("Name",
                                            "absolute_magnitude_h",
                                            "estimated_diameter_kilometers_min",
                                            "estimated_diameter_kilometers_max",
                                            "is_potentially_hazardous_asteroid",
                                            "close_approach_data_date",
                                            "orbital_period")
                          near_earth_objects <<- df
                          print(df)
                        },
                        hazardousAsteroids = function(){
                                          hazardous <- near_earth_objects[near_earth_objects['is_potentially_hazardous_asteroid'] == TRUE,]
                                          cat("Total asteroid is",nrow(near_earth_objects))
                                          cat("\n")
                                          cat("Total hazardous asteroid number is",nrow(hazardous))
                        }
                      ))


# nasa <- nasaAsteroid$new("tYWfgxjr4fPL3KYfmtzWGQvmLcxe7fCciJ3hZjuz")

