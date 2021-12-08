#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(lubridate)
library(plotly)
library(tidygeocoder)
library(geodist)
library(leaflet)
library(geosphere)
library(shiny)

manhattan_stations <- read_csv("./manhattan_stations.csv")
manhattan_rides <- read_csv("./manhattan_rides.csv")
nyc_locations <- read_csv("./nyc_locations.csv")

# manhattan_stations_upd filters out any citi bike station that appears more than once
# (i.e. some stations with the same id have slightly different coordinates and thus appear
# multiple times)
manhattan_stations_upd <-
  manhattan_stations %>% 
  group_by(id) %>% 
  mutate(
    num_rep = row_number()
  ) %>% 
  ungroup() %>% 
  filter(num_rep == 1) %>% 
  select(!num_rep)

# manhattan_rides_diff filters out manhattan_rides so we're only looking at rides
# that start and end at different stations
manhattan_rides_diff <-
  manhattan_rides %>%  
  filter(start_station_id != end_station_id)

# the closest_station_info_fn takes in vectors of latitude and longitude
# and returns a tibble where we get for each nyc location 
# the geographic info for the closest end_station 
# the geographic info for the most common start_station for trips ending at that end_station
# the average age of riders for trips ending at that end_station
# the average trip duration for trips ending at that end_station
closest_station_info_fn <- function(nyc_lat, nyc_long) {
  
  station <-
    manhattan_stations_upd %>%
    mutate(
      distance = 
        (geodist_vec(
          x1 = rep(nyc_long, nrow(manhattan_stations_upd)),
          y1 = rep(nyc_lat, nrow(manhattan_stations_upd)),
          x2 = longitude, 
          y2 = latitude, 
          paired = T, 
          measure = "vincenty"
        ) * 3.281) 
    ) %>% 
    slice_min(order_by = distance)
  
  start_station_popular <-
    manhattan_rides_diff %>% 
    filter(end_station_id %in% pull(station, id)) %>% 
    group_by(start_station_id, start_station_name, start_station_latitude, start_station_longitude) %>% 
    summarize(
      total_trips = n()
    ) %>% 
    ungroup() %>% 
    slice_max(order_by = total_trips)
  
  avg_table <-
    manhattan_rides_diff %>% 
    filter(
      end_station_id %in% pull(station, id)
    ) %>% 
    group_by(end_station_id, end_station_name, end_station_latitude, end_station_longitude) %>% 
    summarize(
      avg_trip_min = mean(tripduration/60), 
      avg_age = mean(age)
    ) %>% 
    ungroup() %>%
    bind_cols(
      start_station_popular
    ) %>% 
    rename_with(~str_replace(., "^end_station_", "end_")) %>% 
    rename_with(~str_replace(., "^start_station_", "start_")) %>% 
    select(!total_trips) %>% 
    relocate(avg_trip_min:avg_age, .after = start_longitude)
  
  return(avg_table)
  
}

# in the event there are ties for most common start_station for a particular end_station
# we take the start_station that is furthest from the end_station
# we also add a variable 'mi_from_end' that tells how many miles the destination is from the end_station
nyc <-
  nyc_locations %>% 
  mutate(
    closest_station_info = map2(.x = location_lat, .y = location_long, .f = closest_station_info_fn)
  ) %>% 
  unnest(closest_station_info) %>% 
  mutate(
    distance_to_start = geodist_vec(
      x1 = start_longitude,
      y1 = start_latitude,
      x2 = end_longitude, 
      y2 = end_latitude, 
      paired = T, 
      measure = "vincenty"
    ), 
    location = factor(location)
  ) %>% 
  group_by(location) %>% 
  slice_max(order_by = distance_to_start) %>% 
  select(!distance_to_start) %>% 
  mutate(
    mi_from_end = geodist_vec(
      x1 = location_long, 
      y1 = location_lat, 
      x2 = end_longitude, 
      y2 = end_latitude,
      paired = T, 
      measure = "vincenty"
    ) / 1609.344
  ) %>% 
  relocate(mi_from_end, .before = start_id) %>% 
  ungroup()

nyc_choices <-
  nyc_locations %>% 
  pull(location)

### Shiny app stuff
ui <- 
  fluidPage(
    titlePanel("Visualizing Trips to Manhattan Destinations"), 
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "site_choice", 
          label = h4("Select your destination:"), 
          choices = nyc_choices
        ), 
        width = 3
      ),
      mainPanel(
        leafletOutput("manhatttan_map"), 
        br(), br(),
        plotOutput("lollipop_plot")
      )
    )
  )
server <- function(input, output) {
  output$manhatttan_map <- renderLeaflet({
    
    nyc_site <-
      nyc %>% 
      filter(
        location == input[["site_choice"]]
      )
    
    bike_icons <- awesomeIconList(
      red_bike   = makeAwesomeIcon(icon = 'bicycle', markerColor = 'red', iconColor = 'black', library = "fa"),
      green_bike = makeAwesomeIcon(icon = 'bicycle', markerColor = 'green', iconColor = 'black', library = "fa")
    )
    
    city_icons <- awesomeIconList(
      city_pic = makeAwesomeIcon(icon = 'building', markerColor = 'white', iconColor = 'black', library = "fa")
    )
    
    gcIntermediate(
      c(nyc_site$end_longitude[[1]], nyc_site$end_latitude[[1]]),
      c(nyc_site$start_longitude[[1]], nyc_site$start_latitude[[1]]),
      n = 100, 
      addStartEnd = TRUE,
      sp = TRUE
    ) %>% 
    leaflet() %>% 
      addAwesomeMarkers(lng = nyc_site$end_longitude[[1]],
                        lat = nyc_site$end_latitude[[1]],
                        popup = str_c("You parked at the ", nyc_site$end_name[[1]], " station"),
                        icon = bike_icons[["red_bike"]]
      ) %>% 
      addAwesomeMarkers(lng = nyc_site$start_longitude[[1]],
                        lat = nyc_site$start_latitude[[1]],
                        popup = str_c("You started at the ", nyc_site$start_name[[1]], " station"),
                        icon = bike_icons[["green_bike"]]
      ) %>% 
      addAwesomeMarkers(lng = nyc_site$location_long[[1]],
                        lat = nyc_site$location_lat[[1]], 
                        popup = str_c("Your destination: ", nyc_site$location[[1]]), 
                        icon = city_icons[["city_pic"]]
      ) %>% 
      addProviderTiles("OpenStreetMap.HOT") %>% 
      addPolylines()
    
  })
  
  output$lollipop_plot <- renderPlot({
    
    input[["site_choice"]]
    
    without_choice <-
      nyc %>% 
      filter(
        location != input[["site_choice"]]
      ) %>% 
      mutate(
        location = fct_reorder(location, avg_age)
      )
    
    choice_only <-
      nyc %>% 
      filter(
        location == input[["site_choice"]]
      )
    
    without_choice %>% 
      ggplot(aes(x = avg_age, y = location)) +
      geom_segment(aes(x = choice_only$avg_age[[1]], xend = avg_age, y = location, yend = location), color = "grey") +
      geom_point(
        color = ifelse(without_choice$avg_age > choice_only$avg_age[[1]], "blue", "red"),
        size = 4
      ) +
      theme_light() +
      theme(
        panel.grid.major.y = element_blank(), 
        panel.border = element_blank(), 
        axis.ticks.y = element_blank()
      ) +
      xlab("") + ylab("")
    
  })
  
}

shinyApp(ui = ui, server = server)

