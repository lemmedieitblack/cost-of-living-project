
library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(viridis)


rental_data_path <- "Data cleanning/Cleaned Data for Rental.csv"
salary_data_path <- "Data cleanning/cleaned_salary_data.csv"
housing_data_path <- "Data cleanning/cleaned_houses.csv"
food_data_path <- "Data used/wfp_food_prices_arm.csv"
auto_data_path <- "Data cleanning/auto_am_en_data.csv"

rental_data <- reactive({
  req(file.exists(rental_data_path))
  read_csv(rental_data_path) %>%
    rename_with(make.names) %>%
    mutate(
      Listing.Name = str_replace_all(Listing.Name, "\"", ""),
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude),
      Popup_Info = sprintf(
        "<strong>%s</strong><br />Host: %s<br />Guests: %s | Bedrooms: %s<br />Price: $%s<br />Rating: %.1f ⭐<br />Reviews: %s",
        Listing.Name, Host.Name, Max.Guests, Bedrooms,
        TTM.Avg.Daily.Rate, Overall.Rating, Number.of.Reviews
      )
    ) %>%
    filter(!is.na(Latitude), !is.na(Longitude))
})

rental_data_reactive <- reactive({ rental_data() })

salary_data <- reactive({
  req(file.exists(salary_data_path))
  read_csv(salary_data_path) %>%
    rename_with(make.names) %>%
    mutate(
      across(c(Company, Position, Position_Standardized, Salary_Category), as.character),
      Experience_Years = case_when(
        str_detect(Experience_Level, "Entry Level") ~ 0,
        str_detect(Experience_Level, "Junior")     ~ 1,
        str_detect(Experience_Level, "Mid-Level")  ~ 3.5,
        str_detect(Experience_Level, "Senior")     ~ 7.5,
        str_detect(Experience_Level, "Expert")     ~ 12,
        TRUE ~ NA_real_
      )
    ) %>%
    select(Company, Position, Position_Standardized,
           Salary = Salary_AMD, Experience_Level,
           Experience_Years, Salary_Category)
})

salary_data_reactive <- reactive({ salary_data() })

houses_data <- reactive({
  req(file.exists(housing_data_path))
  read_csv(housing_data_path) %>%
    rename_with(make.names) %>%
    mutate(
      price           = as.numeric(price),
      condition       = as.factor(condition),
      district        = as.factor(district),
      max_floor       = as.integer(max_floor),
      num_rooms       = as.integer(num_rooms),
      region          = as.factor(region),
      area            = as.numeric(area),
      num_bathrooms   = as.integer(num_bathrooms),
      building_type   = as.factor(building_type),
      floor           = as.integer(floor),
      ceiling_height  = as.numeric(ceiling_height)
    )
})

houses_data_reactive <- reactive({ houses_data() })


food_data <- reactive({
  req(file.exists(food_data_path))
  read_csv(food_data_path) %>%
    filter(category != "non-food") %>%
    mutate(
      date = parse_date_time(date, orders = c("ymd", "dmy", "mdy", "Ymd", "Y-m-d")),
      price = as.numeric(price),
      usdprice = as.numeric(usdprice)
    ) %>%
    filter(!is.na(date), !is.na(usdprice), usdprice > 0) %>%
    mutate(
      month = format(date, "%m"),
      year = format(date, "%Y")
    )
})

food_data_reactive <- reactive({ food_data() })

auto_data <- reactive({
  req(file.exists(auto_data_path))
  df <- read_csv(auto_data_path)
  
  brands <- c("Toyota", "Mazda", "Mercedes-Benz", "BMW", "Tesla", "Suzuki", "Hyundai", 
              "Kia", "Lexus", "Volkswagen", "Audi", "Honda", "Ford", "Opel", "Nissan", 
              "Jeep", "Chevrolet", "Land Rover", "Porsche", "Subaru", "Mitsubishi", 
              "Infiniti", "Jaguar", "Mini", "Genesis", "Bentley", "Dodge", "Rolls Royce", 
              "Cadillac", "Volvo", "Smart", "Fiat", "Chery", "GAZ", "VAZ (Lada)", 
              "Leapmotor", "BYD", "Hongqi", "Zeekr", "Xiaomi", "Lixiang", "Avatr", 
              "Sena", "Dongfeng", "Wuling", "Renault", "Exeed", "Alfa Romeo", "Alpina", 
              "Lincoln", "Hummer", "Aston Martin", "Maybach", "Arcfox", "Neta", "Changan")
  
  brand_pattern <- paste0("^(", paste(str_replace_all(brands, "([()])", "\\\\\\1"), collapse = "|"), ")")
  
  df %>%
    filter(motor != "" & color != "") %>%
    mutate(
      price = parse_number(as.character(price)),
      brand = str_extract(car, brand_pattern)
    ) %>%
    filter(!is.na(price), !is.na(color), !is.na(motor), !is.na(brand))
})

auto_data_reactive <- reactive({ auto_data() })

source("modules/rental_map_module.R")
source("modules/analytics_module.R")
source("modules/visuals_module.R")
source("modules/salary_module.R")
source("modules/houses_module.R")
source("modules/Food_Dashboards.R") 
source("modules/auto_module.R")