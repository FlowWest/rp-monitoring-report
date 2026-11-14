# USGS ------------
library(dataRetrieval)
library(tidyverse)
library("CDECRetrieve")
library(here)

start <- as.Date("2026-04-01")
end <- as.Date("2026-06-01")

## Get flow at Colusa (for Rancho Caleta and Willow Bend) -----------------
flow_colusa <- read_waterdata_continuous(monitoring_location_id = "USGS-11389500",
                                     parameter_code = "00060",
                                     time = c(start, end))
hourly_flow_colusa <- flow_colusa |> 
  mutate(date = date(time),
         hour = hour(time)) |> 
  group_by(date, hour, unit_of_measure) |> 
  slice(1) |> 
  ungroup() |> 
  select(location = monitoring_location_id, datetime = time, flow_cfs = value)

write_csv(hourly_flow_colusa, here("data-raw", "hourly_flow_colusa.csv"))

## Get flow at HMC (Wilsons Landing) -------------------
get_data <- function(station) {
  cdec_query(station, 25, "E", start, end)
}

hourly_discharge_hmc <- get_data("HMC")|> 
  mutate(date = date(datetime),
         hour = hour(datetime)) |> 
  group_by(date, hour) |> 
  slice(1) |> 
  ungroup() |> 
  select(location = location_id, datetime, flow_cfs = parameter_value)

write_csv(hourly_discharge_hmc, here("data-raw", "hourly_discharge_HMC.csv"))

hourly_temperature_bso <- get_data("BSO")|> 
  mutate(date = date(datetime),
         hour = hour(datetime)) |> 
  group_by(location_id, date, hour) |> 
  slice(1) |> 
  ungroup() |> 
  select(location = location_id, datetime, temp = parameter_value)

## likely pretty upstream 
# daily
temperature_rdb <- get_data("RDB")|> 
  mutate(date = date(datetime),
         hour = hour(datetime)) |> 
  group_by(location_id, date) |> 
  slice(1) |> 
  ungroup() |> 
  select(location = location_id, datetime, temp = parameter_value)

## likely pretty downstream 
# event
temperature_wlk <- get_data("WLK")|> 
  mutate(date = date(datetime),
         hour = hour(datetime)) |> 
  group_by(location_id, date) |> 
  summarize(mean_temp = mean(parameter_value)) |> 
  ungroup() |> 
  select(location = location_id, datetime=date, temp = mean_temp)

temperature <- bind_rows(temperature_rdb, temperature_wlk) |> 
  group_by(datetime) |> 
  reframe(temperature = mean(temp),
          sd = sd(temp, na.rm = FALSE))

write_csv(temperature_rdb, here("data-raw", "temp_rdb.csv"))
write_csv(temperature_wlk, here("data-raw", "temp_wlk.csv"))

## turbidity ----------------

get_data <- function(station) {
  cdec_query(station, 25, "E", start, end)
}

hourly_turb_rdb <- cdec_query("RDB", 27, "H", start, end)|> 
  select(location = location_id, datetime, turb = parameter_value)
write_csv(hourly_turb_rdb, here("data-raw", "hourly_turb_rdb.csv"))
