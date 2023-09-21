
library(odeqIRtools)
library(tidyverse)
library(lubridate)
library(runner)


# Download air temperature data from NOAA -------------------------------------------------------------------------


#These are the air stations we used in 2018/2020

air_temp_stations <-c(
  "USC00350036",
  "USR0000OAGN",
  "USC00350118",
  "USR0000OALL",
  "USC00350145",
  "USS0017D02S",
  "USS0022G06S",
  "USC00350197",
  "USR0000OANT",
  "USS0019D02S",
  "USC00350265",
  "USC00350304",
  "USW00094224",
  "USW00094281",
  "USR0000OBAD",
  "USW00024130",
  "USR0000OBKL",
  "USR0000OBAL",
  "USC00350471",
  "USC00350501",
  "USR0000OBAS",
  "USS0018D09S",
  "USC00350652",
  "USC00350699",
  "USC00350694",
  "USS0022G21S",
  "USS0023G15S",
  "USS0022G13S",
  "USR0000OBLA",
  "USS0021D33S",
  "USR0000OBLU",
  "USS0018E16S",
  "USR0000OBOC",
  "USR0000OBOH",
  "USC00350858",
  "USC00350897",
  "USR0000OBOU",
  "USS0018E05S",
  "USS0018D20S",
  "USR0000OBRI",
  "USC00351058",
  "USR0000OBRO",
  "USR0000OBRU",
  "USR0000OBUE",
  "USR0000OBUS",
  "USW00094185",
  "USR0000OCAB",
  "USR0000OCAI",
  "USR0000OCAL",
  "USR0000OCAN",
  "USS0022F03S",
  "USR0000OCAS",
  "USS0021F22S",
  "USC00351546",
  "USR0000OCHI",
  "USR0000OCIN",
  "USS0021D13S",
  "USC00351643",
  "USS0021D12S",
  "USC00351682",
  "USS0018D08S",
  "USS0022G24S",
  "USR0000OCOD",
  "USR0000OCOL",
  "USC00351765",
  "USW00004141",
  "USC00351836",
  "USW00004236",
  "USC00351862",
  "USC00351877",
  "USC00351897",
  "USC00351902",
  "USC00351914",
  "USR0000OCRA",
  "USC00351946",
  "USS0020G12S",
  "USR0000OCRO",
  "USC00352112",
  "USS0022E08S",
  "USC00352135",
  "USC00352173",
  "USS0019E03S",
  "USC00352292",
  "USS0022F18S",
  "USC00352374",
  "USC00352406",
  "USR0000ODUN",
  "USR0000OEAG",
  "USR0000OEDE",
  "USS0018E03S",
  "USR0000OECK",
  "USC00352632",
  "USC00352633",
  "USR0000OEMI",
  "USS0018D04S",
  "USC00352693",
  "USW00024221",
  "USR0000OEVA",
  "USR0000OFAL",
  "USC00352867",
  "USR0000OFIE",
  "USR0000OFIN",
  "USS0018G02S",
  "USR0000OFIS",
  "USS0022G14S",
  "USR0000OFLA",
  "USC00352973",
  "USC00352997",
  "USR0000OFOR",
  "USC00353047",
  "USR0000OFOS",
  "USS0022G12S",
  "USR0000OGER",
  "USS0021G04S",
  "USC00353356",
  "USS0018E08S",
  "USC00353402",
  "USR0000OGAD",
  "USC00353445",
  "USR0000OGRM",
  "USS0021D01S",
  "USC00353542",
  "USC00353604",
  "USR0000OHAR",
  "USC00353692",
  "USR0000OHAY",
  "USC00353770",
  "USR0000OHEH",
  "USC00353827",
  "USC00353818",
  "USW00004113",
  "USR0000OHIG",
  "USS0018D19S",
  "USS0021E06S",
  "USS0022F42S",
  "USC00353995",
  "USC00354003",
  "USR0000OHOR",
  "USC00354060",
  "USR0000OHOY",
  "USR0000OILL",
  "USS0021F21S",
  "USR0000OJRI",
  "USW00004125",
  "USS0022E07S",
  "USR0000OKE2",
  "USR0000OKEL",
  "USC00354403",
  "USS0023G09S",
  "USR0000CKLA",
  "USC00354622",
  "USC00354606",
  "USR0000OLAG",
  "USS0018E18S",
  "USC00354721",
  "USC00354776",
  "USR0000OLAV",
  "USC00354811",
  "USC00354819",
  "USC00354835",
  "USR0000OLMC",
  "USS0022E09S",
  "USR0000OLOG",
  "USC00355050",
  "USC00355055",
  "USS0018D06S",
  "USS0019D03S",
  "USC00355160",
  "USC00355221",
  "USS0021E04S",
  "USC00355258",
  "USS0021E07S",
  "USW00094273",
  "USC00355392",
  "USW00024225",
  "USR0000OMER",
  "USR0000OMET",
  "USR0000OMID",
  "USS0017D20S",
  "USR0000OMLL",
  "USS0023D03S",
  "USC00355593",
  "USC00355638",
  "USC00355681",
  "USC00355711",
  "USR0000OMOO",
  "USR0000OMOR",
  "USS0017D06S",
  "USS0021D08S",
  "USS0017D18S",
  "USR0000OMOU",
  "USR0000OMTW",
  "USR0000OMTY",
  "USS0021D35S",
  "USR0000OMUT",
  "USC00355945",
  "USS0021F10S",
  "USW00024284",
  "USS0022D02S",
  "USR0000ONPR",
  "USC00356179",
  "USC00356213",
  "USS0020E02S",
  "USC00356252",
  "USW00024162",
  "USC00356366",
  "USC00356405",
  "USR0000OOWY",
  "USR0000OPHI",
  "USC00356426",
  "USR0000OPAR",
  "USR0000OPAT",
  "USS0021D14S",
  "USR0000OPEB",
  "USC00356532",
  "USW00024155",
  "USC00356550",
  "USC00356634",
  "USR0000OPOL",
  "USC00356784",
  "USW00094261",
  "USW00024229",
  "USC00356749",
  "USW00024242",
  "USC00356750",
  "USC00356820",
  "USC00356883",
  "USC00356907",
  "USR0000OPRO",
  "USR0000OQUA",
  "USS0020G06S",
  "USS0022F05S",
  "USR0000OREB",
  "USR0000ORED",
  "USS0021D04S",
  "USR0000OREM",
  "USW00024230",
  "USC00357127",
  "USR0000ORID",
  "USW00004128",
  "USS0022F43S",
  "USR0000OROB",
  "USR0000OROC",
  "USS0018F01S",
  "USR0000ORCK",
  "USC00357310",
  "USC00357331",
  "USR0000OROU",
  "USC00357391",
  "USR0000ORYE",
  "USS0023D01S",
  "USR0000OSAG",
  "USW00024232",
  "USS0022F04S",
  "USR0000OSAL",
  "USS0021E05S",
  "USW00004201",
  "USS0017D08S",
  "USC00357641",
  "USS0023D02S",
  "USR0000OSEL",
  "USC00357675",
  "USS0022G33S",
  "USW00024235",
  "USR0000OSIG",
  "USR0000OSIL",
  "USC00357809",
  "USS0021F12S",
  "USC00357817",
  "USC00357823",
  "USS0018G01S",
  "USC00357857",
  "USR0000OSLI",
  "USS0019F01S",
  "USS0022D03S",
  "USR0000OSFK",
  "USR0000OSPA",
  "USC00358029",
  "USR0000OSQU",
  "USS0019E07S",
  "USC00358095",
  "USR0000OSTR",
  "USS0020G09S",
  "USR0000OSUG",
  "USC00358173",
  "USS0020G02S",
  "USS0022F14S",
  "USR0000OSUM",
  "USS0021G17S",
  "USC00358246",
  "USS0021G16S",
  "USS0021G03S",
  "USS0017D07S",
  "USC00358407",
  "USS0021E13S",
  "USC00358466",
  "USC00358498",
  "USR0000OTIL",
  "USC00358494",
  "USR0000OTIM",
  "USS0018E09S",
  "USS0022F45S",
  "USC00358536",
  "USR0000OTOK",
  "USR0000OTRO",
  "USR0000OTUM",
  "USR0000OTUP",
  "USC00358726",
  "USR0000OUMA",
  "USC00358746",
  "USC00358884",
  "USR0000OVIL",
  "USR0000OWAG",
  "USC00358997",
  "USR0000OWAM",
  "USR0000OWAN",
  "USR0000OWAS",
  "USC00359316",
  "USR0000OWIL",
  "USC00359461",
  "USS0018D21S",
  "USC00359588",
  "USR0000OYLP",
  "USR0000OYEL",
  "USR0000OZIM"
)

# Rather than replacing the errors with values, safely() returns both the results and the errors in a list. This
# function is also a wrapper function. It defaults to using otherwise = NULL, and I generally haven’t had reason
# to change away from that default.
noaa_air_safe <- safely(.f = noaa_air)

NOAA_air_temp <- air_temp_stations %>%
  map(noaa_air_safe, '2010-12-26', '2022-12-31')

NOAA_air <- bind_rows(map(NOAA_air_temp, "result"))


# errors ----------------------------------------------------------------------------------------------------------


  error_stations <- setdiff(air_temp_stations, NOAA_air$STATION)

  NOAA_air_temp_error <- error_stations %>%
    map(noaa_air_safe, '2010-12-26', '2022-12-31')

  NOAA_air_errors <- bind_rows(map(NOAA_air_temp_error, "result"))


  NOAA_air <- bind_rows(NOAA_air, NOAA_air_errors)

# Calculate air exclusion thresholds for each station -------------------------------------------------------------


  a <- Sys.time()
  air_temp_7d <- NOAA_air %>%
    dplyr::filter(!is.na(TMAX)) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE),
                  TMAX = as.numeric(TMAX)) %>%
    dplyr::group_by(STATION) %>%
    dplyr::mutate(row = dplyr::row_number(),
                  d = runner(x = data.frame(dDTmax_run = TMAX,
                                            date_run = DATE),
                             k = "7 days",
                             lag = 0,
                             idx = DATE,
                             f = function(x) list(x))) %>%
    dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                   dplyr::summarise(ma.max7 = dplyr::case_when(length(dDTmax_run) >= 6 ~  mean(dDTmax_run),
                                                                               TRUE ~ NA_real_),
                                                    ana_startdate7 = min(date_run),
                                                    ana_enddate7   = max(date_run),
                                                    act_enddate7   = max(date_run),
                                                    comment =dplyr::case_when(length(dDTmax_run) < 6 ~  'Not enough values to calculate 7 day metric',
                                                                              TRUE ~ NA_character_) )

    )) %>%
    tidyr::unnest_wider(d) %>%
    dplyr::filter(DATE >= lubridate::ymd('2012-01-01'))

  Sys.time() - a


air_temp_7d_2 <- air_temp_7d %>%
  mutate(in_crit = ifelse(lubridate::month(DATE) %in% c(7, 8, 9), 1, 0 ),
         year = lubridate::year(DATE))

OR_airtemp_exclusion_thresholds <- air_temp_7d_2 %>%
  filter(!is.na(ma.max7)) %>%
  group_by(STATION,year ) %>%
  summarise(max_7d_temp = max(ma.max7, na.rm = TRUE),
            num_critical_period = sum(in_crit, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sufficient_crit = ifelse(num_critical_period >= 0.8*92, 1, 0 )) %>%
  filter(sufficient_crit == 1) %>%
  group_by(STATION) %>%
  mutate(num_sufficient_years = n_distinct(year)) %>%
  filter(num_sufficient_years == 10) %>%
  ungroup() %>%
  group_by(STATION) %>%
  summarise(air_temp_exclusion_value = quantile(max_7d_temp, 0.90)) %>%
  rename(Air_temp_station = STATION)


usethis::use_data(OR_airtemp_exclusion_thresholds, overwrite = TRUE)

OR_air_temp <- air_temp_7d %>%
  filter(STATION %in% unique(OR_airtemp_exclusion_thresholds$Air_temp_station)) %>%
  select(STATION, LONGITUDE, LATITUDE, DATE, TMAX) %>%
  rename(Air_Station  = STATION,
         Air_temp_long = LONGITUDE,
         Air_temp_lat = LATITUDE,
         Date = DATE,
         Air_Temp_daily_max = TMAX) %>%
 left_join(OR_airtemp_exclusion_thresholds, by = c('Air_Station' = 'Air_temp_station')) %>%
  mutate(above_exclusion_1d = case_when(Air_Temp_daily_max > air_temp_exclusion_value ~ "Yes",
                                     Air_Temp_daily_max <=  air_temp_exclusion_value ~ "No",
                                     TRUE ~ "ERROR") ) %>%
  dplyr::group_by(Air_Station) %>%
  dplyr::mutate(d = runner(x = data.frame(dDTmax_run = Air_Temp_daily_max,
                                          date_run = Date,
                                          exclusion_value = air_temp_exclusion_value),
                           k = "7 days",
                           lag = 0,
                           idx = Date,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(above_exclusion_7d = case_when(max(dDTmax_run) > first(exclusion_value) ~ "Yes",
                                                                                 TRUE ~ "No") )

  )) %>%
  tidyr::unnest_wider(d)



usethis::use_data(OR_air_temp, overwrite = TRUE)

#save(OR_air_temp, file = '//deqlab1/Assessment/Integrated_Report/DataSources/2022/NOAA air temp/NOAA_air_temp.RDATA')


# Generate list of usable stations --------------------------------------------------------------------------------

#Import this in ARCGIS and calculate shp file of polygons
#save shp file to Air_temp_stations
OR_usable_air_stations <- OR_air_temp %>%
  select(Air_Station,  Air_temp_lat, Air_temp_long) %>%
  rename(Longitude = Air_temp_long,
         Latitude = Air_temp_lat) |>
  distinct()

write.csv(OR_usable_air_stations, 'data-raw/NOAA_usable_airstations.csv',
          row.names = FALSE)

