LON_DEFAULT <- -1.733029 
LAT_DEFAULT <- 50.820184
ZOOM_DEFAULT <- 13

YEAR_MIN <- 2025
YEAR_MAX <- 2049
YEAR_DEFAULT <- 2025

POPUP_SIGFIG <- 2

CARBON_MIN <- 500
CARBON_MAX <- 1000
CARBON_DEFAULT <-800

SPECIES_DEFAULT <- 12
SPECIES_GM_DEFAULT <- 90
SPECIES_SB_DEFAULT <- 20
SPECIES_LICHENS_DEFAULT <- 2
AREA_DEFAULT <- 7
RECREATION_DEFAULT <- 10

CHECKBOX_COL <- 1
SLIDER_COL <- 11

AVAILABLE_PARCEL_COLOUR <- "#b1b1b1"
UNAVAILABLE_PARCEL_COLOUR <- "#808080"
BLOCKED_PARCEL_COLOUR <- "#ff2222"
PARCEL_LINE_COLOUR <- "#000000"
FILL_OPACITY <- 0.7

COLOUR_MAPPING <- c("Conifer" = "#006400", "Deciduous" = "#008080", "Available" = AVAILABLE_PARCEL_COLOUR, "Unavailable" = UNAVAILABLE_PARCEL_COLOUR, "Blocked" = BLOCKED_PARCEL_COLOUR)
COLOUR_MAPPING2 <- c("Conifer" = "#ff0", "Deciduous" = "#dd0", "Available" = AVAILABLE_PARCEL_COLOUR, "Unavailable" = UNAVAILABLE_PARCEL_COLOUR, "Blocked" = BLOCKED_PARCEL_COLOUR)
# MOCK_SLEEP_TIME <- 0.8

API_PORT <- Sys.getenv("API_PORT")
if (API_PORT == "") {
  API_PORT <- 40000
}
API_HOST <- Sys.getenv("API_HOST")
if (API_HOST == "") {
  API_HOST <- "144.173.60.164"
}
API_URL <- paste0("http://", API_HOST, ":", API_PORT)

SLIDER_NAMES <- list(
  carbon = list(name = "Tree Carbon Stored", unit = HTML("Tonnes of CO<sub>2</sub>")),
  species = list(name = "Species Richness", unit = ""),
  species_goat_moth = list(name = "Goat Moth", unit = "%"),
  species_stag_beetle = list(name = "Stag Beetle", unit = "%"),
  species_lichens = list(name = "Lichen", unit = "%"),
  area = list(name = "Area Planted", unit = HTML("km<sup>2</sup>")),
  recreation = list(name = "Recreation", unit = "Visits/month")
)