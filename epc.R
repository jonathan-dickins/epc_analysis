library(httr)
library(dplyr)
library(readr)
library(ggspatial)
library(ggrepel)
library(sf)
library(grid)

# get postcodes into consistent format
fix_postcodes <- function(postcode) {
  str_to_upper(
    case_when(
      str_length(postcode) == 8 ~ str_replace(postcode, " ", ""), #e.g. "CB10 1AA" to "CB101AA"
      str_length(postcode) == 7 ~ postcode,
      str_length(postcode) == 6 & !str_detect(postcode, " ") ~
      str_c(str_sub(postcode, 1, -4), " ", str_sub(postcode, -3)), #e.g. "CB61AA" to "CB6 1AA"
      str_length(postcode) == 6 ~ str_replace(postcode, " ", "  "), #e.g. "M1 1AA" to "M1  1AA"
      str_length(postcode) == 5 ~ str_c(str_sub(postcode, 1, -4), "  ",
                                                          str_sub(postcode, -3)), #e.g. "M11AA" to "M1  1AA"
      TRUE ~ NA
    )
  )
}

# get lat/long points from postcodes
# https://geoportal.statistics.gov.uk/datasets/ons::ons-postcode-directory-february-2026-for-the-uk
get_coordinates_from_postcodes <- function(data, postcode_column) {
  
  x <- data |>
    mutate(pcd7 = fix_postcodes({{ postcode_column }})) |>
    inner_join(
      read_csv("ONSPD_FEB_2026_UK.csv") %>% select(pcd7, lat, long),
      by = "pcd7"
    )
  
}

cambs_codes <- c(
  "E07000008",  # Cambridge city
  "E07000009",  # East Cambridgeshire
  "E07000010",  # Fenland
  "E07000011",  # Huntingdonshire
  "E07000012",  # South Cambridgeshire
  "E06000031"   # Peterborough 
)

# function to query EPC API, takes single LA code or vector of codes
# API docs: https://epc.opendatacommunities.org/docs/api/domestic#domestic-cert
# credentials.csv needs 2 cols: email, api_key
pull_epc <- function(la_code, max_pages = 100) {
  creds <- read_csv("credentials.csv")
  all_data <- list()
  search_after <- NULL
  page <- 1
  
  while (page <= max_pages) {
    params <- list(size = 5000, `local-authority` = la_code)
    if (!is.null(search_after)) params[["search-after"]] <- search_after
    
    resp <- GET(
      "https://epc.opendatacommunities.org/api/v1/domestic/search",
      authenticate(
        user = creds$email[1],
        password = creds$api_key[1],
        type = "basic"
      ),
      add_headers(Accept = "text/csv"),
      query = params
    )
    
    chunk <- read_csv(
      content(resp, as = "text", encoding = "UTF-8"),
      show_col_types = FALSE,
      col_types = cols(.default = "c") # all cols as character to facilitate bind_rows
    )
    
    cat("LA:", la_code, "| Page:", page, "| Rows:", nrow(chunk), "\n")
    
    if (nrow(chunk) == 0) break
    
    all_data <- append(all_data, list(chunk))
    search_after <- headers(resp)[["X-Next-Search-After"]]
    
    if (is.null(search_after) || search_after == "") break
    
    page <- page + 1
  }
  
  if (page > max_pages) warning("Reached max_pages limit for LA: ", la_code)
  
  bind_rows(all_data)
}

cambs_epc <- bind_rows(lapply(cambs_codes, pull_epc)) |>
  mutate(across(c(`current-energy-efficiency`, `heating-cost-current`), as.numeric))

# msoa shapefile via Open Geography Portal
msoa <- read_sf("msoa/MSOA_2021_EW_BGC_V3.shp") %>%
  filter(grepl("Cambridge|Fenland|Huntingdonshire|Peterborough|Ely", 
               MSOA21NM, ignore.case = TRUE))

# quick check of the shapefile
ggplot(msoa) +
  geom_sf() 

# msoa names not included in the shapefile, add them back in
msoa_names <- read_csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv")

msoa <- msoa %>%
  left_join(msoa_names %>% select(msoa21cd, msoa21hclnm), 
            by = c("MSOA21CD" = "msoa21cd"))

# postcodes to lat long for spatial join
points <- cambs_epc_clean %>%
  get_coordinates_from_postcodes(postcode) %>%
  filter(!is.na(long), !is.na(lat)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(st_crs(msoa))

# check coordinate systems match
st_crs(points) == st_crs(msoa)

epc_joined <- st_join(points, msoa, join = st_within)

# fuel poverty proxy will mark a home as fuel poor if current energy efficiency
# is less than 55 and heating cost current is 75th percentile or higher. 
heating_75th <- quantile(epc_joined$`heating-cost-current`, 0.75, na.rm = TRUE)

msoa_fuelpov <- epc_joined %>%
  st_drop_geometry() %>%
  mutate(fuel_poor_proxy = case_when(
             `current-energy-efficiency` < 55 & `heating-cost-current` >= heating_75th ~ TRUE,
             TRUE ~ FALSE)) %>%
  group_by(MSOA21CD, msoa21hclnm) %>%
  summarise(
    n_properties = n(),
    n_fuel_poor = sum(fuel_poor_proxy, na.rm = TRUE),
    pct_fuel_poor = n_fuel_poor / n_properties * 100,
    mean_efficiency = mean(`current-energy-efficiency`, na.rm = TRUE),
    mean_heating_cost = mean(`heating-cost-current`, na.rm = TRUE),
    .groups = "drop")

cambs_msoa_fuelpov <- msoa %>%
  left_join(msoa_fuelpov, by = c("MSOA21CD", "msoa21hclnm"))

# Key Cambridgeshire settlements as named points
places <- data.frame(
  name = c("Cambridge", "Peterborough", "Ely", "Huntingdon", 
           "March", "Wisbech", "St Ives", "St Neots"),
  lon  = c( 0.1192,  -0.2508,  0.2622,  -0.1826, 
            0.0880,   0.1594, -0.0776,  -0.2644),
  lat  = c(52.2053, 52.5739, 52.3981, 52.3364, 
           52.5516, 52.6662, 52.3226, 52.2284)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(cambs_msoa_fuelpov))

ggplot() +
  geom_sf(data = cambs_msoa_fuelpov, 
          aes(fill = pct_fuel_poor), 
          colour = "white", linewidth = 0.2) +
  geom_sf(data = places, size = 1.5, colour = "black") +
  geom_text_repel(data = places |>
                    mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2]) |> 
      st_drop_geometry(), 
      aes(x = x, y = y, label = name), size = 3, fontface = "bold", box.padding = 0.5) +
  scale_fill_gradient2(low = "#1D9E75", mid = "#FAC775", high = "#E24B4A",
                       midpoint = 15, na.value = "grey90", name = "Fuel poverty (%)") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = unit(6, "cm"),
                               barheight = unit(0.5, "cm"), ticks.colour = "grey30",
                               frame.colour = "grey30")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        legend.margin = margin(t = 5),
        legend.box.margin = margin(t = 10)) 