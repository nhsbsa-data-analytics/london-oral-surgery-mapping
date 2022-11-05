library(rgdal)
library(dplyr)
library(tidyr)
library(glue)
library(lubridate)
library(stringr)
library(rlang)


# Data --------------------------------------------------------------------

input_path <- "data-raw"

# GIS data downloaded from 
# http://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
boroughs <- readOGR(
  dsn = "Data/statistical-gis-boundaries-london/ESRI",
  layer = "London_Borough_Excluding_MHW",
  verbose = FALSE
) %>% suppressWarnings()

# Keep only the borough name column
boroughs@data <- boroughs@data %>%
  transmute(borough = NAME)

# Transform to GPS reference system
boroughs <- boroughs %>% spTransform(CRS("+init=epsg:4326"))

# Find the edges of our map
bounds <- boroughs@bbox

# Get the contractor data and survey responses
endo_contractors <- read.csv(
  file.path(input_path, "endodontics_contractors.csv"), fileEncoding = "UTF-8-BOM"
)
imos_contractors <- read.csv(
  file.path(input_path, "imos_contractors.csv"), fileEncoding = "UTF-8-BOM"
)
endo_data <- read.csv(
  file.path(input_path, "endodontics_data.csv"), fileEncoding = "UTF-8-BOM"
)
imos_data <- read.csv(
  file.path(input_path, "imos_data.csv"), fileEncoding = "UTF-8-BOM"
)

max_boroughs <- function(df) {
  df %>%
    pull(Borough) %>%
    lapply(str_split, pattern = ";") %>%
    squash() %>%
    lengths() %>%
    max()
}

max_endo_boroughs <- max_boroughs(endo_contractors)
max_imos_boroughs <- max_boroughs(imos_contractors)

# A contractor is uniquely identified by their Lot Number. Each Lot Number also
# corresponds with one or more London Boroughs, with no overlap between Lot Numbers
# with the exception of Croydon for IMOS.
# Each month, each contractor provides data which is the OVERALL numbers for their
# lot (i.e. one or more Boroughs). Where a lot covers multiple Boroughs, the values
# that represent counts need to be spread evenly across these boroughs. For values
# that represent wait times, each of the boroughs will take this value (except in 
# cases like Croydon IMOS where multiple lot numbers (equiv. contractors) operate).
prepare_data <- function(service) {
  .data         <- glue("{service}_data")
  .contractors  <- glue("{service}_contractors")
  .max_boroughs <- glue("max_{service}_boroughs")
  
  get(.data) %>% 
    mutate(LotNum = as.character(LotNum)) %>% 
    left_join(
      get(.contractors) %>% mutate(Lot.Number = as.character(Lot.Number)),
      by = c("LotNum" = "Lot.Number")
    ) %>%
    transmute(
      month = floor_date(dmy(ID.endDate), unit = "month"),
      lotnum = as.character(LotNum),
      borough = Borough,
      service = Service,
      total_refs_in =  !!sym(names(select(., starts_with("Q1a.")))),
      total_refs_out = !!sym(names(select(., starts_with("Q1c.")))),
      total_treated =  !!sym(names(select(., starts_with("Q2a.")))),
      total_waiting =  !!sym(names(select(., starts_with("Q3a.")))),
      avg_wait_non_urgent = !!sym(names(select(., starts_with("Q4a."))))
    ) %>% 
    separate(
      borough,
      glue("borough_{1:get(.max_boroughs)}"),
      sep = ";",
      fill = "right"
    ) %>% 
    pivot_longer(starts_with("borough_"), values_to = "borough") %>% 
    select(-name) %>% 
    drop_na(borough) %>% 
    group_by(month, lotnum) %>% 
    add_count() %>% 
    mutate(
      total_refs_in = total_refs_in / n,
      total_refs_out = total_refs_out / n,
      total_treated = total_treated / n,
      total_waiting = total_waiting / n
    ) %>%
    select(-n) %>% 
    select(month, lotnum, borough, everything())
}

endo <- prepare_data("endo")
imos <- prepare_data("imos")

# Create the labels for the map tool tips
create_label <- function(borough, lotnum, total_refs_in, total_refs_out, 
                         total_treated, total_waiting, avg_wait_non_urgent) {
  args <- as.list(environment())
  
  if_else(
    !all(is.na(args[-c(1, 2)])), # Will be TRUE when ALL of the arguments except 
                                 # the first 2 are NA
    glue("
          <strong>Borough: </strong>{borough}<br>
          <strong>Lot: </strong>{lotnum}<br>
          <strong>Referrals in: </strong>{total_refs_in} patients<br>
          <strong>Referrals out: </strong>{total_refs_out} patients<br>
          <strong>Patients treated: </strong>{total_treated} patients<br>
          <strong>Patients waiting: </strong>{total_waiting} patients<br>
          <strong>Avg. wait (non-urgent): </strong>{avg_wait_non_urgent} weeks<br>
        "),
    glue("
          <strong>Borough: </strong>{borough}<br>
          No data available
        ")
  )
}

# For multi-contractor boroughs need to condense the lot numbers and values to
# one row before adding labels
data <- bind_rows(endo, imos) %>%
  group_by(month, borough, service) %>%
  summarise(
    lotnum = toString(lotnum),
    total_refs_in = sum(total_refs_in, na.rm = TRUE) %>% round(),
    total_refs_out = sum(total_refs_out, na.rm = TRUE) %>% round(),
    total_treated = sum(total_treated, na.rm = TRUE) %>% round(),
    total_waiting = sum(total_waiting, na.rm = TRUE) %>% round(),
    avg_wait_non_urgent = mean(avg_wait_non_urgent, na.rm = TRUE) %>% round(2),
    .groups = "drop"
  ) %>%
  complete(nesting(borough, service), month) %>% 
  rowwise() %>% 
  mutate(
    label = create_label(
      borough, 
      lotnum,
      total_refs_in,
      total_refs_out,
      total_treated,
      total_waiting,
      avg_wait_non_urgent
    )
  ) %>% 
  as.data.frame() # remove rowwise class

months <- sort(unique(data$month), decreasing = TRUE) %m-% months(1)
names(months) <- format(months, "%b %y")

saveRDS(boroughs, "data/boroughs.rds")
saveRDS(bounds, "data/bounds.rds")
saveRDS(data, "data/data.rds")
saveRDS(months, "data/months.rds")
