# load packages
library(dygraphs)
library(leaflet)
library(dplyr)
library(xts)

# download and prepare data
if (file.exists('bird.rds')) {
  # obtain data from file
  bird <- readRDS('bird.rds')
} else {
  # obtain data from web - warning takes an hour with decent internet connection
  library(spocc)
  limit <- 100000 # set lower for debugging
  bird <- occ(query='Apus pacificus', from='gbif',has_coords=TRUE, limit=limit,
      gbifopts=list(basisOfRecord='HUMAN_OBSERVATION', hasGeospatialIssue=FALSE, year='1990,2016'))$gbif$data[[1]] %>%
    mutate(label=paste0("Recorded by: ", recordedBy)) %>%
    select(name, longitude, latitude, eventDate, label) %>%
    rename(date=eventDate) %>%
    mutate(year_month = format(strptime(date, '%Y-%m-%d'), format='%Y-%m'))
  saveRDS(bird, file='bird.rds', compress='xz')
}

# calculate number of observations per year/month
bird.monthly <- bird %>%
  mutate(year_month = format(strptime(date, '%Y-%m-%d'), format='%Y-%m')) %>%
  group_by(year_month) %>%
  summarise(n_observations = n())
bird.monthly <- as.xts(bird.monthly$n_observations, order.by=as.Date(paste0(bird.monthly$year_month, '-01'), format='%Y-%m-%d'))


