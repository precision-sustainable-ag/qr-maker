library(purrr)
library(dplyr)
library(googlesheets4)
library(googledrive)

# TODO, this isn't working non-interactively, need to use API token
# auth_token <- sheets_auth()
# saveRDS(auth_token, "auth_token.rds")

source("barcode_for_weeds.R")

# Remove "Data Validation" sheet at the end
sitelist <- 
  sheets_sheets(gs_key_for_site_enrollment) %>% 
  head(-1) %>% 
  set_names() %>% 
  map_dfr(
    ~sheets_read(gs_key_for_site_enrollment, sheet = .x) %>% 
      select(CODE = `CODE*`) %>% 
      filter(!is.na(CODE), nchar(CODE) == 3),
    .id = "state"
    ) %>% 
  mutate(
    state = stringr::str_remove_all(state, "[0-9]+"),
    state = stringr::str_trim(state),
    state_abb = set_names(state.abb, state.name)[state]
  )


strings_generator <- function(site_code, timing) {
  expand.grid(
    prefix = "W ",
    site = site_code,
    quadrat = 0:9,
    trt = c(" B", " C"),
    timing_code = paste0(" T", timing)
  ) %>% 
    mutate(labels = paste0(prefix, site, trt, quadrat, timing_code))
}


sheets_generator <- function(CODE, timing, fn, ...) {
  lbl_df <- strings_generator(CODE, timing)
  
  weed_sheets(lbl_df$labels, fn)
}


# check for paths locally, create if needed
paths <- unique(sitelist$state_abb)
local_paths <- file.path("temp", paths)

local_paths[!dir.exists(local_paths)] %>% 
  walk(dir.create, recursive = T)



labels_df <- 
  sitelist %>% 
  mutate(timing = list(1:3)) %>% 
  tidyr::unnest(timing) %>% 
  mutate(
    fn = file.path(
      "temp", state_abb, 
      paste0(CODE, " time ", timing, ".pdf")
      )
    )

pwalk(labels_df, sheets_generator)


barcode_dribble <- as_dribble(as_id(drive_key_for_pdfs))

drive_paths <- drive_ls(barcode_dribble)

drive_paths_new <- 
  paths[!(paths %in% drive_paths$name)] %>% 
  map_dfr(~drive_mkdir(.x, path = barcode_dribble))


drive_paths_full <- bind_rows(drive_paths, drive_paths_new)

existing_folders <- drive_ls(barcode_dribble)
existing_files <- 
  existing_folders$id %>% 
  map_dfr(~drive_ls(path = as_dribble(as_id(.x))))


qr_upload <- function(lbls_df, state_name) {
  id = drive_paths_full[drive_paths_full$name == state_name, ]$id 
  id = as_dribble(as_id(id))
  
  map_dfr(lbls_df$fn, ~drive_upload(.x, path = id))
}


drive_responses <- 
  labels_df %>% 
  filter(!(basename(fn) %in% existing_files$name)) %>% 
  split(.$state_abb) %>% 
  imap(qr_upload)

drive_responses %>% bind_rows(.id = "state")
