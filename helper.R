# SECRETS
source("secrets.R")
amion_dir <- amion_dir
googlesheet_url <- googlesheet_url
master_code <- master_code

# PATHS
schedules_path <- sprintf("%s/schedules", amion_dir)
changes_path <- sprintf("%s/changes/stpeds_changes.csv", amion_dir)


# PRE-PROCESSING

# Create DF of all archived schedules
available_schedules <- tibble(files = list.files(sprintf("%s/schedules", amion_dir), full.names = T)) %>%
  arrange(desc(files)) %>% 
  separate(files, into = c("schedules"), sep = "\\.", extra = "drop", remove = F) %>% 
  separate(schedules, into = c(NA, NA, "date", "time"), sep = "_") %>% 
  mutate(datetime = as.character(ymd_hms(paste(date, time, sep = "_"), tz = "America/Los_Angeles"))) %>% 
  mutate(datetime_string = paste(date, time, sep = "_")) %>% 
  arrange(datetime)

# Denote the first archived schedule
first_schedule <- available_schedules %>% head(1) %>% pull(datetime)
# Denote the most recent archived schedule
current_schedule <- available_schedules %>% tail(1) %>% pull(datetime)
# Vector of all available schedules to be used in widget options
all_schedules <- available_schedules %>% pull(datetime)

# Vector of all resident names
names <- read_rds(list.files(schedules_path, full.names = T)[1]) %>% 
  pull(Staff_Name) %>% 
  unique() %>% 
  sort()

# Vector of all time points when a change was documented
change_points <- read_csv(changes_path) %>% 
  mutate(change_detected = as.character(change_detected)) %>% 
  pull(change_detected) %>% 
  unique()

# Key that connects resident names to their submitted ID
name_key <- gsheet2tbl(googlesheet_url) %>% 
  select(names = `Name (don't change format)`, 
         email = `Stanford email`, 
         res_id = `Submit ID`,
         send_email = `Do you want email alerts of schedule changes?`) %>% 
  mutate(res_id = toupper(res_id)) %>% 
  right_join(tibble(names = names))
