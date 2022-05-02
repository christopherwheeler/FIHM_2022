# Official AIMS data pipeline for round 1 Konza STICs

source("tidy_hobo_data.R")
source("get_calibration.R")
source("apply_calibration.R") 
source("classify_wetdry.R") 

# subset string to just numeric part; use gsub to replace kona stics round 1 slash with just number

# step 1: get numeric part of sensor ID
# step 2: tidy hobo data
# step 3: getting and applying calibration (load in all the calibration points, 
#then subset to particular SN, then apply calibration)
# step 4: classify based on 50 PSC
# step 5: write all the files out into a folder

data_dir <- "konza_stics_round_1"

fs::dir_ls(data_dir)

length(data_dir)

stic_files <- fs::dir_ls(file.path(data_dir, "raw"), regexp = "\\.csv$")

# read in all calibration points
#  columns: date, logger_no, standard, conductivity_uncal

stic_calibrations <- read_csv("stic_calibration.csv")

for(i in 1:length(stic_files)) {
  # get information about file and sensor
  path_to_raw <- stic_files[i]
  logger_no <- 
    gsub(".csv", "", path_to_raw) %>%
    gsub(file.path(data_dir, "raw/"), "", .) %>%
    gsub("/", "", .)
  
  # tidy hobo data and save
  path_to_tidy <- file.path(data_dir, "tidy", paste0(logger_no, "_tidy.csv"))
  stic_data_tidy <- tidy_hobo_data(infile = stic_files[i], outfile = path_to_tidy)
  
  # get and apply calibration
  logger_calibration <- subset(stic_calibrations, logger_no == logger_no)
  calibration_fit <- get_calibration(logger_calibration)
  stic_data_calibrated <- apply_calibration(stic_data_tidy, calibration_fit)
  
  # classify wetdry and save
  stic_data_classified <- classify_wetdry(stic_data_calibrated, classify_var = "spc", threshold = 50)
  write_csv(stic_data_classified, file.path(data_dir, "classified", paste0(logger_no, "_classified.csv")))
  
  # status update
  print(paste0("Completed STIC # ", i, " of ", length(stic_files), " at ", Sys.time()))
  
}
