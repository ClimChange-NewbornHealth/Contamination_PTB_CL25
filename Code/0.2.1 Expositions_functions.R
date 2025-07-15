# 0.2.1 Weeks expositions functions ----

# Divide data in parts 

parts <- function(data, path, folder, num_parts = 20) {
  # Size parts
  part_size <- ceiling(nrow(data) / num_parts)
  
  # Process parts
  for (part_id in 1:num_parts) {
    # Parts
    part_data <- data[((part_id - 1) * part_size + 1):min(part_id * part_size, nrow(data)), ]
    
    # Save
    save(part_data, file=sprintf(paste0(path, folder, "/part_%02d_results.RData"), part_id))
  }
}

# Calculate contamination metrics

calculate_cont_stats <- function(row, cont_data) {
  # data.table objects
  setDT(row)
  setDT(cont_data)
  
  # Adjust dates
  row_copy <- copy(row)
  row_copy[, date_start_week := as.Date(date_start_week)]
  row_copy[, date_end_week := as.Date(date_end_week)]
  
  # Filter
  week_cont <- cont_data[date >= row_copy$date_start_week[1] & date < row_copy$date_end_week[1] & name_com == row_copy$name_com[1]]
  
  if (nrow(week_cont) == 0) {
    return(data.table(row_copy, 
                      pm25_krg_week=NA_real_, 
                      o3_krg_week=NA_real_, 
                      pm25_idw_week=NA_real_, 
                      o3_idw_week=NA_real_))
  } else {
    return(data.table(row_copy, 
                      pm25_krg_week = round(mean(as.numeric(week_cont$pm25_krg), na.rm = TRUE), 3),
                      o3_krg_week = round(mean(as.numeric(week_cont$o3_krg), na.rm = TRUE), 3),
                      pm25_idw_week = round(mean(as.numeric(week_cont$pm25_idw), na.rm = TRUE), 3),
                      o3_idw_week = round(mean(as.numeric(week_cont$o3_idw), na.rm = TRUE), 3)
                      )
                      )
  }
}

# Process data (old)

process_files <- function(input_directory, output_directory, cont_data, calc_func) {
  
  # Assure cont_data is available
  setDT(cont_data)
  
  files <- list.files(path = input_directory, full.names = TRUE, pattern = "\\.RData$")
  file_count <- 0
  
  for (file_path in files) {
    
    start <- Sys.time()
    
    file_count <- file_count + 1
    load(file_path)
    setDT(part_data)
    
    # Apply calculation function to each row as a data.table slice
    results <- part_data[, calc_func(.SD, cont_data), by = .I]
    
    # Save results
    save(results, file = file.path(output_directory, sprintf("%s_processed.RData", tools::file_path_sans_ext(basename(file_path)))))
    
    end <- Sys.time()
    cat("Time process data:", end-start, "\n") 
    
    if (file_count %% 5 == 0) {
      cat("Pause for 2 seconds to avoid overload...\n")
      Sys.sleep(2)
    }
    
  }
  
  cat("All files have been processed and saved in:", output_directory, "\n")
}

process_files_parallel <- function(input_directory,
                                   output_directory,
                                   cont_data,
                                   calc_func,
                                   workers = parallel::detectCores() - 4) {
  # data.table to fast
  setDT(cont_data)
  
  # List files RData
  files <- list.files(path       = input_directory,
                      pattern    = "\\.RData$",
                      full.names = TRUE)
  
  # Edit settings parallel
  plan(multisession, workers = workers)
  
  # Parallel process
  furrr::future_walk(
    .x = files,
    .f = function(file_path, cont_data, calc_func, output_directory) {
      # 1) Load .RData (define part_data)
      load(file_path)
      setDT(part_data)
      
      # 2) Apply functions
      results <- part_data[
        , calc_func(.SD, cont_data),
        by = .I
      ]
      
      # 3) Save results
      out_file <- file.path(
        output_directory,
        paste0(tools::file_path_sans_ext(basename(file_path)),
               "_processed.RData")
      )
      save(results, file=out_file)
    },
    # Object by worker
    cont_data        = cont_data,
    calc_func        = calc_func,
    output_directory = output_directory,
    .progress        = TRUE
  )
  
  # Volvemos al plan secuencial
  plan(sequential)
  
  beep(1)
  cat("✔ Todos los archivos procesados en paralelo y guardados en:\n", 
      output_directory, "\n")
}

# Post process load 
load_and_extract_df <- function(file_path) {
  e <- new.env()  # Enviroment to load data 
  load(file_path, envir = e)  # Load data
  # DF unique in enviroment 
  df <- e[[names(e)[1]]]  # First element in object 
  return(df)
}


# Optimal use power computation 
options(future.globals.maxSize = 3000 * 1024^2)  
plan(multisession, workers = detectCores() - 4) # Parallelization
