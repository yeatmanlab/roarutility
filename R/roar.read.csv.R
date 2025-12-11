roar.read.csv <- function(data_name, data_path, opt_out_name, opt_out_path){
  # Fix variable names
  data <- read.csv(file.path(data_path, data_name))
  opt.out <- read.csv(file.path(opt_out_path, opt_out_name))

  # Check which ID column exists and filter accordingly
  if("assessment_pid" %in% colnames(data)){
    data <- data %>% filter(!assessment_pid %in% opt.out$assessment_pid)
  } else if("pid" %in% colnames(data)){
    data <- data %>% filter(!pid %in% opt.out$assessment_pid)
  } else if("PID" %in% colnames(data)){
    data <- data %>% filter(!PID %in% opt.out$assessment_pid)
  } else if("user.assessmentPid" %in% colnames(data)){
    data <- data %>% filter(!user.assessmentPid %in% opt.out$assessment_pid)
  }

  # Return the cleaned data
  return(data)
}
