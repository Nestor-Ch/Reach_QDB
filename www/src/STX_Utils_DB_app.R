# utils for the app
options(shiny.maxRequestSize=30*1024^2)

# Define functions--------------------------------------------
# function that will be used for the round numbers in the research cycle table to make them pretty
numbers_to_string <- function(vec) {
  
  if (length(vec)==1){
    return(as.character(vec))
  }else{
    
    ranges <- c()
    start_range <- vec[1]
    for (i in 2:length(vec)) {
      if (vec[i] != vec[i - 1] + 1) {
        if (start_range != vec[i - 1]) {
          ranges <- append(ranges, paste(start_range, vec[i - 1], sep = "-"))
        } else {
          ranges <- append(ranges, as.character(start_range))
        }
        start_range <- vec[i]
      }
    }
    # Handle the last range
    if (start_range != vec[length(vec)]) {
      ranges <- append(ranges, paste(start_range, vec[length(vec)], sep = "-"))
    } else {
      ranges <- append(ranges, as.character(start_range))
    }
    result <- paste(ranges, collapse = ",")
    return(result)}
}
# define a function that cleans columns
column_cleaner <- function(df, name, label) {
  new_input = df %>%
    mutate(text2 = ifelse(
      str_detect(!!as.symbol(label), "^[0-9]"),
      sub(".*? ", "", (!!as.symbol(label))),!!as.symbol(label)
    )) %>%
    mutate(
      text2 = tolower(text2),
      text2 = str_squish(text2)
    ) %>% 
    unnest_tokens(word, text2) %>%
    filter(!grepl("^[a-z]+[._]?\\d+$", word)) %>%
    filter(!grepl("\\d+[._][a-zA-Z]{3}", word)) %>%
    filter(!grepl("\\d+[._]\\d+", word)) %>% 
    filter(! word %in% c(letters, LETTERS)) %>% 
    group_by(across(c(-word))) %>%
    summarize(text2 = str_c(word, collapse = " ")) %>%
    ungroup()
  
  return(new_input)
}

load.label_colname <- function(data, language = "English") {
  tool_colnames <- data  %>% names
  return(tool_colnames[agrep(paste0("label::", language), tool_colnames)])
}

# data downloader
sp_get_file_reach <- function (connection, rurl, destfile) {
  # A reworked version of script by https://github.com/Shaunson26/sharepointR
  
  if (grepl(connection$site$site, rurl)) {
    rurl <- sub(paste0("[\\/]?", connection$site$site, "[\\/]?"),
                "", rurl)
  }
  rurl = sub("^\\/?", "", rurl)
  url <-
    file.path(do.call(file.path, connection$site), URLencode(rurl))
  if (missing(destfile)) {
    destfile = basename(url)
  }
  
  
  
  temp_file <- tempfile(fileext = ".xlsx")
  
  response = httr::GET(
    url,
    httr::set_cookies(
      rtFa = connection$cookies$rtFa,
      FedAuth = connection$cookies$FedAuth
    ),
    config = connection$config,
    httr::write_disk(temp_file, overwrite = T)
  )
  
  data <- openxlsx::read.xlsx(temp_file)
  return(data)
  if (response$all_headers[[1]]$status == 200) {
    message("File successfully downloaded to \"", destfile,
            "\"")
  }
  else {
    stop("File failed downloaded.")
  }
  invisible(NULL)
  
  file.remove(temp_file)
  
}

# Data uploader

sp_post_file_reach <- function(connection, rurl, file, write_tool = F, work_book_name =NULL) {
  # A reworked version of script by https://github.com/Shaunson26/sharepointR
  
  file_env = gsub(".xlsx", '', file)

  # Save the data frame to an XLSX file in the temp folder
  temp_file <- tempfile(fileext = ".xlsx")
  
  # if the goal is to write the tool, the upload process is a bit different
  if(write_tool){
    openxlsx::saveWorkbook(work_book_name, temp_file, overwrite = TRUE)
    
  }else{
    openxlsx::write.xlsx(get(file_env), temp_file)
  }
  
  # Remove site URI if present
  if (grepl(connection$site$site, rurl)) {
    rurl <-
      sub(paste0('[\\/]?', connection$site$site, '[\\/]?'), '', rurl)
  }
  
  # Leading/trailing /
  rurl <- gsub('^\\/?|\\/?$', '', rurl)
  
  # Build URL
  re_rurl <-
    sprintf("GetFolderByServerRelativeUrl('%s')/files",
            URLencode(rurl))
  re_file <-
    sprintf("add(url='%s',overwrite=true)", URLencode(basename(file)))
  url <-
    file.path(do.call(file.path, connection$site),
              '_api/web',
              re_rurl,
              re_file)
  print(url)
  
  # Begin POST
  xrequestdigest <- httr::POST(
    url = url,
    httr::set_cookies(
      rtFa = connection$cookies$rtFa,
      FedAuth = connection$cookies$FedAuth
    ),
    config = connection$config
  )
  
  xrequestdigest <-
    httr::headers(xrequestdigest)$`x-requestdigest`[[1]]
  
  # POST to the 'server'
  response <- httr::POST(
    url = url,
    httr::set_cookies(
      rtFa = connection$cookies$rtFa,
      FedAuth = connection$cookies$FedAuth
    ),
    httr::add_headers(`x-requestdigest` = xrequestdigest),
    config = connection$config,
    body = httr::upload_file(temp_file)
  )
  
  # Outcome
  if (response$status_code != 200) {
    stop('Uploading file failed with status: ', response$status_code)
  } else {
    message('File successfully uploaded to: ',
            file.path(do.call(file.path, connection$site), rurl, basename(file)))
  }
  
  # Remove the temporary file
  file.remove(temp_file)
}

print('All functions have been added')


# Define the Sharepoint connection --------------------------------------------------
sp_con <-
  sp_connection(
    site = 'https://acted-my.sharepoint.com/personal/nestor_cheryba_reach-initiative_org', # replace with your address
    username = 'nestor.cheryba@reach-initiative.org', #replace with your username
    password = "Par1s1sBurning", # your password
    get_config = T
  )

print('Sharepoint connection established')



