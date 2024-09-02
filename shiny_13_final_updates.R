library(bcrypt)
library(shiny)
library(shinyjs)
library(DT)
library(openxlsx)
library(dplyr)
library(tidyverse)
library(stringdist)
library(fuzzyjoin)
library(rhandsontable)
library(data.table)
library(ids)
library(httr)
library(xml2)
library(curl)
library(tidytext)
library(utilityR)

# library(mapview)
# library(webshot)

source('.Rprofile')
source("www/src/STX_Utils_DB_app.R")

# to do -----------

req_variables <- c('q.type','sector','name','label::English','list_name','datasheet')


# webshot::install_phantomjs(force = T)
options(shiny.maxRequestSize = 30 * 1024 ^ 2,
        rsconnect.max.bundle.files = 5145728000)
options(shiny.sanitize.errors = TRUE)

# map setup


# Python Setup 

virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
python_path = Sys.getenv('PYTHON_PATH')

# Create virtual env and install dependencies

if(!virtualenv_dir %in% reticulate::virtualenv_list()){
  reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
}

# install packages
# numpy_av <-  reticulate::py_module_available("numpy")
# pandas_av <- reticulate::py_module_available("pandas")
# spacy_av <- reticulate::py_module_available("spacy")

# if(!(pandas_av&spacy_av)){
  reticulate::virtualenv_install(virtualenv_dir,
                                 packages = c("-r", "www/requirements.txt"),
                                 ignore_installed=TRUE)
# }

# check if modules are available, install only if needed
reticulate::use_virtualenv(virtualenv_dir, required = T)
model_av <- reticulate::py_module_available("en_core_web_md")

#install the language model
if(!model_av){
  system("python -c \"import spacy; spacy.cli.download('en_core_web_md')\"")
}

if(!exists('similarity_calculator')){
  reticulate::source_python('www/src/semantic_match.py')
}



# end--------------------------------------


# --------------- Shiny itself ---------------------

# set up the UI
ui <- function(req) { fluidPage(
  useShinyjs(),
  titlePanel("Interactive QNR database: IMPACT Ukraine"),
  # add the possibility of an overflow of dropdown options for the table with table-container ID
  tags$head(
    # tags$script(
    #   # this thing allows us to update the app when a certain button is clicked
    #   HTML("
    #        function doReload(tab_index) {
    #           let loc = window.location;
    #           let params = new URLSearchParams(loc.search);
    #           params.set('tab_index', tab_index);
    #           loc.replace(loc.origin + loc.pathname + '?' + params.toString());
    #        }"), type ="text/javascript"
    # ),
    tags$style(HTML(
      "
        #table-container {
          overflow: visible !important;
        }"
    )),
    HTML(
      '<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"/>'
    ),
    includeCSS("www/style.css"),
    HTML(
      '<a style="padding-left:10px;" class="app-title" href= "https://www.reach-initiative.org/" target="_blank"><img src="reach.jpg" height = "50"></a><span class="app-description" style="font-size: 16px; color: #FFFFFF"><strong>Database_test</strong></span>'
    ),
    #   ,
    #   tags$script("
    #   $(document).ready(function() {    
    #     setTimeout(function() {
    # 
    #       var map = $('#country_choice').data('leaflet-map');            
    #       function disableZoom(e) {map.scrollWheelZoom.disable();}
    # 
    #       $(document).on('mousemove', '*', disableZoom);
    # 
    #       map.on('click', function() {
    #         $(document).off('mousemove', '*', disableZoom);
    #         map.scrollWheelZoom.enable();
    #       });
    #     }, 100);
    #   })
    # "),
    # tags$script('
    #                     var dimension = [0, 0];
    #                     $(document).on("shiny:connected", function(e) {
    #                     dimension[0] = document.getElementById("map").clientWidth;
    #                     dimension[1] = document.getElementById("map").clientHeight;
    #                     Shiny.onInputChange("dimension", dimension);
    #                     });
    #                     $(window).resize(function(e) {
    #                     dimension[0] = document.getElementById("map").clientWidth;
    #                     dimension[1] = document.getElementById("map").clientHeight;
    #                     Shiny.onInputChange("dimension", dimension);
    #                     });
    #                     ')
  ),
  hr(),
  tabsetPanel(id = 'Tabpanel',
              tabPanel('Read me',
                       column(12,
                              h3("General information"),
                              span(htmlOutput("text_row1"), style = "font-size:20px;")
                       ),
                       column(6,
                              h3("Preparing the input file"),
                              span(htmlOutput("text_column1"), style = "font-size:20px;")
                       ),
                       column(6,
                              h3("The matching process"),
                              span(htmlOutput("text_column2"), style = "font-size:20px;")
                       ),
                       column(12,
                              h3("Why should I do this?"),
                              span(htmlOutput("text_row2"), style = "font-size:20px;")
                       ),
              ),
              tabPanel("Available data",
                       sidebarLayout(
                         sidebarPanel(
                           span("What this is", style = "font-weight: bold;"),
                           div(style = "height: 20px;"),
                           # Empty space with CSS styling
                           span(
                             "This page provides a brief description of all available data in the database.
               You can see the projects uploaded to the database as well as the number of questions in each project."
                           ),
                           div(style = "height: 20px;"),
                           # Empty space with CSS styling
                           width = 3
                         ),
                         mainPanel("", DT::dataTableOutput('table2'))
                       )),
              tabPanel("Data uploader",
                       sidebarLayout(
                         sidebarPanel(
                           textInput('Password_input','Input the access password'),
                           fileInput('Workflow_table', 'Choose Excel File', accept = c('.xlsx', '.xls')),
                           tags$hr(style = "border: 1px solid black;"),
                           span("What is the ID of this project?",
                                style = "font-weight: bold;"),
                           div(style = "height: 20px;"),
                           uiOutput('newNameInput'),
                           div(style = "height: 10px;"),
                           selectInput(
                             'round_id',
                             'What round of the assessment are you uploading?',
                             choices = c("",1:30),
                             selected = ''
                           ),
                           div(style = "height: 10px;"),
                           # Button to trigger table creation
                           selectInput(
                             'newType',
                             'Was this a household or individual assessment?',
                             choices = c("",'Households', 'Individual','KeyInformants' ,'Customers', 'Retailers'),
                             selected = ""
                           ),
                           div(style = "height: 20px;"),
                           span(
                             "Once you've uploaded the kobo tool, click this button to build the table",
                             style = "font-weight: bold;"
                           ),
                           div(style = "height: 10px;"),
                           # Empty space with CSS styling
                           actionButton("buildTables", "Build Tables"),
                           tags$hr(style = "border: 1px solid black;"),
                           div(style = "height: 10px;"),
                           # Empty space with CSS styling
                           span("Apply changes to the Database", style = "font-weight: bold;"),
                           div(style = "height: 10px;"),
                           # Empty space with CSS styling
                           actionButton("saveButton", "Save Table"),
                           width = 3
                         ),
                         mainPanel(# Add the table ID so that we can add overflow to it
                           div(
                             # Add the table ID so that we can add overflow to it
                             div(id = "table-container",
                                 rHandsontableOutput("final_table"))
                           ))
                       ))
  )
)
}

server <- function(input, output, session) {
  
  # text block
  output$text_row1 <- renderText({
    "
    The app allows the user to systematize the KOBO forms used within their organization. 
    The questions of each new research cycle are matched with what already exists in the database and allows to understand the 
    external connections across research cycles.
    The following Readme goes through the proccess of uploading your tools into the database.<br>
    The <b>Available data</b> tab describes the data already uploaded into the database
    The <b>Data uploader</b> tab allows the user to load their data into the database.
    "
  })
  
  output$text_column1 <- renderText({
    "
    Prior to the uploading process the user has to prepare the Kobo form <b>(NOT DAP)</b>. 
    The only manual input into the form that is needed from the side of the user is creating and filling the <b>sector</b> column 
    in the <b>survey</b> sheet of the kobo form. 
    This column has to describe what sector does each question in the survey correspond to.<br>
    This sector should be one of the following:
    <ul>
   <li>WASH</li>
   <li>AAP</li>
   <li>CCCM</li>
   <li>Shelter and NFI</li>
   <li>Education</li>
   <li>Food Security and Livelihoods</li>
   <li>Health</li>
   <li>Displacement</li>
   <li>Protection</li>
   <li>Cash and Markets</li>
   <li>Nutrition</li>
   <li>Emergency Telecommunications</li>
   <li>Logistics</li>
   <li>Social cohesion</li>
   <li>Government services</li>
   <li>Winterization</li>
   <li>Interview component</li>
   <li>Demographics</li>
   </ul>
If the sector in your KOBO tool doesn't match one of these exactly, it will be left blank in the Final QDB, 
which is fine for Demographic variables and general ones that are needed for backend KOBO functionalities. 
The input kobo file's survey sheet has to have the following columns:
    <ul>
   <li>Sector</li>
   <li>type</li>
   <li>name</li>
   <li>label::English</li>
   <li>label::Ukrainian</li>
   <li>label::Russian</li>
   </ul>
   Other columns are allowed but not necessary
    "
  })
  
  output$text_column2 <- renderText({
    "
    To begin the matching process click the <b>Data uploader</b> and input the access password 
    (Ask the package mantainers for it if you don't already have it).
    After inputting the password click <b>Browse</b> button and select your kobo tool.<br>
    Input the Project's ID, round and the type of respondents that participated in the survey you're uploading. 
    If the type of the survey you're uploading is not present in the list, please contact the package mantainers.<br>
    Once you're done inputting the information, click <b>Build tables</b> button. This will start the matching process that will
    try to find semantical similarities between the questions you're uploading and the ones already present in the database. 
    Currently the algorithm classifies the matches into 3 categories + the final category that is user defined:
    <ol>
   <li><b>Matched questions</b> - cases of a confident match between the loaded and database data, no action from user is required</li>
   <li><b>New questions</b> - cases of a confident non-match between the loaded and database data, no action from user is required</li>
   <li><b>Uncertain cases</b> - cases where a degree of semantic similarity was found in the database, but we cannot be certain that the 
   match is perfect. These questions will have an empty cell that the user can click. Once clicked the user will see possible matches 
   for the question, if one of the matches works as an alternative way of asking the uploaded question, the user can select it. If none
   of the matches work for the user, they can leave the cell blank (the 4th class of matching) or define it as new</li>
   <li><b>Blank entries</b> - These occur during the user's work with the 3rd class of matching. If none of the options in the cell
   are close enough for the user to select and the user doesn't want the question to be present in the database, they can't leave the cell
   blank and leave it out of the uploading.</li>
   </ol><br>
   When the user is finished with matching the questions they can click the <b>Save table</b> button and load the data into the database.
    "
  })
  
  output$text_row2 <- renderText({
    "
    This process allows us to systematize the tools within the Ukraine mission and understand cross-research cycle relationshipts.
    It allows one to look at the situation in Ukraine as an evolving continious process and see the interconnectedness of different research cycles
    This tool is part of a larger effort to systematize all of the data within the Ukraine mission, if you'd like to know more, please check the folling:<br>
    To browse the Questionnaire database and explore these relationships between these questions, please refer to the <a href='https://impact-initiatives.shinyapps.io/QDB_browser/'>QDB_browser tool</a><br>
    To browse the etire Reach database and see the data for each research cycle, please refer to the <a href='https://impact-initiatives.shinyapps.io/Reach_DB_browser/'>Reach_DB browser tool</a><br>
    If you have any questions or bug reports, please contact the app maintainers:<br>
    nestor.cheryba@reach-initiative.org<br>
    bohdan.pelekh@reach-initiative.org<br>
    "
  })
  
  
  # Block where we get our data from the server
  
  
  source('www/src/get_db.R')
  database_research_cycle <- dbGetQuery(my_connection , "SELECT * from Research_cycle_db")
  
  # available data block -----------------------------------------------------------
  
  project_table <- reactive({
    database_proj_dist <- dbGetQuery(my_connection , "SELECT DISTINCT project_ID, round_ID from Reach_QDB")
    database_proj_count<- dbGetQuery(my_connection , "SELECT project_ID, COUNT(*) as N_questions from Reach_QDB GROUP BY project_ID")
    dbDisconnect(my_connection)
    if(nrow(database_proj_dist)>0){
      
      database_proj_rounds <- database_proj_dist %>% 
        mutate(round_ID = as.numeric(round_ID)) %>% 
        group_by(project_ID) %>% 
        arrange(project_ID,round_ID) %>% 
        summarise(rounds = numbers_to_string(round_ID)) %>% 
        ungroup()
      
      database_proj_count %>% 
        inner_join(database_research_cycle %>% 
                     rename(project_ID=Research_cycle_ID)) %>% 
        inner_join(database_proj_rounds) %>% 
        relocate(project_ID,Name,rounds,N_questions)
    }else{data.frame()}
    
  })
  
  output$table2 <- DT::renderDataTable({
    DT::datatable(project_table())
  })
  
  
  # clickable table block --------------------------------------------------------------
  
  dataReady <-
    reactiveVal(FALSE)  # Reactive value to track if button is clicked
  
  label_present <- reactiveVal(FALSE)
  
  # check if colnames are correct 
  
  observeEvent(input$Workflow_table,{
    if(input$Password_input==Sys.getenv('pas')){
      inFile <- input$Workflow_table
      if(!is.null(inFile)){
        file <- read.xlsx(inFile$datapath, sheet='survey')
        col_labels <- names(file)
        label_present('label::English'%in% col_labels)
        
        # error message if the user has missed the correct label column
        
        if(label_present()==F){
          
          showModal(
            modalDialog(
              title = "Wrong label colnames",
              "Your tool's label columns don't follow the established convention. 
            They have to be named label::Language (e.g label::English, label:Ukrainian, etc.) for the app to run",
              footer = NULL,
              easyClose = TRUE
            )
          )
          output$final_table <- NULL
        }
      }
    }else{
      output$final_table <- NULL
      showModal(
        modalDialog(
          title = "Wrong password entered",
          paste0('Please enter the correct password'),
          footer = NULL,
          easyClose = TRUE
        ))
    }
  })
  
  # upload your data ---------------------------
  
  myData1 <- reactive({
    inFile <- input$Workflow_table
    
    if (is.null(inFile) | label_present()==F)
      # Only read data if button is clicked
      return(NULL)
    
    data <- utilityR::load.tool.survey(inFile$datapath,label_colname = 'label::English')
    # testo_2<<-data
    #filter out rows with columns we don't need
    data <- data %>%
      mutate(name = gsub('\\.','-',name)) %>% 
      filter(
        !grepl(
          'enum|partner|rectangle|geo_location|geopoint|street_type_calc|
                     name_uid_list|site_name_text|site_name|consent',
          name
        )
      )
    
    if('Sector' %in% names(data)){
      names(data)[which(names(data)%in%'Sector')] <- 'sector'
    }
    
    # rename the label column, filter the needed types and get the needed columns
    data <- select(data, any_of(req_variables)) %>%
      rename(label_english = `label::English`)%>%
      filter(q.type %in% c('select_one', 'select_multiple', 'integer', 'decimal')) %>% 
      filter(!is.na(label_english))
    
    names(data) <- tolower(names(data))
    
    
    if(!'sector' %in% names(data)){
      showModal(
        modalDialog(
          title = "Missing columns",
          "This file has no 'sector' column, please add it and try again",
          footer = NULL,
          easyClose = TRUE
        )
      )
      
      output$final_table <- NULL
      
    }
    # check if the correct sector names were assigned
    if('sector' %in% names(data) & 
       any(! (unique((data$sector)) %in% c('WASH','AAP','CCCM','Shelter and NFI','Education',
                                           'Food Security and Livelihoods','Winterization','Health','Displacement',
                                           'Cash and Markets','Government services','Social cohesion',
                                           'Protection','Nutrition','Emergency Telecommunications',
                                           'Logistics','Interview component','Demographics'))
       )
    ){
      # get the list of the wrong sectors
      wrong_sectors <- setdiff(
        unique(data$sector), c('WASH','AAP','CCCM','Shelter and NFI','Education',
                               'Food Security and Livelihoods','Winterization','Health','Displacement',
                               'Cash and Markets','Government services','Social cohesion',
                               'Protection','Nutrition','Emergency Telecommunications',
                               'Logistics','Interview component','Demographics', NA)
      )
      # drop them from the dataframe
      data <- data %>% 
        mutate(sector = ifelse(sector %in% wrong_sectors, NA, sector))

      # if all are na then stop
      if (all(is.na(data$sector))){
        showModal(
          modalDialog(
            title = "Wrong sectors entered",
            paste0("All of the sectors you've entered into your file are NA or are not in the list. Please try again"),
            footer = NULL,
            easyClose = TRUE
          )
        )
        
        output$final_table <- NULL
        
      }else{
        removeModal()
        # if only a few were wrong
        showModal(
          modalDialog(
            title = "Processing: Wrong sectors entered",
            paste0("The file uploaded has the wrong sector names entered, the questions with 
                 following sector names will be assigned a blank sector: ", paste0(wrong_sectors, collapse=', ')),
            footer = NULL,
            easyClose = TRUE
          )
        )
      }
      
    }
    
    if('sector' %in% names(data)){
      data <- data %>% 
        select(sector,name, label_english, list_name, q.type, datasheet) %>%
        mutate(sector = gsub('\\&|\\&amp;','and',sector)) %>% 
        distinct() 
      data
    }
    
  })
  
  sector_selected <- reactive({
    req(myData1())
    
    data <- myData1()
    # test2<<-data
    # data<-test2
    if(all(is.na(data$sector)) || 
       !('sector' %in% names(data))){
      return(TRUE)
    }else{return(FALSE)}
  })
  
  
  
  # get the list of survey IDs for the user to select
  # survey id
  assessment_value <- reactive({
    txt <- unique(database_research_cycle$Research_cycle_ID)
    txt
    
  })
  
  # Render the text input field with the default value
  output$newNameInput <- renderUI({
    choices <- assessment_value() 
    selectInput("newName",
                "Select an option:",
                choices = c('',choices),
                selected = '')
  })
  
  
  # clean your data ---------------------------
  
  
  dataReady <- reactiveVal(FALSE)  # Reactive value to track if button is clicked
  
  observeEvent(input$buildTables, {
    inputs <- c(input$newName,
                input$round_id,
                input$newType)
    if(any(inputs %in% '')){
      showModal(
        modalDialog(
          title = "Error",
          'Some of your inputs (project_id, round_id, survey_type) are empty. Fix and try again',
          footer = NULL,
          easyClose = TRUE
        )
      )
      dataReady(FALSE)
    }else{
      dataReady(TRUE)
    } # Set the reactive value to TRUE when button is clicked
  })
  
  
  database_proj <- reactive({
    req(myData1())
    source('www/src/get_db.R')
    dbGetQuery(my_connection , "SELECT * from Reach_QDB")
  })
  
  new_input_frame <- reactive({

    data <- myData1()   # Access the uploaded dataset from myData1 reactive object
    # print(0)
    # print(head(data))
    # print(dataReady())
    # Check if data is NULL
    if (is.null(data) || !dataReady() || sector_selected()){ 
      return(NULL)
    }
    #Cleaning the frame
    
    showModal(
      modalDialog(
        title = "Processing the data",
        'The uploaded KOBO tool is getting processed',
        footer = NULL,
        easyClose = TRUE
      )
    )
    new_input <-
      column_cleaner(data, name = 'name', label = 'label_english')
    
    
    # check if the project db has some of the questions
    
    updatedName <-
      input$newName # get the updated name of the survey
    
    round_id_survey <-
      input$round_id # get the updated round of the survey
    
    type_id_survey <- input$newType # get the type of the survey
    
    database_proj <- database_proj()
    
    
    if (nrow(database_proj) > 0) {
      # we'll only need two columns for comparison
      data_cl <-
        new_input %>% # we'll compare the table to what we have in the df using labels and project ID
        select(text2, q.type,datasheet,name,sector)
      
      # get the same two columns from the main DB
      database_proj_cl <-  database_proj %>%
        select(database_label_clean, q.type) %>%
        rename(text2 = database_label_clean)
      
      # get the differences between the DB that you're uploading and what's already in the proj DB
      data_cl_diff <-  data_cl %>%  anti_join(database_proj_cl)
      # reassign your clean data, depending on the setdiff result
      new_input <-
        new_input[new_input$text2 %in% data_cl_diff$text2,]
      
      # Get the project database without the project you're already uploading
      # we will filter out questions that have already been uploaded in other surveys
      database_proj_union <- database_proj %>% 
        filter(!(project_ID == updatedName &
                   round_ID == round_id_survey &
                   survey_type == type_id_survey)) %>%  # important, the comparison db has to omit the round that you're uploading
        select(database_label_clean, q.type,true_ID)%>% 
        rename(text2 = database_label_clean)
      
      
      # get the rows of the survey that the user is currently uploading (if such exist)
      # Get the project database with only the project you're already uploading - this will help filter out cases where 2 rounds have identical questions
      database_proj_intersection <- database_proj %>% 
        filter((project_ID == updatedName &
                  round_ID == round_id_survey &
                  survey_type == type_id_survey)) %>%  # important, the comparison db has to omit the round that you're uploading
        select(database_label_clean, q.type)%>% 
        rename(text2 = database_label_clean)
      
      
      data_cl_union <- data_cl %>% 
        semi_join(database_proj_union) %>%  # get the cases where the uploaded data matches with data outside Proj_id + round_id you're uploading
        mutate(true_ID = plyr::mapvalues(paste0(text2,q.type), 
                                         from=paste0(database_proj_union$text2,database_proj_union$q.type),
                                         to =database_proj_union$true_ID, warn_missing =F))
      
      data_cl_union <- data_cl_union %>%  anti_join(database_proj_intersection) # remove cases where the user has already uploaded data for this round into the DB
      if (nrow(data_cl_union)>0){
        
        # check this bit with CCCM data -----------------------------------
        # Now we're getting 
        matching_inputs <-  data_cl_union %>%  # take those questions that match between the database and what you're uploading (same Proj ID, different round)
          rename(database_label_clean=text2) %>% # keep only what you'll need and rename for easy matching
          mutate(round_ID =round_id_survey,
                 project_ID = updatedName,
                 survey_type = type_id_survey) # add the ID of the survey that you're working on
        
        # add the correct DB ID somewhere here ----------------------------
        
        
      }else{
        matching_inputs <- data.frame()
      }
      
      
    }else{
      matching_inputs <- data.frame()
    }
    split_data <- list(new_input,matching_inputs)
    split_data
    
  })
  
  myData0_clean <- reactive({
    req(new_input_frame())
    data <- new_input_frame()  # Access the uploaded dataset from myData1 reactive object
    
    if (is.null(data) || !dataReady())
      # Check if data is NULL
      return(NULL)
    
    new_input <- data[[1]] # as we've worked with a list of dataframes, we need to use the correct element of this list for future operations
    
    database_proj <- database_proj()
    
    # clean the comparison DB to not include questions from the project that the user is evaluating
    # create merger column
    
    stop_words <- stop_words %>%
      filter(!word %in% c(
        'one',
        'two',
        'three',
        'four',
        'five',
        'six',
        'seven',
        'eight',
        'nine',
        'area',
        'areas'
      ))
    
    database_clean <- database_proj %>%
      filter(!(project_ID == input$newName &
                 round_ID == input$round_id &
                 survey_type == input$newType)) %>% 
      mutate(merger_column = database_label_clean) %>%
      unnest_tokens(word, merger_column) %>%
      filter(!word %in% stop_words$word) %>%
      group_by(across(c(-word))) %>%
      summarize(merger_column = str_c(word, collapse = " ")) %>%
      ungroup() %>% 
      filter(str_count(database_label_clean, '\\w+')>=1) %>% 
      rename(name_DB=name )
    
    new_input <-  new_input %>%
      mutate(merger_column = text2) %>%
      unnest_tokens(word, merger_column) %>%
      filter(!word %in% stop_words$word) %>%
      group_by(across(c(-word))) %>%
      summarize(merger_column_new = str_c(word, collapse = " ")) %>%
      ungroup() %>% 
      filter(str_count(text2, '\\w+')>=1)
    # database_clean <<- database_clean
    # type_element<<-c('select_one', 'select_multiple')
    
    type_ls <-
      list(c('select_one', 'select_multiple'),
           c('integer', 'decimal')
           
           
      )
    # fuzzy matching to the project database
    fuzzy_final <-  data.frame()
    fuzzy_identical_final <- data.frame()
    for (type_element in type_ls) {
      new_input_type <- new_input[new_input$q.type %in% type_element,]
      database_clean_type <- database_clean[database_clean$q.type %in% type_element,]
      fuzzy_identical <- data.frame()
      match_id <- c()
      
      fuzzy_result_id <-  stringdist_left_join(
        new_input_type,
        database_clean_type %>%
          select(database_label_clean, name_DB, merger_column, true_ID),
        by = c("merger_column_new" = "merger_column"),
        method = "jaccard",
        q = 2,
        distance_col = "distance"
      ) 
      
      fuzzy_result_id <- fuzzy_result_id %>%
        filter(distance <= 0.1) %>%  # remove all observations that are too far away from each other
        group_by(text2) %>%
        arrange(distance) %>% 
        do(head(.,1)) %>% # and keep the top choice for each. At distance of 0.1 they can be considered the same
        ungroup()
      
      # remove matches from further process
      if(nrow(fuzzy_result_id)>0){
        new_input_type <- new_input_type %>% 
          anti_join(fuzzy_result_id %>% select(names(new_input_type)))
        
        # keep the identical entries and get their TRUE_ID
        fuzzy_result_id <- fuzzy_result_id %>% 
          select(sector,text2, true_ID,
                 q.type,name,database_label_clean,datasheet,list_name) %>% 
          select(-database_label_clean) %>% 
          rename(database_label_clean = text2) %>% 
          mutate(project_ID = input$newName,
                 round_ID = input$round_id,
                 survey_type = input$newType)
        
        fuzzy_identical <- rbind(fuzzy_identical,fuzzy_result_id)
      }
      
      # semantic matching for the rest ----------------------
      # new_input_test<<- new_input_type
      # type_element<<-c('integer', 'decimal')
      # database_clean_test<<-database_clean_type
      
      # if everything was matched with character matching, there's no point in running this
      if(nrow(new_input_type)>0){
        # new_input_test<<- new_input_type
        # database_clean_test<<-database_clean_type
        
        # call a python function that'll get me the semantic similarities. Run them on merger columns. Works better
        fuzzy_result <-  similarity_calculator(
          column_new = new_input_type %>% pull(merger_column_new) %>% unique(),
          qdb = database_clean_type %>% select(true_ID, merger_column) %>%  distinct(),
          sim_th = 0.89
        ) 
        # fuzzy_result<<-fuzzy_result
        # fuzzy_identical<<-fuzzy_identical
        # new_input<<- new_input
        # database_clean<<-database_clean
        
        if (nrow(fuzzy_result) > 0) {
          # in case multiple matches of the same True ID, choose the most similar one
          fuzzy_result <- fuzzy_result %>% 
            group_by(merger_column_new,true_ID) %>% 
            slice_max(similarity,n=1) %>% 
            ungroup()
          
          # get top 10 matches per question
          fuzzy_result <- fuzzy_result %>%
            group_by(merger_column_new) %>%
            arrange(desc(similarity)) %>% 
            do(head(.,10)) %>% # and keep the top choice for each. At distance of 0.1 they can be considered the same
            ungroup()
          
          # get semantically identical rows. We can later semi join them with the fuzzy_result table
          fuzzy_result_id2 <- fuzzy_result %>% 
            group_by(merger_column_new) %>%
            filter(similarity>=0.97) %>% 
            group_by(merger_column_new) %>% 
            do(head(.,1)) %>% 
            ungroup()
          
          
          # get the labels from the database + other variables from the new_input
          
          # fuzzy_result<<-fuzzy_result
          # database_clean_type<<-database_clean_type
          # new_input_type<<-new_input_type
          
          fuzzy_result <- fuzzy_result %>% 
            left_join(database_clean_type %>% 
                        select(true_ID, database_label_clean,merger_column) %>% distinct()) %>% 
            left_join(new_input_type %>%
                        select(sector,text2, label_english,
                               q.type,name,merger_column_new,list_name,datasheet))%>% 
            distinct() 
          
          
          # if there are any semantically identical matches - add them to the frame
          if(nrow(fuzzy_result_id2) >0){
            
            fuzzy_result_back <- fuzzy_result
            
            fuzzy_result <- fuzzy_result %>% filter(!merger_column_new %in% fuzzy_result_id2$merger_column_new)
            
            fuzzy_result_id2 <- fuzzy_result_back%>% 
              semi_join(fuzzy_result_id2) %>% 
              select(-c(merger_column,similarity)) %>% 
              select(-c(database_label_clean,label_english,merger_column_new)) %>% 
              rename(database_label_clean = text2) %>% 
              mutate(project_ID = input$newName,
                     round_ID = input$round_id,
                     survey_type = input$newType) %>%
              distinct()
            # testo1<<-fuzzy_identical
            # actual_res<<-fuzzy_result_id2
            fuzzy_identical <- rbind(fuzzy_identical,fuzzy_result_id2)
            
            match_id <- unique(fuzzy_identical[fuzzy_identical$q.type %in% type_element,]$database_label_clean)
            
          }else{match_id2 <- c()}
          # remove the columns that we don't need anymore
          fuzzy_result <- fuzzy_result %>% 
            filter(similarity <0.97) %>% 
            select(-c(merger_column,similarity)) %>% 
            tibble()
          
          # new_input_type<<-new_input_type
          # fuzzy_result<<-fuzzy_result
          # fuzzy_identical<<-fuzzy_identical
          # match_id<<-match_id
          # type_element<<-type_element
          
          # some labels didn't have any matches, so we have to add them back
          if (any(!new_input_type$text2 %in% 
                  c(unique(fuzzy_result[fuzzy_result$q.type %in% type_element,]$text2),match_id))) {
            missing_text <-
              new_input_type[!new_input_type$text2 %in% 
                               c(unique(fuzzy_result$text2),match_id), ]
            
            # set up an empty df for the missing questions
            missings_df <- setNames(data.frame(matrix(
              ncol = ncol(fuzzy_result),
              nrow = nrow(missing_text)
            )), colnames(fuzzy_result))
            # if there are no matches in the existing data, this entry is probably new
            missings_df$database_label_clean = 'new'
            
            # fill the empty df with the match-less questions
            missings_df[, names(missing_text)] = missing_text
            missings_df$true_ID = NA
            # bind them together
            #print('here2')
            fuzzy_result <- rbind(fuzzy_result, missings_df)
            #fuzzy_result<<-fuzzy_result
          }
        } else{
          fuzzy_result <- new_input_type %>%
            mutate(database_label_clean = 'new')
        }
        
        if(!"true_ID" %in% names(fuzzy_result)){
          fuzzy_result$true_ID <- NA
        }
        # fuzzy_result<<-fuzzy_result
        # fuzzy_final<<-fuzzy_final
        
      }else{fuzzy_result <- data.frame()}
      
      fuzzy_final <- rbind(fuzzy_result, fuzzy_final)
      fuzzy_identical_final <- rbind(fuzzy_identical, fuzzy_identical_final)
      
    }
    # Make pretty
    if(nrow(fuzzy_final)>0){
      fuzzy_final <- fuzzy_final %>%
        rename(
          upload_name = name,
          upload_label = label_english,
          upload_label_clean = text2,
          question_type = q.type
        ) %>%
        as.data.table() %>%
        distinct()
    }else{
      fuzzy_final <- data.frame() 
    }
    # set up the reference DB
    
    # Split into the final output + identical output
    
    split_data <- list(fuzzy_final,fuzzy_identical_final)
    split_data
    
  })
  
  # set up myData1_clean that only contains the data that was fuzzy matched
  
  myData1_clean <- reactive({
    data <-  myData0_clean()
    data <- data[[1]]
  })
  
  # set up the  data that the user will be clicking ---------------------------
  
  emptyDT <- reactive({
    data <- myData1_clean()
    if (is.null(data) || !dataReady())
      # Only read data if button is clicked
      return(NULL)
    if(nrow(data)>0){
      empty <- data %>%
        select(database_label_clean,
               upload_label_clean,
               upload_name,
               question_type) %>%
        mutate(database_label_clean = ifelse(database_label_clean %in% 'new', 'new', NA_character_)) %>%
        distinct() %>%
        arrange(!is.na(database_label_clean), database_label_clean)
    }else{empty = data.frame()}
    empty
  })
  
  save_needed <-
    reactiveVal(FALSE)  # Reactive value to track if button is clicked (will remove the table from view as soon as you click upload to db)
  
  observeEvent(input$buildTables, {
    save_needed(FALSE) # allow the table to be displayed
    
    
    data <- emptyDT()
    
    if(!sector_selected()){
      if(nrow(data)>0){
        cnt <-   sum(data$database_label_clean %in% 'new')
        showModal(
          modalDialog(
            title = "Data uploaded to the workspace",
            paste0(
              "Your kobo tool has ",
              nrow(data),
              ' questions. ',
              cnt,
              ' out of them',
              ifelse(cnt > 1, ' are new.', ' is new.')
            ),
            footer = NULL,
            easyClose = TRUE
          )
        )}else{
          showModal(
            modalDialog(
              title = "Data uploaded to the workspace",
              "All of the questions within your kobo tool were matched within QDB click Save Table to upload the results.",
              footer = NULL,
              easyClose = TRUE
            )
          )
        }
    }
  })
  
  # empty display and click tracking ---------------------------
  
  
  displayRV <-
    reactiveVal(NULL) # our dataset will go here when we upload it
  selectedRowRV <-
    reactiveVal(NULL) # a null value for the click action of the selected row
  
  # Display the data table for user to see ---------------------------
  
  observeEvent(input$buildTables, {
    cleaned_data <- emptyDT()  # Get the cleaned data
    if (!is.null(cleaned_data)) {
      # Perform any additional processing on the cleaned_data if needed
      displayRV(cleaned_data)  # Assign the cleaned data to displayRV
      dataReady(TRUE)  # Set the reactive value to TRUE when button is clicked
    }
  })
  
  
  # track changes that the user makes ---------------------------
  
  # triggers when there's a change in the final data. It inputs the changed data into the display.
  # this way we track the changes and display them on our dashboard
  observeEvent(input$final_table, {
    modifiedData <- hot_to_r(input$final_table)
    displayRV(modifiedData)
  })
  
  
  # Set up the dropdown lists and the table design ---------------------------
  
  output$final_table <- renderRHandsontable({
    if (is.null(myData1_clean()) || save_needed()) { # so if the button is clicked (or the df is null) the view will be empty
      return(NULL)  # Return an empty rhandsontable object if myData1_clean() is NULL
    }
    if(nrow(myData1_clean())==0){
      return(NULL)
    }
    
    n_row <- nrow(myData1_clean())
    # set up the visual parameters of the table you want to display, Lots of customization can be done
    rhandsontableObj <-
      rhandsontable(
        displayRV(),
        rowHeaders = NULL,
        stretchH = "all",
        trimDropdown = FALSE,
        selectCallback = TRUE,
        maxRows = n_row,
        widisplayRVh = 2000,
        height = 1500,
        width = 1500,
        colWidths = c(200, 200, 100, 100),
        wordWrap = TRUE
      )
    # here we're adding the variables which will be used for subsetting (most of our DF in this case)
    # so take all possible combinations of your col variable given possible combinations in the myData1_clean
    rowOptionsDT <-
      myData1_clean()[selectedRowRV(), on = names(selectedRowRV())]
    # col is your clicked column. Select which ones you'd like to be clickable (customize as needed).
    for (col in 'database_label_clean') {
      rhandsontableObj <-
        hot_col(
          rhandsontableObj,
          col,
          allowInvalid = FALSE,
          # add a possibility of the clickable row/column
          type = "dropdown",
          # add the dropdown options
          source = unique(c(
            NA_character_, sort(unique(rowOptionsDT[[col]][!rowOptionsDT[[col]] %in% 'new'])), 'new'
          )),
          readOnly = TRUE
        ) %>%
        hot_cell(
          row = input$final_table_select$select$r,
          col = col,
          # allow the user to modify the cell they're clicking
          readOnly = FALSE
        )
    }
    rhandsontableObj
  })
  
  # set up a tracker for where exactly did the user click ------------------------
  
  #check for changes in the datatable and remove columns where all values are NA through lapply
  # this bit of code allows us to populate selectedRowRV value with the row of the table that we've clicked
  observeEvent(input$final_table_select$select$r, {
    # if there were any clicks in the displayed dataframe
    selectedRowDT <-
      hot_to_r(input$final_table)[input$final_table_select$select$r, ] # get the row that we've clicked and transform it into a data.frame format
    # check if at least 1 was clicked
    if (nrow(selectedRowDT) > 0) {
      selectedRowDT <-
        selectedRowDT[, which(unlist(lapply(selectedRowDT, function(x)
          ! all(is.na(x))))), with = FALSE] # drop NA rows
      #check that we still have >0 selected rows after removing NA
      if (nrow(selectedRowDT) > 0) {
        selectedRowRV(selectedRowDT) # updates selectedRowRV reactive value with the selected row data. Think of selectedRowRV as a function and we're giving it a parameter to estimate
      } else {
        selectedRowRV(NULL)
      }
    } else {
      selectedRowRV(NULL)
    }
  })
  
  # repeated questions_object
  
  repeated_df <- reactive({
    req(new_input_frame(), myData0_clean())
    repeated_qs <- new_input_frame()[[2]]
    repeated_qs_semantic <- myData0_clean()[[2]]
    if(nrow(repeated_qs_semantic)>0){
      repeated_qs_semantic <- repeated_qs_semantic%>%
        select(names(repeated_qs))
      repeated_qs <- rbind(repeated_qs,repeated_qs_semantic)
    }
    repeated_qs
  })
  
  
  # clean and save the final dataframe ------------------------
  
  # # Save the table when the button is clicked
  observeEvent(input$saveButton, {
    # get the merger_column for the final db
    data_for_merging <- myData1_clean() 
    
    if(nrow(data_for_merging)>0){
      # add new row per entry
      data_for_merging_new <- data_for_merging %>% 
        distinct_at(vars(-c(true_ID,database_label_clean)),.keep_all = T) %>% 
        group_by(across(-c(true_ID,database_label_clean))) %>% 
        filter(!sum(ifelse(any(database_label_clean=='new'),1,0))>0) %>%
        ungroup() %>% 
        mutate(true_ID = NA,
               database_label_clean = 'new')
      
      # merge together (this is done so that when we join data_for_merging with final_table, we don't miss anything)
      # this is for cases where the algos found a match but the match is not good enough for us
      
      data_for_merging <- rbind(data_for_merging,data_for_merging_new)
    }
    
    updatedName <-
      input$newName # get the updated name of the survey
    
    survey_type <-  input$newType # get the type of the survey
    
    round_id_var<- input$round_id
    
    req(displayRV())  # Add this line
    # get the final table and clean it
    final_frame <-  displayRV() # get the data
    
    # get the matching questions (Questions that were matched by being the same project but different round than what is uploaded)
    
    repeated_qs <-repeated_df()
    
    original_data <- myData1() 
    
    if(nrow(repeated_qs)>0){
      repeated_qs <- repeated_qs %>% left_join(original_data %>% select(name,list_name))
    }
    
    if(length(na.omit(final_frame$database_label_clean))>0 | nrow(repeated_qs)>0){
      
      if(length(na.omit(final_frame$database_label_clean))>0){
        
        # add the merger column to the final db
        final_frame <- final_frame %>% 
          inner_join(data_for_merging %>%
                       distinct())
        # assign true ID
        n_new <- nrow(final_frame[final_frame$database_label_clean=='new',])
        
        if('true_ID' %in% names(final_frame)){
          final_frame$true_ID <- as.character(final_frame$true_ID)
          final_frame[is.na(final_frame$true_ID),]$true_ID <- ids::uuid(n_new)
        }else{
          final_frame$true_ID <- ids::uuid(n_new)
        }
        
        # prepare the final version of the data
        main_questions_db <- final_frame %>%
          filter(!is.na(database_label_clean)) %>%  # delete entries that need to be deleted
          select(-database_label_clean,-merger_column_new,-upload_label ) %>% 
          rename(name = upload_name,
                 database_label_clean=upload_label_clean,
                 q.type = question_type)
        
        
        # Add Table_ID components
        Project_database <- main_questions_db %>%
          mutate(project_ID = updatedName,
                 survey_type = survey_type,
                 round_ID = round_id_var) 
        
        Project_database <- rbind(Project_database,repeated_qs)
        Project_database$upload_time <- Sys.time() 
        
      }else{
        repeated_qs$upload_time <- Sys.time() 
        Project_database <- repeated_qs
      }
      #testo<<-Project_database
      
      dbWriteTable(my_connection, 'Reach_QDB', Project_database,append=T)
      dbDisconnect(my_connection)
      
      print('My boy works the grill')
      
      # Optional: Provide a confirmation message
      showModal(
        modalDialog(
          title = "Table Saved",
          "The table has been saved into the QNR database.",
          footer = NULL,
          easyClose = TRUE
        )
      )
      
    }else{
      showModal(
        modalDialog(
          title = "Nothing to save",
          "All entries in the table you've just tried to upload are already present in QDB.",
          footer = NULL,
          easyClose = TRUE
        )
      )
      
    }
    
    # reset the view once the user clicks the save button
    save_needed(TRUE)
    
  })
  
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
