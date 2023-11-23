# Reach_QDB
This repo is based on the Questionnaire database system I've developed within Reach Ukraine.

The app allows the user to systematize the KOBO forms used within their organization. The following Readme goes through the proccess of uploading your tools into the database, browsing the results and explains what's going on under the hood of the app

## Table of Contents
- [Database structure](#The-structure)
- [Uploading the data](#Data-uploading)
- [The matching process](#Matching-process)
  - [Semantic matching](#Semantic-matching)
  - [Setting up the Python virtual environment](#Setting-up-a-Python-virtual-environment-in-Shiny)


## The structure
The app is created to support the database structure of a 3 tables:
   - **The Research_cycle_tracker table** - The table consisting of 2 columns: 
     - **'Research_cycle_ID'** - column containing the project IDs (unique identifiers of each research cycle (but not of each round of research))
     - **'Name'** - column containing the project's full names/descriptions
This first table is created by the user and is one of the only manual inputs needed for the database to function. It serves as a descriptor, telling the user what exactly is conatined in the database at the moment

The following two tables are created by the user during the process of uploading the KOBO form:


   - **Project-database** - the main table of the database hosting all of the questions of all of the KOBO forms uploaded into the tool. The questions in this table are not unique, some of them are duplicated if the same question is present in multiple surveys. This dataframe consists of the following columns:
     - **'database_label_clean'** - The clean version of the question's english label
     - **'question_type'** - KOBO column 'type' describing whether the column is a 'select_one', 'select_multiple', 'decimal' or 'integer'
     - **'true_ID'** - The uuid of the unique question - This identifier is used to locate identical questions across different research cycle to allow intertemporal and inter-cycle comparison [Backend column]
     - **'DB_ID'** - The name of the question in the data - 'name' column in the KOBO form 
     - **'merger_column'** - a version of 'database_label_clean' with removed stopwords. Used during the process data uploading (see the 'Data uploading' section for more details) [Backend column]
     - **'Project_ID'** - The ID of the project. Has to be one of the 'Research_cycle_ID's from the 'Research_cycle_tracker' table
     - **'round_id'** - The number of the round of this research cycle
     - **'survey_type'** - Whether the survey was conducted on the Individual, household or settlement level
     - **'sector'** - What sector is the question from (e.g. Cash and Markets question, WASH question, Demographics question, etc.). The user needs to add this column to the 'survey' sheet of the KOBO form prior to uploading
     - **'label_english_choices'** - English labels of choices for each question
     - **'label_ukrainian'** - Ukrainian translation of the question
     - **'label_ukrainian_choices'** - Ukrainian labels of choices for each question
     - **'label_russian'** - russian translation of the question
     - **'label_russian_choices'** - russian labels of choices for each question

   - **Database_questions** - an evolving table that has the representations of each of the questions. This table is created from the Project-database table each time a new survey is uploaded. It hosts all of the unique questions in the database. Its everchanging nature allows us to track the most recent ways of asking a question. While the matching process (see the 'Matching process' chapter) only uses the Project-database table, this file provides a record of the most novel ways of asking questions. This table has the following columns:
     - **'database_label_clean'** - The clean version of the question's english label
     - **'question_type'** - KOBO column 'type' describing whether the column is a 'select_one', 'select_multiple', 'decimal' or 'integer'
     - **'true_ID'** - The uuid of the unique question - This identifier is used to locate identical questions across different research cycle to allow intertemporal and inter-cycle comparison [Backend column]
     - **'DB_ID'** - The name of the question in the data - 'name' column in the KOBO form 
     - **'merger_column'** - a version of 'database_label_clean' with removed stopwords. Used during the process data uploading (see the 'Data uploading' section for more details) [Backend column]

## Data uploading

To upload the new data the user has to open the **Data Uploader** tab of the app. The user will be faced with the interface presented below:

<img width="514" alt="image" src="https://github.com/Nestor-Ch/Reach_QDB/assets/132923140/a8d16243-965e-40e9-91ae-1ee910596497">

Prior to the uploading process the user has to prepare the Kobo form. The only manual input into the form that is needed from the side of the user is creating and filling the **sector** column in the **survey** sheet of the kobo form. This column has to describe what sector does each question in the survey correspond to. This sector should be one of the following:
   - WASH
   - AAP
   - CCCM
   - Shelter and NFI
   - Education
   - Food Security and Livelihoods
   - Health
   - Displacement
   - Protection
   - Nutrition
   - Emergency Telecommunications
   - Logistics
   - Winterization

If the sector in your KOBO tool doesn't match one of these exactly, it will be left blank in the Final QDB, which is fine for Demographic variables and general ones that are needed for backend KOBO functionalities. 

**If your survey sheet has no sector column or if all entries are left blank, the tool won't allow you to proceed.**

To start the uploading process the user has to press the **Browse** button and select the appropriate Kobo form. When the user does this, on the backend the app will upload the form into a special folder in the Sharepoint folder dedicated to the app. After this, the user has to select the ID of the research cycle and the number of the survey round. After clicking this, the user can click the **"Build tables"** button to start the **Matching process**.

## Matching process

The matching algorithm starts off with taking the uploaded and checking it against what already has been uploaded into the project database. If we have questions that are a 1 to 1 match with the ones we're uploading, those are removed from the further steps in the algorithm and assigned into a separate dataframe. This separate dataframe is matched with the Project ID to get the existing True_IDs for the matching questions.
The algorithm also checks if the questions from the KOBO tool we're uploading aren't already present in the dataframe (to see if there were previous attempts to upload this round of questions), if this match is found, the matching questions are dropped from the matching process.

After these checks are done, the algorithm takes the new unique questions from the uploaded KOBO tool and tries to match them to the questions already present in the QDB while, omitting the Project_ID & round_id combination that the user is uploading (i.e. one cannot match a question in a KOBO form to another question in that same KOBO form - this would just mean that we've asked the same question twice, and that's not useful).

To match the new input to the existing questions in the Project_database, the algorithm starts off by cleaning the text label of the Kobo survey by removing all of the punctuation and stopwords. It then tries to match this clean column to the same clean column already existing in the Project_database by calculating a q-gram Jaccard distance of each item in the input text column to the text column in the Project_database. The tests conducted during the initial implementation showed that the algorithm yields the best results when parameter q = 2. For more information on [Jaccardian distance](https://www.statisticshowto.com/jaccard-index/) and [q-grams](https://profs.scienze.univr.it/~liptak/FundBA/slides/StringDistance2_6up.pdf) please check the linked materials.

All matches where the distance is smaller than 0.9 are added, into the main database before proceeding to the final stage of the matching process.

### Semantic matching
The clean database is passed into a Python based function that calculates the semantic distance between the uploaded data and the existing database. The matching is conducted through Python's Spacy module which is based on Google's [word2vec model](https://code.google.com/archive/p/word2vec/). The app is using medium sized language model, due to Shiny server's memory constratints. Cases where the similarity coefficient is larger than 0.97 are automatically matched into the existing database. Cases where these differences are smaller than 0.97 but larger than 0.89 are filtered out for the user input as the closest matches to the input data. Entries with similarity coefficients less than 0.89 are considered `new`. These thresholds have been chosen after testing with different Kobo forms and showed to provide the best results. Top 10 matches for each new question are selected for the final matching table (top 10 is the best case scenario, usually we're getting less than that).

After these steps are done the user has to select the type of the assessment that they're uploading (whether the survey was conducted on the Individual, household or settlement level) and click the **Build tables** button.

#### Setting up a Python virtual environment in Shiny
The environment set up happens in the first lines of script outside of the Shiny server with the use of [reticulate package](https://cran.r-project.org/web/packages/reticulate/reticulate.pdf). This way, the Python environment, packages, and word2vec model are loaded only once and do not need to be activated every time the app is refreshed. This causes an extension of the initial load time of the app (about 2 minutes to load everything) but doesn't add any additional time constraints onto the user.

Setting up the environment relies on the Python paths set up in the .Rprofile file. In cases of running the app on your local machine it relies on the python version that the user has on their computer (Python 3.9.9 in my case). If the app is run in the Shiny environment, the script uses the Shiny server's Python path - PYTHON_PATH = '/opt/python/3.7.7/bin/python3'. 

#### The environment itself is set up through the following set of commands:

##### Creation of the virtual environment on the Shiny server
reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
##### Activation of the virtual environment
reticulate::use_virtualenv(virtualenv_dir, required = T)
##### Package installation
reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=FALSE)

In my case, the app also needs to install the language model, this is done through the `system` command, which is a pip wraparound
system("python -c \"import spacy; spacy.cli.download('en_core_web_md')\"")
##### Function activation
The matching function is sourced through the following command.
reticulate::source_python('www/src/semantic_match.py')

Most of these functions are wrapped around `if` statements to make the app run faster - it's faster to check  the environment for existance of packages and functions than calling Python functions multiple times.
After this is run, the function is added into the local environment and can be used as a regular R function.

These set ups to a large extent are based on the following [page](https://github.com/ranikay/shiny-reticulate-app#setting-up-the-local-virtual-environment) with tweaks to make it work in my environment.

