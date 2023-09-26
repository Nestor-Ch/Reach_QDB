# Reach_QDB
This repo is based on the Questionnaire database system I've developed within Reach Ukraine.

The app allows the user to systematize the KOBO forms used within their organization. The following Readme goes through the proccess of uploading your tools into the database, browsing the results and explains what's going on under the hood of the app

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


