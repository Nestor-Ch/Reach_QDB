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

To upload the new data the user has to open the **Data Uploader** tab of the app. The user will be faced with the interface presented below:

<img width="514" alt="image" src="https://github.com/Nestor-Ch/Reach_QDB/assets/132923140/a8d16243-965e-40e9-91ae-1ee910596497">

Prior to the uploading process the user has to prepare the Kobo form. The only manual input into the form that is needed from the side of the user is creating and filling the **sector** column in the **survey** sheet of the kobo form. This column has to describe what sector does each question in the survey correspond to. This sector should be one of the following:
   - Sector 1
   - Sector 2
   - Sector 3

To start the uploading process the user has to press the **Browse** button and select the appropriate Kobo form. When the user does this, on the backend the app will upload the form into a special folder in the Sharepoint folder dedicated to the app. After this, the user has to select the ID of the research cycle and the number of the survey round. After clicking this, the user can click the **"Build tables"** button to start the **Matching process**.

## Matching process

The matching algorithm starts off with taking the uploaded and checking it against what already has been uploaded into the project database. If we have questions that are a 1 to 1 match with the ones we're uploading, those are removed from the further steps in the algorithm and assigned into a separate dataframe. This separate dataframe is matched with the Project ID to get the existing True_IDs for the matching questions.
The algorithm also checks if the questions from the KOBO tool we're uploading aren't already present in the dataframe (to see if there were previous attempts to upload this round of questions), if this match is found, the matching questions are dropped from the matching process.

After these checks are done, the algorithm takes the new unique questions from the uploaded KOBO tool and tries to match them to the questions already present in the QDB while, omitting the Project_ID & round_id combination that the user is uploading (i.e. one cannot match a question in a KOBO form to another question in that same KOBO form - this would just mean that we've asked the same question twice, and that's not useful).

To match the new input to the existing questions in the Project_database, the algorithm starts off by cleaning the text label of the Kobo survey by removing all of the punctuation and stopwords. It then tries to match this clean column to the same clean column already existing in the Project_database by calculating a q-gram Jaccard distance of each item in the input text column to the text column in the Project_database. The tests conducted during testing showed that the algorithm yields the best results when parameter q = 2. For more information on [Jaccardian distance](https://www.statisticshowto.com/jaccard-index/) and [q-grams](https://profs.scienze.univr.it/~liptak/FundBA/slides/StringDistance2_6up.pdf) please check the linked materials.

All matches where the distance is larger than 0.65 are dropped, and top 10 matches for each new column are selected for the final matching table (top 10 is the best case scenario, usually we're getting less than that). If there are no matches in the Project_database, the question is considered 'new'.






