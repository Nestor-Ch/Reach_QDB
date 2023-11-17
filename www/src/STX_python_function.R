library(reticulate)


use_python('www/spacy_venv/Scripts/python.exe')

source_python('www/src/semantic_match_simple.py')

print('Python model and environment loaded')