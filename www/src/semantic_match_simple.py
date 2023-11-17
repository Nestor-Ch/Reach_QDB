import spacy
import numpy as np
nlp = spacy.load('en_core_web_lg')

def similarity_calculator(qdb, column_new, sim_th):

    docs_df1 = list(nlp.pipe(column_new))
    docs_df2 = list(nlp.pipe(qdb['merger_column']))  # Extract the 'merger_column' from the list of tuples
     
    result_list = [{"merger_column_new": column} for column in column_new]
    
    for i, doc_df2 in enumerate(docs_df2):
        col_name = f"{qdb['true_ID'][i]}_separator_{qdb['merger_column'][i]}"
        similarities = np.array([doc1.similarity(doc_df2) for doc1 in docs_df1])
        
        
        for j, similarity in enumerate(similarities):
            result_list[j][col_name] = similarity

    result_list = [item for item in result_list if len(item) > 1]
    
    result_df_long = []
    for item in result_list:
        for key, value in item.items():
            if key != 'merger_column_new':
                true_id, merger_column = key.split('_separator_')
                result_df_long.append({'merger_column_new': item['merger_column_new'], 'true_ID': true_id, 'merger_column': merger_column, 'similarity': value})
    
    if len(result_df_long) == 0:
        return []
    else:
        result_df_long = [{key: float(value) if key == 'similarity' else value for key, value in item.items()} for item in result_df_long if float(item['similarity']) > sim_th]
        return result_df_long