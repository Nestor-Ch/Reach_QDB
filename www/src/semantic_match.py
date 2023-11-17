import spacy
import numpy as np
import pandas as pd

nlp = spacy.load('en_core_web_md')

def similarity_calculator(qdb, column_new, sim_th):
    column_qdb = qdb[['merger_column','true_ID']].drop_duplicates()
    column_qdb = column_qdb.reset_index(drop=True)
    docs_df1 = list(nlp.pipe(column_new))

    docs_df2 = list(nlp.pipe(column_qdb['merger_column']))
    
    result_df = pd.DataFrame({"merger_column_new": column_new})
    
    for i, doc_df2 in enumerate(docs_df2):
        col_name = f"{column_qdb['true_ID'][i]}_separator_{docs_df2[i]}"
        # Calculate similarities
        similarities = np.array([doc1.similarity(doc_df2) for doc1 in docs_df1])
    
        filtered_similarities = similarities[pd.to_numeric(similarities) > sim_th]
    
        if len(filtered_similarities) > 0:
            result_df = pd.concat([result_df, pd.DataFrame({col_name: similarities})], axis=1)
            
    
    result_df_long = result_df.melt(id_vars=['merger_column_new'], var_name='merger_column_inter', value_name='similarity')
   
    if len(result_df_long.index) == 0:
        return(pd.DataFrame(columns = ['merger_column_new','similarity','true_ID','merger_column']))
    else:
        result_df_long[['true_ID', 'merger_column']] = result_df_long['merger_column_inter'].str.split('_separator_', expand=True)
        result_df_long = result_df_long.drop('merger_column_inter', axis=1)
    
        result_df_long['similarity'] = pd.to_numeric(result_df_long['similarity'])
        result_df_clean = result_df_long[result_df_long['similarity']>sim_th]
    
        return(result_df_clean)
