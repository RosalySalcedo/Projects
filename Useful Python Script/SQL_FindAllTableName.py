import pandas as pd, os

# Finds all of the Table names within a SQL script
#  
li = [  
    "Select Column1, Column2, Column3, Column4, Column4 FROM Table1",
    "Select Column6, Column7, Column8, Column9, Column10 FROM Table2",
    "Select Column11, Column12, Column13, Column14, Column15 FROM Table3",
    "Select A.Column16, A.Column17, B.Column18, A.Column19, A.Column20 FROM Table4 A JOIN Table5 B ON A.Column16 = B.Column3" 
    ]

Table_query_dict = {}
test = []
for i in li:
    sp = i.split()
    check = []
    for i_word in range(len(sp)):
        if sp[i_word] == "FROM":   
                 check.append(sp[i_word + 1])
        if sp[i_word] == "JOIN":
            if sp[i_word + 1][0] == "(":
                continue
            else:
                check.append(sp[i_word + 1])
        Table_query_dict[i] = check
print(Table_query_dict)


df = pd.DataFrame.from_dict(Table_query_dict, orient='index')
df1 = df.reset_index()
print(df1)
# Insert file location path 
# filepath = r"\Tables.xlsx"
# df1.to_excel(filepath)

# os.startfile(filepath)
