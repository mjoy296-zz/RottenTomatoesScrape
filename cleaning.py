import pandas as pd
import csv

t = pd.read_csv("Top100.csv")
t['titles'] = t['titles'].str[:-6]
t.titles = t.titles.str.strip()
t['month'] = t['date'].str[2:-3]
t['month'] = t['month'].str.replace("-","")
t2 = t[['titles', 'ranks, month']]


w = pd.read_csv("Detailed.csv")
w = w.dropna(how = "all")

w_obj = w.select_dtypes(['object'])

w[w_obj.columns] = w_obj.apply(lambda x: x.str.strip())
w = pd.DataFrame(w)

w['a_count'] = w['a_count'].str[4:]
w['year'] = w['year'].astype(str)
w['year'] = w['year'].str[1:]
w['a_score'] = w['a_score'].str[:-1]
w['runtime'] = w['runtime'].str[:-7]
w[['year','a_score','a_count','c_score', 'c_count','runtime','titles']] = w[['year','a_score','a_count','c_score', 'c_count','runtime','titles']].apply(strip)
#w[['year','a_score','a_count','c_score', 'c_count','runtime']] = w[['year','a_score','a_count','c_score', 'c_count','runtime']].apply(pd.to_numeric)

result = pd.merge(t2, w, on = "titles", how = "left")
result.to_csv("TopLists.csv")

result
g = pd.read_csv("a_genders.csv")
g.actor = g.actor.str.strip()


result2 = pd.merge(result, g, left_on = "actor1", right_on = "actor", how = "left")
result2['a1_g'] = result2['ga_gender']
result2.drop(['actor','ga_gender','ga_accuracy'], axis=1, inplace=True)
temp = result2
temp

result3 = pd.merge(temp, g, left_on = "actor2", right_on = "actor", how = "left")
result3['a2_g'] = result3['ga_gender']
result3.drop(['actor','ga_gender','ga_accuracy'], axis=1, inplace=True)
temp2 = result3
temp2

result4 = pd.merge(temp2, g, left_on = "actor3", right_on = "actor", how = "left")
result4['a3_g'] = result4['ga_gender']
result4.drop(['actor','ga_gender','ga_accuracy'], axis=1, inplace=True)
temp3 = result4
temp3

result4.to_csv("TopComprehensive.csv")
##convert dates
t = []
for x in temp3.date.iteritems():
    x = list(x)
    if pd.notnull(x[1]):
        t.append((x[1][:6]+ '-20' + x[1][-2:]).replace("--","-"))
     else:
        t.append("NaN")

t = pd.DataFrame(t)
temp3.date = t
##drop missing values
temp3.isnull().sum(axis =1)
temp3.dropna(subset=['a_score','a_score','actor2'], how = "all")
temp3.to_csv("TopComprehensive.csv")
##clean up genres
t = pd.read_csv("TopComprehensive.csv", encoding = "latin-1")
z = t.genre.unique()
z = pd.DataFrame(z)
z = z.replace("\\r\\r\\r\\r\\n ", " ", regex = True)
t.genre = z

#unique genres and count
z = z.replace(",","",regex = True)
x = pd.DataFrame(z[0].str.split().tolist()).stack()
