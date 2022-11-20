#install.packages(c("tidyverse", "RSQLite", "lubridate"))
library("tidyverse")
library("RSQLite")
library("lubridate")
library(DBI)
#Using a [demo version](https://physionet.org/content/mimiciii-demo/1.4/) of the
#full database. Instead of using a remote relational database management system
#(RDBMS), we are using a file-based [SQLite](https://www.sqlite.org/index.html)
#RDBMS in this exercise.

## Connect to the database 

#database file in the same folder
con <- dbConnect(RSQLite::SQLite(), "mimic3.db")

#The tables in the database
dbListTables(con)

tbl(con, "D_ITEMS")


### Task 1
#Use the previous "recipe" to find out the top-5 prescribed drugs.
#Hint: They are in the **PRESCRIPTIONS** table. Check the database schema
#description to find a suitable column for grouping.

prescriptions <- tbl(con, "PRESCRIPTIONS")
top5_prescribed_drugs <- prescriptions %>%
  group_by(DRUG_NAME_GENERIC) %>%
  summarise(count = n()) %>%
  slice_max(count, n=5)
top5_prescribed_drugs

### Task 2
#Replicate the bottom part of Figure 2 in the MIMIC-III paper (Nature Scientific Data).

#Notice that the x axis should now show time relative to the admission, not absolute
#time. To calculate the relative time, you should join the **ADMISSIONS** table,
#which contains the time of admission. The chart also shows multiple types of
#measurements, not only "Heart Rate".

chartevents <- tbl(con, "CHARTEVENTS")
d_items <- tbl(con, "D_ITEMS")

labels <- c("Heart Rate",
            "Respiratory Rate",
            "SpO2",
            "Temperature C (calc)",
            "Arterial BP [Diastolic]",
            "Arterial BP [Systolic]")

admissions <- tbl(con, "ADMISSIONS")

#joining chartevents and d_items tables and filtering one patient and the desired labels
a <- chartevents %>%
  inner_join(d_items,by = c("ITEMID")) %>%
  inner_join(admissions, by= c("HADM_ID", "SUBJECT_ID")) %>%
  filter(SUBJECT_ID == 10019 & LABEL %IN% labels) 
  
a

# THEn changing the time to appropriate form and calculating the relative time 

a %>%
  collect() %>%                              
  mutate(time = difftime(as_datetime(CHARTTIME),
                          as_datetime(ADMITTIME),
                          units = "hours" )) %>% 
  ggplot(aes(x = time,
             y = VALUENUM,
             group =LABEL)) +
  xlab("Time after addmission") + 
  ylab("Measurement absolute value") +
  ggtitle("Measurements after addmission to intensive care unit")+
  geom_line(aes(col = LABEL))


### Task 3

#Study the database schema and explore the database. Try to find
#something interesting and report the finding together with your code.
#Hint: you can join, filter, group, and summarize the tables. Plotting is
#encouraged! Dates are difficult with dbplyr, but you can `collect()`
#intermediary results into a data frame. Checking the dplyr [vignette
#(https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html)
#is recommended.

dbListTables(con)
#diagnosis frequency 
diagnose_num <- tbl(con, "DIAGNOSES_ICD") 
diagnose_text <- tbl(con, "D_ICD_DIAGNOSES")
diagnoses <- diagnose_num %>%
  inner_join(diagnose_text, by = c("ICD9_CODE")) %>%
  select(ICD9_CODE, SHORT_TITLE) %>%
  collect()
diagnoses
a <- table(diagnoses$ICD9_CODE)
hist(a)

#https://academic.oup.com/ajcn/article-abstract/5/4/431/4787047
#B12 deficiency causes hyperglycemia 

labitems<-tbl(con, "D_LABITEMS") 
labevents <- tbl(con, "LABEVENTS")
labels <- labitems %>%
  pull(LABEL)
labels

labels <- c("Vitamin B12",
            "Glucose")



plott <- labitems %>%
  inner_join(labevents, by="ITEMID") %>%
  filter(LABEL %IN% labels) %>%
  select(SUBJECT_ID, LABEL, VALUENUM) %>%
  group_by(SUBJECT_ID, LABEL) %>%
  summarize(mean_val = mean(VALUENUM)) %>%
  pivot_wider(names_from = LABEL, values_from = mean_val) %>%
  filter(!is.na(Glucose) & !is.na(`Vitamin B12`)) %>%
  rename(B12 = 'Vitamin B12') 


plott%>%
  ggplot(aes( x= B12, y = Glucose)) + geom_point()

table_1 <- collect(plott)

hist(table_1$Glucose)
hist(table_1$B12)
#not normally distributed 
cor.test(table_1$Glucose, table_1$B12 , method = 'spearman')
wilcox.test(table_1$Glucose, table_1$B12)



