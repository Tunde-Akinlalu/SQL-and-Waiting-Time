


con  = dbConnect(MySQL(), 
                           user = 'root', 
                           password = '@newconvenant', 
                           dbname = 'garkidb',
                           host = 'localhost')
dbListTables(con)


##############################################################################
# USING LUBRIDATE DIFF TIME TO CALCULATE WAITING TIME
#FROM THE TABLE 'ENCOUNTER'


waity = tbl(con, 'encounter') %>% na.omit() %>% 
  select(start_date, triaged_on, canceled, follow_up) %>% 
  filter(canceled == 0 & follow_up == 1)  %>% 
  collect() %>%
  mutate(start_date =  as.POSIXct(start_date)) %>%
  mutate(triaged_on = as.POSIXct(triaged_on)) %>%
  mutate(pxwait = difftime(triaged_on, start_date, units = "mins"))  %>%
  mutate(year = year(start_date)) %>% 
  select(year, everything()) %>%
  mutate(pxwait = as.numeric(pxwait)) %>%
  mutate(min_wait = pxwait/60) %>%
  mutate(min_wait = as.numeric(min_wait)) 



#######################################################################
#Removing Outliers

wait_outlier =boxplot(waity$min_wait, plot=FALSE)$out

#CREATE ANOTHER DF TO PRESERVE ORIGINAL VALUE
temp = waity

#REMOVE OUTLIERS FROM THE NEW DF
temp = temp[-which(temp$min_wait %in% outliers),]

#CHECK THE MEAN FOR THE YEAR YOU ARE INTERESTED IN
#USE SUMMARY TO KNOW THE MEAN
temp %>% filter(year== 2019) %>% summary(pxwait)



#################################################################################################

#ANOTHER WAY TO DO THIS: DIRECTLY IN SQL
################################################################################################

dbListTables(con)



dsql = "SELECT start_date, triaged_on, consulted_on, consulted_by,  
TIMESTAMPDIFF(MINUTE, triaged_on, consulted_on)
AS patient_wait, YEAR(start_date) AS visit_year FROM encounter 
WHERE (start_date > '2017-12-31 23:59:59' AND
(triaged_on IS NOT NULL AND consulted_on IS NOT NULL) AND 
TIMESTAMPDIFF(DAY, consulted_on, triaged_on) < 1 AND 
TIMESTAMPDIFF(DAY, start_date, consulted_on) < 1)"

a = dbGetQuery(con, dsql)


b = a %>% filter(visit_year == 2020) %>% mutate(wait_time = patient_wait/60) %>% summary(mean(wait_time))
b



waity = tbl(con, 'encounter') %>% na.omit() %>% 
  select(start_date, triaged_on, canceled, follow_up) %>% 
  filter(canceled == 0 & follow_up == 1)  %>% 
  collect() %>%
  mutate(start_date =  as.POSIXct(start_date)) %>%
  mutate(triaged_on = as.POSIXct(triaged_on)) %>%
  mutate(pxwait = time_length(start = triaged_on, end = start_date, tzone = triaged_on)) %>%
  mutate(year = year(start_date)) %>% 
  select(year, everything()) %>%
  mutate(pxwait = as.numeric(pxwait)) %>%
  mutate(min_wait = pxwait/60) %>%
  mutate(min_wait = as.numeric(min_wait)) 

  
###################################################################################
# GET THE STAFF THAT CONSULTED ON THE PATIENTS

dstaff = "SELECT start_date, consulted_by, doctor_start_time, 
CONCAT(firstname, ' ', lastname) AS provider, departments.name AS department, 
staff_type AS specialization, YEAR(start_date) AS year FROM encounter 
LEFT JOIN staff_directory ON staff_directory.staffID = encounter.consulted_by 
LEFT JOIN departments ON departments.id = encounter.department_id
JOIN staff_specialization ON staff_specialization.id = encounter.specialization_id 
WHERE staff_directory.is_consultant IS NOT NULL"
dstaff = dbGetQuery(con, dstaff)


