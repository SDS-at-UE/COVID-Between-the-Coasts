

#citizenship by age and nativity and citizenship

#create variables_2018 set from 5 year ACS
variables_2018 <- load_variables(2018, "acs5", cache = TRUE) %>%
 rename(variable = name)
# create sex_by_age_by_nativity_and_citizenship_status
sex_by_age_by_nativity_and_citizenship_status<- 
  left_join((get_acs(geography="zcta", table="B05003")), variables_2018, by="variable")

#select Chicago ZIPs
filter(sex_by_age_by_nativity_and_citizenship_status, GEOID %in% 60601:60827)

#remove unneeded !s

sex_by_age_by_nativity_and_citizenship_status$label <- 
  as_factor(str_replace(sex_by_age_by_nativity_and_citizenship_status$label, ".*!!(.*)", "\\1"))

#remove total rows
sex_by_age_by_nativity_and_citizenship_status<- 
  filter(sex_by_age_by_nativity_and_citizenship_status, label!="Total")


#ancestry


ancestry<-left_join((get_acs(geography="zcta", table="B04006")), variables_2018, by="variable")

ancestry$label<- as_factor(str_replace(ancestry$label, ".*!!(.*)", "\\1"))

ancestry<-filter(ancestry, label!="Total")

ancestry<- filter(ancestry, GEOID %in% 60601:60827)


#health insurance by type 

HI_type<-left_join((get_acs(geography="zcta", table="B27010")), variables_2018, by="variable")

HI_type$label<- as_factor(str_replace(HI_type$label, ".*!!(.*)", "\\1"))

HI_type<-filter(HI_type, label!="Total")

HI_type<-filter(HI_type, GEOID %in% 60601:60827)
