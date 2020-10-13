
library(tidyverse)
library(tidycensus)

#load Chicago data

chicago_covid <- read_csv("Data/chicago_zip_positive_cases.csv",
                          col_types = cols(Zip = col_character()))
chicago_covid <- na.omit(chicago_covid)
chicago_covid <- chicago_covid %>% 
  mutate(case_rate = Cases/Population*100000) %>% 
  mutate(pos_rate = Cases/Tested)
zip_code <- read_csv("Data/zip_county.csv")
zip_code_chicago <- zip_code %>% filter(primary_city == "Chicago",
                                        state == "IL")



#citizenship by age and nativity and citizenship

#create variables_2018 set from 5 year ACS
variables_2018 <- load_variables(2018, "acs5", cache = TRUE) %>%
 rename(variable = name)
# create sex_by_age_by_nativity_and_citizenship_status
sex_by_age_by_nativity_and_citizenship_status<- 
  left_join((get_acs(geography="zcta", table="B05003")), variables_2018, by="variable")

#select Chicago ZIPs
sex_by_age_by_nativity_and_citizenship_status<-
  filter(sex_by_age_by_nativity_and_citizenship_status, GEOID %in% 60601:60827)

#remove unneeded !s

sex_by_age_by_nativity_and_citizenship_status$label <- 
  as_factor(str_replace(sex_by_age_by_nativity_and_citizenship_status$label, ".*!!(.*)", "\\1"))

#remove unneeded rows
sex_by_age_by_nativity_and_citizenship_status <- sex_by_age_by_nativity_and_citizenship_status %>%
  filter(!label %in% c("Total", "Naturalized U.S. citizen", "Not a U.S. citizen"))



#rename variables

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable, 
                  "B05003_002", "total males" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                  "B05003_003", "males under 18" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
   str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable, 
                   "B05003_004", "native males under 18" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
    str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                    "B05003_005", "foreign-born males under 18" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
   str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable, 
                   "B05003_008", "males 18 years and over" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable, 
                  "B05003_009", "native males 18 years and over" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
   str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                      "B05003_010", "foreign-born males 18 years and over" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                  "B05003_013", "total females" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                  "B05003_014", "females under 18" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                  "B05003_015", "native females under 18" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                  "B05003_016", "foreign-born females under 18" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                  "B05003_019", "females 18 and over" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                  "B05003_020", "native females 18 and over" )

sex_by_age_by_nativity_and_citizenship_status$variable<-
  str_replace_all(sex_by_age_by_nativity_and_citizenship_status$variable,
                  "B05003_021", "foreign-born females 18 years and over" )

#left join Covid data

sex_by_age_by_nativity_and_citizenship_status<- 
  left_join(sex_by_age_by_nativity_and_citizenship_status, chicago_covid, by=c("GEOID"="Zip"))

#ancestry


ancestry<-left_join((get_acs(geography="zcta", table="B04006")), variables_2018, by="variable")

ancestry$label<- as_factor(str_replace(ancestry$label, ".*!!(.*)", "\\1"))

ancestry<-filter(ancestry, label!="Total")

ancestry<- filter(ancestry, GEOID %in% 60601:60827)



ancestry_2 <- ancestry %>% 
  arrange(GEOID) %>% 
  group_by(GEOID) %>% 
  mutate(zip_pop = sum(estimate)) %>% 
  group_by(GEOID, label) %>% 
  mutate(prop_anc = sum(estimate)/zip_pop) %>% 
  group_by(GEOID, label) 

ancestry<- left_join(ancestry, chicago_covid, by=c("GEOID"="Zip"))

#health insurance by type 

HI_type_orig<-left_join(
  (get_acs(geography="zcta", table="B27010")), variables_2018, by="variable") %>% 
  filter(GEOID %in% zip_code_chicago$zip)

HI_type_final<-HI_type_orig%>%filter(str_count(label, "!!") >=3, 
                                     !str_detect(label, "of health insurance coverage$"))
HI_type_final$label<-str_remove(HI_type_final$label, "Estimate!!Total!!")


HI_type_final <- separate(HI_type_final,
                      label,
                      sep = "!!",
                      into = c( "Age Group", "Number_HI_polcies", "HI_Type"))

HI_type_final<- HI_type_final  %>% 
  arrange(GEOID) %>% 
  group_by(GEOID) %>% 
  mutate(zip_pop = sum(estimate)) %>% 
  group_by(GEOID, variable) %>% 
  mutate(prop_HItype = sum(estimate)/zip_pop)

HI_type<-filter (HI_type, !variable %in% 
                  c( "B27010_001","B27010_010",  "B27010_002", "B27010_003", 
                    "B27010_026",  "B27010_019", "B27010_018", "B27010_034",
                    "B27010_042",  "B27010_051",
                     "B27010_035", 
                    "B27010_058", "B27010_052"))


HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_004", "under 19 with employer-based health insurance" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_005", "under 19 with direct-purchase health insurance" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_006", "under 19 with Medicare" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_007", "under 19 with Medicaid" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_008", "under 19 with TRICARE" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_009", "under 19 with VA Health Care" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_017", "under 19 with no health insurance" )

HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_020", "19 to 34 with employer-based health insurance" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_021", "19 to 34 with direct-purchase health insurance" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_022", "19 to 34! with Medicare" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_023", "19 to 34 with Medicaid" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_024", "19 to 34 with TRICARE" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_025", "19 to 34 with VA Health Care" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_033", "19 to 34 with no Health Insurance" )

HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_036", "35 to 64 with Employer-Based Health Insurance" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_037", "35 to 64 with Direct-Purchase Health Insurance" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_038", "35 to 64 with Medicare" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_039", "35 to 64 with Medicaid" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_040", "35 to 64 with TRICARE" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_041", "35 to 64 with VA Health Care" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_050", "35 to 64 with no Health Insurance" )

HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_053", "65 and over with employer-based health insurance" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_053", "65 and over with direct-purchase health insurance" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_054", "65 and over with Medicare" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_055", "65 and over with Medicaid" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_056", "65 and over with TRICARE" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_057", "65 and over with VA Health Care" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_066", "65 and over with no Health Insurance" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_011", "Under 19 with employer-based and direct-purchase coverage" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_012", "Under 19 with employer-based and Medicare coverage" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_013", "Under 19 with Medicare and Medicaid" )

HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_014", "Under 19 with other private-only combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_015", "Under 19 with other public combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_016", "Under 19 with other coverage combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_027", "19 to 34 with employer-based and direct-purchase coverage" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_028", "19 to 34 with employer-based and Medicare coverage" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_029", "19 to 34 with Medicare and Medicaid" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_030", "19 to 34 with other private-only combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_031", "19 to 34 with other public combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_032", "19 to 34 with other coverage combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_043", "35 to 64 with employer-based and direct-purchase coverage" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_044", "35 to 64 with employer-based and Medicare coverage" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_045", "35 to 64 with direct-purchase and Medicare coverage" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_046", "35 to 64 with Medicare and Medicaid" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_047", "35 to 64 with other private-only combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_048", "35 to 64 with other public combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_049", "35 to 64 with other coverage combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_059", "65 and over with employer-based and direct-purchase coverage")
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_060", "65 and over with employer-based and Medicare coverage" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_061", "65 and over with direct-purchase and Medicare coverage" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_062", "65 and over with Medicare and Medicaid" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_063", "65 and over with other private-only combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_064", "65 and over with other public combinations" )
HI_type$variable<-
  str_replace_all(HI_type$variable,
                  "B27010_065", "65 and over with other coverage combinations" )
HI_type_2<- HI_type  %>% 
  arrange(GEOID) %>% 
  group_by(GEOID) %>% 
  mutate(zip_pop = sum(estimate)) %>% 
  group_by(GEOID, variable) %>% 
  mutate(prop_HItype = sum(estimate)/zip_pop)

HI_type_2 <- separate(HI_type_2,
                    variable,
                    sep = "with",
                    into = c( "Age", "HI_Type"))


HI_type<- left_join(HI_type, chicago_covid, by=c("GEOID"="Zip"))
