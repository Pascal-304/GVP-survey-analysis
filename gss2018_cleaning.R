#### Workspace set-up ####
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("gss2018_data.csv")
dict <- read_lines("gss2018_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss2018_labels.txt")


# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case 
# when statement for the categorical variables
gss <- raw_data %>% 
  select(CASEID,
         agegr10, 
         gndr,
         marstat,
         dg_005, 
         dg_030,
         dg_050,
         dg_060,
         dg7075a,
         dg7075b,
         dg7075c,
         dg7075d,
         dg7075e, 
         dg7075f, 
         dg7075g, 
         dg7075h,
         dg_080,
         dg_090,
         fg1a_030,
         fg1a_040,
         fg1a_050,
         fg1a_060,
         fg1a_070,
         fg1a_080,
         fg1a_090,
         fg1a_100,
         fg1a_110,
         fg1a_120,
         fg1a_130,
         fg1a_140,
         fg1a_170,
         fg1dad03,
         fg1dad04,
         fg1dad05,
         fg1dad06,
         fg1dad07,
         fg1dad08,
         fg1dad09,
         fg1dad10,
         fg1dad11,
         fg1dad12,
         fg1dad13,
         fg1dad14,
         fg1dad17,
         fg1dnd03,
         fg1dnd04,
         fg1dnd05,
         fg1dnd06,
         fg1dnd07,
         fg1dnd08,
         fg1dnd09,
         fg1dnd10,
         fg1dnd11,
         fg1dnd12,
         fg1dnd13,
         fg1dnd14,
         fg1dnd17,
         fg1fgiv,
         fg2a_180,
         gs1da201,
         gs1da202,
         gs1da203,
         gs1da204,
         gs1da205,
         gs1da206,
         gs1da207,
         gs1da208,
         gs1da209,
         gs1da210,
         gs1da211,
         gs1da212,
         gs1datot,
         gs1dax01,
         gs1dax02,
         gs1dax03,
         gs1dax04,
         gs1dax05,
         gs1dax06,
         gs1dax07,
         gs1dax08,
         gs1dax09,
         gs1dax10,
         gs1dax11,
         gs1dax12,
         gs1dax13,
         gs1dax14,
         gs1dax15,
         gs1dn201,
         gs1dn202,
         gs1dn203,
         gs1dn204,
         gs1dn205,
         gs1dn206,
         gs1dn207,
         gs1dn208,
         gs1dn209,
         gs1dn210,
         gs1dn211,
         gs1dn212,
         gs1dntot,
         gs1dnx01,
         gs1dnx02,
         gs1dnx03,
         gs1dnx04,
         gs1dnx05,
         gs1dnx06,
         gs1dnx07,
         gs1dnx08,
         gs1dnx09,
         gs1dnx10,
         gs1dnx11,
         gs1dnx12,
         gs1dnx13,
         gs1dnx14,
         gs1dnx15,
         famincg2,
         famincq,
         incg2,
         incq,
         ng_020,
         ng_030,
         ng_040,
         ng_050,
         ng_060,
         ng_070,  
         ng_080,
         ng_090,
         ng_110,
         ng_130,
         ng_150,
         ng_160,
         og_010,
         og_020,  
         og_030,
         og_040,
         rg_010,  
         rg_020,
         rg_030,
         rg_035,
         rg_040,
         rg_050,
         rg_060,
         rg_070) %>% 
  mutate_at(vars(agegr10:fg1a_170, fg1fgiv:fg2a_180, famincg2:rg_070), 
            .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(agegr10:fg1a_170, fg1fgiv:fg2a_180, famincg2:rg_070),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(age = agegr10, 
         gender = gndr,
         marital_status = marstat,
         decision_on_giving_tax_cred = dg_005, 
         decision_on_giving_decide_in_advance = dg_030,
         decision_on_giving_pattern_of_giving = dg_050,
         decision_on_giving_search = dg_060,
         decision_on_giving_search_info_from_charity = dg7075a,
         decision_on_giving_search_contact_charity = dg7075b,
         decision_on_giving_search_consult_CRA_web = dg7075c,
         decision_on_giving_search_consult_web = dg7075d,
         decision_on_giving_search_general_online = dg7075e, 
         decision_on_giving_search_ask_someone = dg7075f, 
         decision_on_giving_search_consult_other_source = dg7075g, 
         decision_on_giving_search_other_meth = dg7075h,
         decision_on_giving_know_how_to_verify = dg_080,
         decision_on_giving_org_monitoring = dg_090,
         fin_giving_mail = fg1a_030,
         fin_giving_telephone = fg1a_040,
         fin_giving_tv = fg1a_050,
         fin_giving_online = fg1a_060,
         fin_giving_own_initiative = fg1a_070,
         fin_giving_charity_event = fg1a_080,
         fin_giving_in_mem_of_someone = fg1a_090,
         fin_giving_work = fg1a_100,
         fin_giving_door_to_door_canvassing = fg1a_110,
         fin_giving_shopping_centre = fg1a_120,
         fin_giving_place_of_worship = fg1a_130,
         fin_giving_sponsoring = fg1a_140,
         fin_giving_other = fg1a_170,
         amount_of_don_mail = fg1dad03,
         amount_of_don_telephone = fg1dad04,
         amount_of_don_tv = fg1dad05,
         amount_of_don_online = fg1dad06,
         amount_of_don_own_initiative = fg1dad07,
         amount_of_don_charity_event = fg1dad08,
         amount_of_don_in_mem_of_someone = fg1dad09,
         amount_of_don_work = fg1dad10,
         amount_of_don_door_to_door_canvassing = fg1dad11,
         amount_of_don_shopping_centre = fg1dad12,
         amount_of_don_place_of_worship = fg1dad13,
         amount_of_don_sponsoring = fg1dad14,
         amount_of_don_other = fg1dad17,
         num_of_don_mail = fg1dnd03,
         num_of_don_telephone = fg1dnd04,
         num_of_don_tv = fg1dnd05,
         num_of_don_online = fg1dnd06,
         num_of_don_own_initiative = fg1dnd07,
         num_of_don_charity_event = fg1dnd08,
         num_of_don_in_mem_of_someone = fg1dnd09,
         num_of_don_work = fg1dnd10,
         num_of_don_door_to_door_canvassing = fg1dnd11,
         num_of_don_shopping_centre = fg1dnd12,
         num_of_don_place_of_worship = fg1dnd13,
         num_of_don_sponsoring = fg1dnd14,
         num_of_don_other = fg1dnd17,
         giving_flag = fg1fgiv,
         larg_don_decision = fg2a_180,
         amount_don_culture_and_recrea12 = gs1da201,
         amount_don_edu_and_research12 = gs1da202,
         amount_don_health12 = gs1da203,
         amount_don_soc_serv12 = gs1da204,
         amount_don_env12 = gs1da205,
         amount_don_dev_and_housing12 = gs1da206,
         amount_don_law_advocacy12 = gs1da207,
         amount_don_philanthropic_intermediaries12 = gs1da208,
         amount_don_internatl_org12 = gs1da209,
         amount_don_religion12 = gs1da210,
         amount_don_business_and_prof12 = gs1da211,
         amount_don_not_classif12 = gs1da212,
         totl_amount_don12 = gs1datot,
         amount_don_arts_and_culture15 = gs1dax01,
         amount_don_sports_and_recrea15 = gs1dax02,
         amount_don_edu_and_research15 = gs1dax03,
         amount_don_univ_and_col15 = gs1dax04,
         amount_don_health15 = gs1dax05,
         amount_don_hospital15 = gs1dax06,
         amount_don_soc_serv15 = gs1dax07,
         amount_don_env15 = gs1dax08,
         amount_don_dev_and_housing15 = gs1dax09,
         amount_don_law_advocacy15 = gs1dax10,
         amount_don_grant_fundrais15 = gs1dax11,
         amount_don_internatl_org15 = gs1dax12,
         amount_don_religion15 = gs1dax13,
         amount_don_business_and_prof15 = gs1dax14,
         amount_don_not_classif15 = gs1dax15,
         num_don_culture_and_recrea12 = gs1dn201,
         num_don_edu_and_research12 = gs1dn202,
         num_don_health12 = gs1dn203,
         num_don_soc_serv12 = gs1dn204,
         num_don_env12 = gs1dn205,
         num_don_dev_and_housing12 = gs1dn206,
         num_don_law_advocacy12 = gs1dn207,
         num_don_philanthropic_intermediaries12 = gs1dn208,
         num_don_internatl_org12 = gs1dn209,
         num_don_religion12 = gs1dn210,
         num_don_business_and_prof12 = gs1dn211,
         num_don_not_classif12 = gs1dn212,
         totl_num_don12 = gs1dntot,
         num_don_arts_and_culture15 = gs1dnx01,
         num_don_sports_and_recrea15 = gs1dnx02,
         num_don_edu_and_research15 = gs1dnx03,
         num_don_univ_and_col15 = gs1dnx04,
         num_don_health15 = gs1dnx05,
         num_don_hospital15 = gs1dnx06,
         num_don_soc_serv15 = gs1dnx07,
         num_don_env15 = gs1dnx08,
         num_don_dev_and_housing15 = gs1dnx09,
         num_don_law_advocacy15 = gs1dnx10,
         num_don_grant_fundrais15 = gs1dnx11,
         num_don_internatl_org15 = gs1dnx12,
         num_don_religion15 = gs1dnx13,
         num_don_business_and_prof15 = gs1dnx14,
         num_don_not_classif15 = gs1dnx15,
         fam_income_gp = famincg2,
         fam_income_quintile = famincq,
         income_gp = incg2,
         income_quintile = incq,
         reas_not_giv_already_gave = ng_020,
         reas_not_giv_not_afford = ng_030,
         reas_not_giv_no_asked = ng_040,
         reas_not_giv_not_know_where_to = ng_050,
         reas_not_giv_hard_finding_cause = ng_060,
         reas_not_giv_gave_time_instead = ng_070,  
         reas_not_giv_gave_direct_to_ppl = ng_080,
         reas_not_giv_tax_cred_not_incentive = ng_090,
         reas_not_giv_money_would_not_be_used_efficient = ng_110,
         reas_not_giv_not_like_way_requests = ng_130,
         reas_not_giv_so_many_org = ng_150,
         reas_not_giv_charity_fraud = ng_160,
         other_giv_food_bank = og_010,
         other_giv_cloth_toy_household = og_020,  
         other_giv_bequest_in_respond_will = og_030,
         other_giv_direct_to_ppl = og_040,
         reas_giv_pers_affected = rg_010,  
         reas_giv_tax_cred = rg_020,
         reas_giv_religious = rg_030,
         reas_giv_spiritual_beliefs = rg_035,
         reas_giv_cause = rg_040,
         reas_giv_compassion = rg_050,
         reas_giv_community_contribution = rg_060,
         reas_giv_being_asked = rg_070)

#### Clean up ####
gss <- gss %>% 
  mutate_at(vars(age:reas_giv_being_asked), 
            .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated", "NA", .))) 

gss <- gss %>% 
  mutate(
    gender =
           recode(
             gender,
             'Male gender' = 'Male',
             'Female gender' = 'Female'
           )
  )

write_csv(gss, "gss2018.csv")
