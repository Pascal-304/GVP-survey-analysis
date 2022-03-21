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
         fg1fgiv,
         gs1datot,
         gs1dntot,
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
  mutate_at(vars(agegr10:fg1fgiv, famincg2:rg_070), 
            .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(agegr10:fg1fgiv, famincg2:rg_070),
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
         giving_flag = fg1fgiv,
         totl_amount_don12 = gs1datot,
         totl_num_don12 = gs1dntot,
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
