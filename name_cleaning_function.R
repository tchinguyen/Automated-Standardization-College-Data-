name_cleaning_function <- function(raw.data, clean.data, year){
  library(tidyverse)
  library(dplyr)
  library(tools)
  library(stringdist)
  library(RecordLinkage)
  ##read the raw data
  raw_data = read.csv(raw.data)
  ## subset the data for given issue year
  data_year = raw_data[raw_data$Issue.year == year,]
  data_year = data_year[data_year$School.Type == "National Universities",]
  ###read the cleaned dataset
  cleaned_data = readxl::read_xlsx(clean.data)
  ############################################################################################################
  ############### PART 1: SCHOOL NAME MATCHING
  #### Get the unique school names from cleaned dataset and sort them alphabetically
  preferred_names= cleaned_data$`Institution Name` %>% unique() %>% sort()
  #### Get the unique school names from raw dataset for issue year and sort them alphabetically
  names_year = data_year$Name %>% unique() %>% sort()
  ######################## Round 1: match all the school that already have exact names 
  unmodified_ind_match_1 = which(preferred_names %in% names_year)
  unmodified_ind_match_2 = which(names_year %in% preferred_names)
  ##### Result table 1
  df_1 = data.frame(preferred_names[unmodified_ind_match_1], names_year[unmodified_ind_match_2])
  ######################### Round 2: Implement fuzzy matching method by Jaro-Winkler distance
  ######### Find the indexes of schools that are not matched
  ind_unmatched_1 = which(!(preferred_names %in% names_year))
  ind_unmatched_2 = which(!(names_year %in% preferred_names))
  ######### Find the indexes of schools that are not matched
  ######## Exclude the school names that have already matched in both
  shorter_preferred_names = preferred_names[ind_unmatched_1]
  shorter_names_year = names_year[ind_unmatched_2]
  n = length(shorter_names_year)
  best_similarity_scores = numeric(n)
  best_matched_school_ind = numeric(n)
  for(i in seq_len(n)){
    #### Then, use max function to find the best score which mean the best match 
    similarity_scores = jarowinkler(shorter_names_year[i], shorter_preferred_names) 
    best_similarity_scores[i] = max(similarity_scores)
    best_matched_school_ind[i] = which.max(similarity_scores)
  }
  prospective_matched_schools = shorter_preferred_names[best_matched_school_ind]
  ######  Result table 2 using Jaro-Winkler distance
  df_2 = data.frame(shorter_names_year, 
                    prospective_matched_schools,
                    ind_unmatched_2, 
                    best_matched_school_ind,
                    best_similarity_scores) 
  df_2 = df_2[order(df_2$best_similarity_scores, decreasing = TRUE),]
  ######## Find the third quantile of the similarity scores vec.
  ####### Notice that the scores that are larger than Q3 tend to give the good result. 
  q3 = quantile(df_2$best_similarity_scores, 0.75)
  
  #####  Result table 3 containing school names that give correct matches
  df_3 = df_2[which(df_2$best_similarity_scores >= q3),] 
  ############################### Round 3: Implement fuzzy matching method by Levenshtein distance
  ####### Table include school names that we can not match after round 2
  unmatched_df =  df_2[which(df_2$best_similarity_scores < q3),]
  ##### Truncate the number of school names that we need to work on
  shorter_names_year_2 = shorter_names_year[!(shorter_names_year %in% df_3$shorter_names_year)]
  shorter_preferred_names_2 = shorter_preferred_names[!(shorter_preferred_names%in%df_3$prospective_matched_schools)]
  #### Then, use max function to find the best score which mean the best match 
  n2 = length(shorter_names_year_2)
  best_similarity_scores_2 = numeric(n2)
  best_matched_school_ind_2 = numeric(n2)
  for(i in seq_len(n2)){
    similarity_scores_2 = levenshteinSim(shorter_names_year_2[i], shorter_preferred_names_2) 
    best_similarity_scores_2[i] = max(similarity_scores_2)
    best_matched_school_ind_2[i] = which.max(similarity_scores_2)
  }
  prospective_matched_schools_2 = shorter_preferred_names_2[best_matched_school_ind_2]
  ######### Result table 4 using Levenshtein distance
  df_4 = data.frame(shorter_names_year_2, 
                    prospective_matched_schools_2,
                    best_similarity_scores_2)
  df_4 = df_4[order(df_4$best_similarity_scores_2, decreasing = TRUE), ]
  ######### Result table 5 containing school names that give correct matches
  df_5 = df_4[df_4$best_similarity_scores_2 >= 0.8260870,] 
  ########### Round 4: 
  ##### Truncate the number of school names 
  shorter_names_year_3 = shorter_names_year_2[!(shorter_names_year_2 %in% df_5$shorter_names_year_2)]
  shorter_preferred_names_3 = shorter_preferred_names_2[!(shorter_preferred_names_2 %in%df_5$prospective_matched_schools_2)]
  ########### Find new school names in the national university of the given year
  ######### Need data of previous year
  data_past_year = raw_data %>% 
    filter(Issue.year == year - 1, School.Type != "National Universities") %>% 
    group_by(Name) %>%
    select(Name, School.Type) %>%
    summarise(
      count = n()
    )
  new_school_ind = which(shorter_names_year_3 %in% data_past_year$Name)
  new_school_names = shorter_names_year_3[new_school_ind]
  ####### Truncate the number of school names need to match
  shorter_names_year_4 = shorter_names_year_3[which(!(shorter_names_year_3 %in% data_past_year$Name))] 
  ####### Remove common word of and the location parts of some particular cases 
  shorter_names_year_4_modified = shorter_names_year_4 %>% str_replace(" at", "") %>%
    str_replace(",.*", "") %>% 
    str_replace("University", "") %>% 
    str_replace("The ", "")
  shorter_preferred_names_3_modified = shorter_preferred_names_3 %>% str_replace_all(" College", "") %>% 
    str_replace("--", " ") %>% 
    str_replace("University", "")
  ########## Round 5: Implement fuzzy matching method by Jaro-Winkler distance again, but set a high threshold
  n3 = length(shorter_names_year_4_modified)
  best_similarity_scores_3 = numeric(n3)
  best_matched_school_ind_3 = numeric(n3)
  for(i in seq_len(n3)){
    similarity_scores = jarowinkler(shorter_names_year_4_modified[i], shorter_preferred_names_3_modified) 
    best_similarity_scores_3[i] = max(similarity_scores)
    best_matched_school_ind_3[i] = which.max(similarity_scores)
  }
  prospective_matched_schools_3 = shorter_preferred_names_3[best_matched_school_ind_3]
  ######  Result table 
  df_6 = data.frame(shorter_names_year_4, 
                    prospective_matched_schools_3,
                    best_similarity_scores_3) 
  df_6 = df_6[order(df_6$best_similarity_scores_3, decreasing = TRUE),]
  df_7 = df_6[df_6$best_similarity_scores_3 >=0.9085714,] 
  shorter_names_year_5 = shorter_names_year_4[!(shorter_names_year_4 %in% df_7$shorter_names_year_4)]
  ####### Create a final result
  vec1 = c(df_1$names_year.unmodified_ind_match_2., 
           df_3$shorter_names_year, 
           df_5$shorter_names_year_2, 
           df_7$shorter_names_year_4,
           new_school_names, 
           shorter_names_year_5)
  vec2 = c(df_1$preferred_names.unmodified_ind_match_1.,
           df_3$prospective_matched_schools,
           df_5$prospective_matched_schools_2,
           df_7$prospective_matched_schools_3, 
           new_school_names,
           shorter_names_year_5)
  
  vec3 = c(rep("Exact Match", length(df_1$preferred_names.unmodified_ind_match_1.)), 
           rep("Prospective Match", length(c(df_3$shorter_names_year, df_5$shorter_names_year_2,  df_7$shorter_names_year_4))), 
           rep("New School", length(new_school_names)), rep("Unmatched", length( shorter_names_year_5)))
  vec4 = c(rep(1,length(df_1$preferred_names.unmodified_ind_match_1.)),
           df_3$best_similarity_scores,
           df_5$best_similarity_scores_2,
           df_7$best_similarity_scores_3,
           rep(0,length(c(new_school_names, shorter_names_year_5))))
           # df_6$best_similarity_scores_3[df_6$shorter_names_year_4 %in% shorter_names_year_5])
   
  
  levels_vec3 = c("Exact Match", "Prospective Match", "New School", "Unmatched")
  final_result_df_1 = data.frame("Raw_Name" = vec1, 
                                 "Suggested_Name" = vec2, 
                                 "Math_Type" = factor(vec3, levels = levels_vec3),
                                 "Similarity_Score" = vec4)
  final_result_df_1 = final_result_df_1[order(final_result_df_1$Suggested_Name),]
#########################################################################################################
######### PART 2: METRIC MATCHING
  preferred_metrics = cleaned_data$`Metric Name` %>% unique() %>%  sort()
  raw_year_metrics = data_year$Metric.description %>% unique() %>% sort()
  ### Clean up the text, remove unnecessary parts
  raw_year_metrics_modified = raw_year_metrics %>% str_replace("\\(.*\\)", "")
  n4 = length(raw_year_metrics)
  best_similarity_scores_4 = numeric(n4)
  best_matched_metric_ind_4 = numeric(n4)
  for(i in seq_len(n4)){
    similarity_scores = jarowinkler(raw_year_metrics_modified[i], preferred_metrics) 
    best_similarity_scores_4[i] = max(similarity_scores)
    best_matched_metric_ind_4[i] = which.max(similarity_scores)
  }
  prospective_matched_metrics = preferred_metrics[best_matched_metric_ind_4]
  ######  Result table  using Jaro-Winkler distance
  final_result_df_2 = data.frame("Raw_Metric" = raw_year_metrics, 
                  "Suggested_Metric" = prospective_matched_metrics,
                  "Metric_Similarity_Score" = best_similarity_scores_4)
  final_result_df_2 = final_result_df_2[order(final_result_df_2$Metric_Similarity_Score, decreasing = TRUE),] 
  
  output = list("school_name_df" = final_result_df_1, "metric_df" = final_result_df_2)
  return(output)
}


########## Example how to utilize name_cleaning_function
# test = name_cleaning_function("2023-national-universities-2014-2023-metrics-for-stats-consulting-23W-2014-2023-T11-32-08-25-cs-2023-01-19T193212.csv",
#                                "Longitudinal USNWR ranking data.xlsx", 2023)
# schools =test$school_name_df
# metrics = test$metric_df
