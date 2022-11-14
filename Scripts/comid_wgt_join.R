library(readr)
library(dplyr)
library(tidyr)

clean_v2 <- read_csv(
  file = here::here("data",
                    "Final_Cleaned_MG_11_14_22.csv")
  )
comid_wgt <- read_csv(
  file = here::here("outputs",
                    "comid_wgt_all_years.csv")
)
comid_wgt$...1 <- NULL
comid <- separate(
  data = comid_wgt, 
  col = draw_year, 
  sep = "_",
  into = c("start", "end")
)

joined <- left_join(
  x = clean_v2, 
  y = comid, 
  by = "COMID"
)

sub_df <- filter(
  .data = joined, 
  YEAR >= start & YEAR <= end
)

anti_df <- anti_join(
  x = sub_df,
  y = clean_v2,
  by = c("COMID",
         "YEAR",
         "SMAS_ID")
)

test <- clean_v2[!(clean_v2$COMID %in% sub_df$COMID), ]

test$corrected_comid <- ""
test_adj <- test %>% 
  mutate(corrected_comid = case_when(
    COMID == "3246098"~"3246096",
    TRUE~""
  ),
  NOTES = case_when(
    COMID == "22294818"~ifelse(!is.na(NOTES), paste(NOTES, "Not sampled as probabilistic."), "Not sampled as probabilistic."),
    COMID == "4151524"~ifelse(!is.na(NOTES), paste(NOTES, "Not sampled as probabilistic."), "Not sampled as probabilistic."),
    COMID == "15567569"~ifelse(!is.na(NOTES), paste(NOTES, "Not sampled as probabilistic."), "Not sampled as probabilistic."),
    COMID == "15445117"~ifelse(!is.na(NOTES), paste(NOTES, "Not sampled as probabilistic."), "Not sampled as probabilistic."),
    COMID == "8119017"~ifelse(!is.na(NOTES), paste(NOTES, "Predates draws."), "Predates draws."),
    COMID == "22751809"~ifelse(!is.na(NOTES), paste(NOTES, "Predates draws."), "Predates draws."),
    COMID == "22741107"~ifelse(!is.na(NOTES), paste(NOTES, "Predates draws."), "Predates draws."),
    TRUE~NOTES
  ))

test_adj <- test_adj %>% filter(
  !is.na(SMAS_ID) 
)



dups <- sub_df %>% 
  group_by(COMID, YEAR) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

dups_adj <- dups %>% 
  mutate(keep = case_when(
    SMAS_ID == "06-GREA_W-1.1" & start == "2018"~TRUE,
    SMAS_ID == "10-MCKN-6.4" & start == "2013"~TRUE,
    SMAS_ID == "10-METT-17.7" & start == "2013"~TRUE,
    SMAS_ID == "10-RORE-2.3" & start == "2013"~TRUE,
    SMAS_ID == "17-BRNX-12.3" & start == "2013"~TRUE,
    SMAS_ID == "17-BRNX-9.2" & start == "2013"~TRUE,
    SMAS_ID == "17-MANH-1.2" & start == "2013"~TRUE,
    SMAS_ID == "17-MILR-3.5" & start == "2013"~TRUE,
    SMAS_ID == "17-PECN-3.2" & start == "2013"~TRUE,
    SMAS_ID == "13-NORM-7.5" & start == "2008"~TRUE,
    TRUE~FALSE
  )) %>% 
  filter(keep == TRUE)

#Removed and cleaned duplicates from clean_v2 to sub_df
#dups_adj needs keep col removed, then added to sub_df
#test_adj update comid, drop records without adj value, drop adj_comid field, add to sub_df

test_adj2 <- test_adj %>% 
  mutate(COMID == case_when(
    COMID == "3246098"~"3246096",
    TRUE~COMID
  )) %>% 
  filter(corrected_comid != "")

test_adj2$`COMID == case_when(COMID == "3246098" ~ "3246096", )` <-NULL
test_adj2$corrected_comid <- NULL

keep_field <- colnames(test_adj2)
sub_df_2 <- sub_df %>% 
select(
  anyof(keep_field)
)
sub_df_2 <- sub_df[, which((names(sub_df) %in% keep_field)==TRUE)]

dups_adj_2 <- dups_adj[, which((names(dups_adj) %in% keep_field)==TRUE)]

final <- rbind(dups_adj_2, test_adj2, sub_df_2)
