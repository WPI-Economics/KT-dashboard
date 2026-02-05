library(highcharter)
library(tidyverse)
library(readxl)


#number format function
# Create your function
custom_number_format <- function(x){ifelse(x > 999999999,
                                           paste0("£",format(round((x/1000000000), 2), 
                                                        nsmall=1, big.mark=","), "bn"),
                                           ifelse(x > 999999, 
                                                  paste0("£",format(round((x/1000000), 1), 
                                                               nsmall=1, big.mark=","),"m"), 
                                                  format("£",round(x), nsmall=0, big.mark=",")))}

# Now try it out
custom_number_format(999)
custom_number_format(999999)
custom_number_format(1000000)
custom_number_format(999900000)
custom_number_format(1000000000)

#theme for charts
kt_theme <- hc_theme(
  chart = list(
    backgroundColor = "#FCFCF5", 
    style = list(
      fontFamily = "Roboto"
    )
  ),
  tooltip = list(
    style = list(
      fontFamily = "Arial"
    )
  ))



#####################
##################### 1.0 define colours
#####################

#colours
kt_colors <- c("#CC0033", #1 red
               "#1E1964", #2 blue
               "#28D796", #3 limegreen
               "#5009B0", #4 purple
               "#323232", #5 charcoal
               "#FCFCF5", #14 off white
              
               "#7a6da0", #7 mid-purple
               "#a69bbf", #8 light purple
               "#95e8bf", #9 mid green
               "#def6e9", #10 light green
               "#ddb6b3", #11 light red
               "#E6CF01", #12 yellow
               "#C3C3C3" ,#13 steel grey
               "#E6e6e6" #6 grey
               ) 

#####################
##################### 1.1 data read in and clean up
#####################

#read in the data and clean up a bit
df <- read_csv("db_data.csv") %>% 
  select(-`Salaries (-dw, attribution, and displacement) (£)`) %>% 
  rename("Economic value (GVA)" = `GVA (-dw, attribution, and displacement) (£)`,
         "Reduced re-offending" = `Savings from reduced reoffending (£)`,
         "DWP/health admin" = `Savings to DWP/Health (£)`,
         "Wellbeing" = `WELLBY (-dw and attribution) (£)`,
         "Total savings" = `Total savings (£)`
         ) %>% 
  mutate( `Volunteer value` = rowSums( across(c(`Volunteers (-attribution) (£)`, `Volunteer value (mentor and non-mentor) (£)`)), 
                                       na.rm = TRUE ),
    "Dummy" = NA,
    "group" = case_when(
      group == "all"~ "-",
      .default = group
    )
  ) %>% 
  select(-c(`Volunteers (-attribution) (£)`, `Volunteer value (mentor and non-mentor) (£)`)) %>% 
  relocate(`group_type`, .after = `group`) %>% 
  filter(`Cohort years` %in% c(#"2014/15", 
                               "2015/16", 
                               "2016/17", 
                               "2017/18", 
                               "2018/19", 
                               "2019/20", 
                               "2020/21", 
                               "2021/22", 
                               "2022/23", 
                               "2023/24", 
                               "2024/25")) %>% filter(!is.na(group)) %>% unique() %>% 
  
  #remove proposision and programme prefix from the value names
  mutate(
  "group" = case_when(
    str_detect(group, ":") ~
    word(group, 2, sep = ": "),
    TRUE ~ group))
  
  
  #0dp rounding
numeric_cols <- colnames(df[sapply(df, is.numeric)]) #identify numeric columns
df[numeric_cols] <- sapply(df[numeric_cols], function(x){round(x,0)})

#stash the years in a vector
fy_levels <- unique(df$`Cohort years`)

#subset the total/all group for a constant series in the chart
df_all <- df %>% filter(group == "-") #%>% na.omit()

df_ten_yr <- df %>% group_by(group, group_type) %>%
  summarise_if(is.numeric, sum)
  

#set up a color lookup for the chart
kt_sroi_colors_df <- tibble(
  subgroup = colnames(df)[4:ncol(df)],
  subgroup.colour = kt_colors[1:(ncol(df)-3)]
)

#background series always present
df_total <- df %>% filter(`Cohort years` == "2024/25") %>% 
  select(-c(`Cohort count`,`Total savings`, `Dummy`)) %>% 
  filter(group == "-"
  ) %>% 
  pivot_longer(
    cols = 4:ncol(.),
    values_to = "values",
    names_to = "names"
  ) %>% 
  arrange(desc(`values`))



