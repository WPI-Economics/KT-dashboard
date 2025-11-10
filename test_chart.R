library(highcharter)
library(tidyverse)
library(readxl)

#colours
kt_colors <- c("#CC0033", #red
               "#1E1964", #blue
               "#28D796", #limegreen
               "#5009B0", #purple
               "#323232", #charcoal
               "#F2F2F2") #light grey


#read in test data
df <- read_excel("Total savings year.xlsx") 
df[2:9] <- lapply(df[2:9], function(x){str_replace_all(x, "£", "")})
df[2:9] <- lapply(df[2:9], function(x){str_replace_all(x, ",", "")})
df[2:9] <- lapply(df[2:9], as.numeric) 
df <- df %>% na.omit()



#chart
# this sets up a object for the data labels
cht_data <- df$`GVA (-dw, attribution, and displacement) (£)`

# Create a list of points with dataLabels only on the last one, showing series.name
cht_series <- lapply(seq_along(cht_data), function(i) {
  if (i == length(cht_data)) {
    list(
      y = cht_data[i],
      dataLabels = list(
        enabled = TRUE,
        align = "left",
        y = 15,
        crop = F,
        overflow = "allow",
        format = "{series.name}"
      )
    )
  } else {
    list(y = cht_data[i])
  }
})


plot <- highchart() %>% 
  hc_chart(type = "column", spacingRight = 80) %>%
  
  hc_xAxis(categories = df$`Cohort years`,
           title = list(text = "")
           
  ) %>% 
  
  hc_add_series(name="Total SROI",
                data = (df$`Total savings (£)`),
                stack = "Main") %>%
  
  hc_add_series(data = cht_series, 
                type = "line",
                name = "Economic value",
                marker = list(symbol = 'circle'),
                dataLabels = list(enabled = F)) %>%
  
  hc_colors(rev(kt_colors))%>%
  hc_xAxis(title = list(text = ""))%>%
  hc_yAxis(title = list(text = "£")
           )%>%
  #hc_size(width = 500) %>% 
  hc_title(text = "", align = "left", 
           
           style = list(fontSize ="24px",#color = green.pair[1], 
                        fontFamily = "Arial", fontWeight = "400" ))%>% 
  hc_exporting(enabled = F) 

plot


