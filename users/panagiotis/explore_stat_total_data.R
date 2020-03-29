#################################
#                               #
#  Explore stat total data      #
#                               #
#################################


## Import stat_total data

ggplot(df) + 
  
  geom_line(aes(dates, total_sales), col = palette_light()[[1]], alpha = 0.5) +
  
  geom_rect(xmin = calendar$date[1] , xmax = calendar$date[1913],
            ymin = 60000, ymax = 60000,
            fill = "blue")  

stat_total %>% group_by(store_id) %>% summarise(n())
