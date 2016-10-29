# Curtis Kephart
# Visualizing national polling


library(ggplot2) # 2.2
library(dplyr)
library(tidyr)

# load data #####
polls = read.csv(
  file = "data/RCP_quickscrape.csv"
)
# replace with API or web-scraper



# clean up data #####
polls <-   polls %>%
  separate(Date, c("start","end"), sep = " - " ) %>%
  separate(Sample, c("sampleSize","sampleType"), sep = " " ) %>%
  mutate(
    sampleSize = ifelse(sampleSize %in% c("LV","RV"), NA, sampleSize),
    sampleSize = as.numeric(sampleSize),
    start = as.Date(paste(start,"/",year,sep=""), format = "%m/%d/%Y"),
    end = as.Date(paste(end,"/",year,sep=""), format = "%m/%d/%Y"),
    MoE = as.numeric(as.character(MoE)),
    Clinton..D. = as.numeric(as.character(Clinton..D.)),
    Trump..R. = as.numeric(as.character(Trump..R.))
  ) %>%
  tbl_df()


# Setup pretty(pretty ish?) gradient for margin of error ####

alpha_start = 0.2
alpha_end = 0
n = 20


# dems ------
data <- polls %>%
  filter( 
    !is.na(MoE))

rect_total_D = data.frame() %>% tbl_df()
for (i in 1:length(data$Poll)){
  temp = data[i,]
  y_steps <- seq(
    from = temp$Clinton..D., 
    to = (temp$Clinton..D. + temp$MoE), 
    length.out = n + 1)
  alpha_steps <- seq(
    from = alpha_start, 
    to = alpha_end, 
    length.out = n)
  rect_grad <- data.frame(ymin = y_steps[-(n + 1)], 
                          ymax = y_steps[-1], 
                          alpha = alpha_steps)
  
  
  y_steps <- seq(
    from = temp$Clinton..D., 
    to = (temp$Clinton..D. - temp$MoE), 
    length.out = n + 1)
  alpha_steps <- seq(
    from = alpha_start,
    to = alpha_end, 
    length.out = n)
  rect_grad <- rbind(
    data.frame(ymin = y_steps[-(n + 1)], 
               ymax = y_steps[-1], 
               alpha = alpha_steps
    ),
    rect_grad
  )
  
  temp = merge(temp,rect_grad)
  
  
  
  rect_total_D = bind_rows(
    rect_total_D,
    temp
  )
  
}


# reps -----
data <- polls %>%
  filter( 
    !is.na(MoE))

rect_total_R = data.frame() %>% tbl_df()
for (i in 1:length(data$Poll)){
  temp = data[i,]
  y_steps <- seq(
    from = temp$Trump..R., 
    to = (temp$Trump..R. + temp$MoE), 
    length.out = n + 1)
  alpha_steps <- seq(
    from = alpha_start, 
    to = alpha_end, 
    length.out = n)
  rect_grad <- data.frame(ymin = y_steps[-(n + 1)], 
                          ymax = y_steps[-1], 
                          alpha = alpha_steps)
  
  
  y_steps <- seq(
    from = temp$Trump..R., 
    to = (temp$Trump..R. - temp$MoE), 
    length.out = n + 1)
  alpha_steps <- seq(
    from = alpha_start,
    to = alpha_end, 
    length.out = n)
  rect_grad <- rbind(
    data.frame(ymin = y_steps[-(n + 1)], 
               ymax = y_steps[-1], 
               alpha = alpha_steps
    ),
    rect_grad
  )
  
  temp = merge(temp,rect_grad)
  
  
  
  rect_total_R = bind_rows(
    rect_total_R,
    temp
  )
  
}



ggplot(
  data = polls,
  aes(x = end)
) +
  geom_point(
    aes(
      x = end-1,
      y = Clinton..D.
    ),
    color = "blue",
    alpha = 0.3
  )   +
  # geom_smooth(
  #   aes(
  #     x = end,
  #     y = Clinton..D.
  #   ),
  #   color = "blue",
  #   se=F,
  #   span = 0.1
  # ) +
  geom_rect(data=rect_total_D, 
            aes(xmin=start, xmax=end,
                ymin=ymin, 
                ymax=ymax, 
                alpha=I(alpha)), fill="blue") +
  guides(alpha = FALSE) +
  geom_point(
    aes(
      x = end-1,
      y = Trump..R.
    ),
    color = "red",
    alpha = 0.3
  )   +
  # geom_smooth(
  #   aes(
  #     x = end,
  #     y = Trump..R.
  #   ),
  #   color = "red",
  #   se=F,
  #   span = 0.1
  # ) +
  geom_rect(data=rect_total_R, 
            aes(xmin=start, xmax=end,
                ymin=ymin, 
                ymax=ymax, 
                alpha=I(alpha)), fill="red") +
  guides(alpha = FALSE) +
  coord_cartesian(
    xlim = c(as.Date("2016-08-30"), 
             as.Date("2016-11-08")))+
  theme_bw()



