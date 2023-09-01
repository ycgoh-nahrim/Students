# load packages
library(tidyverse)
library(lubridate)
library(scales)

library(openxlsx)
library(viridis)
library(ggpubr)


# set strings as factors to false
options(stringsAsFactors = FALSE)

###############################################################################
#INPUT
###############################################################################

##rainfall station data 
# daily rain data

#from csv
rain_db <- read.csv(file = "F:/Documents/demo_rainfall/RF_1d_2719001.csv",
                    header = TRUE, sep=",")
str(rain_db)


###########################

#select station
stn_no <- "2719001"
stn_name <- "Setor JPS Sikamat Seremban"


#minimum record per year/month for analysis
min_cnt_yr <- 347
min_cnt_mth <- 28


#chart subtitle
chart_subtitle <- paste0("Station ", stn_no, ": ", stn_name)

#chart caption
data_source <- "JPS"
chart_caption <- paste0("Data source: ", data_source)

#font
font_family <- "Roboto"

###########################
#set working directory
#set filename
filename2 <- paste0("RF", stn_no, "_daily")
# get current working directory
working_dir <- getwd()
dir.create(filename2)
setwd(filename2)


###########################

## select data
raindata_sel <- rain_db 

str(raindata_sel)


#if date is not in date format
raindata_sel$Date <- as.Date(raindata_sel$Date, format = "%Y-%m-%d")
#format column from character to numeric
raindata_sel$Depth <- as.numeric(as.character(raindata_sel$Depth))


# add columns for data aggregation
## split date to columns
raindata_sel <- raindata_sel %>%
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))


str(raindata_sel)

#minimum year
year_min <- min(raindata_sel$Year)
#maximum year
year_max <- max(raindata_sel$Year)
#total years
year_total <- year_max - year_min
#year interval
year_interval <- 5



###############################################################################

# CHECK RECORDS 

# remove years with missing data

## count non-missing data
raindata_cnt_mth <- raindata_sel %>%
  group_by(Year, Month) %>%
  summarise(Depth_mth = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth))) 


#format column from character to numeric
raindata_cnt_mth$cnt <- as.numeric(as.character(raindata_cnt_mth$cnt))


###########################

# DATA COUNT HEATMAP

rain_cnt_matrix <- raindata_cnt_mth %>%
  ggplot(aes(x = Year, y = Month)) +
  geom_tile(aes(fill = cnt)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, 
                       limits = c(1, 31), #set legend values?
                       name = "Record by month") +
  theme_bw(base_size = 10) +
  scale_y_continuous(#name = "Month",
                     breaks = seq(1, 12, by = 1), 
                     minor_breaks = NULL, 
                     trans = "reverse",
                     labels = month.abb) +
  scale_x_continuous(name = "Year",
                     breaks = seq(year_min, year_max, by = year_interval), 
                     minor_breaks = NULL) + 
  theme(text=element_text(family = "Roboto", size = 8,
                          color = "grey20"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom") +
  labs(title = paste0(stn_no, " ", stn_name, ": Data Availability")) 

rain_cnt_matrix

#print last plot to file
ggsave(paste0(filename2, "_cnt_heatmap_raw.jpg"), dpi = 300, 
       width = 30, height = 15, units = "cm")


###########################

# DATA SUMMARY


#summarise data

# calculate the total rain (mm) for each year
Rain_sum_yr <- raindata_sel %>%
  select(Year, Depth) %>%
  group_by(Year) %>%
  summarise(Depth_yr = sum(Depth, na.rm = T)) 


# sum all months
Rain_sum_mth <- raindata_sel %>%
  select(Year, Month, Depth) %>%
  group_by(Year, Month) %>%
  summarise(Depth_mth = sum(Depth, na.rm = T))


###########################

# DAILY RAINFALL

# plot daily data
rain_daily_plot <- raindata_sel %>% 
  ggplot(aes(x = Date, y = Depth)) +
  geom_point(alpha = 0.5, size = 1, color = "steelblue") +
  theme_bw(base_size = 10) +
  scale_x_date(name= "Year", date_labels = "%Y",
               date_breaks = paste0(year_interval, " year"),
               minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Daily rainfall (mm)"),
                     breaks = seq(0, 150, by = 20), 
                     minor_breaks = NULL) + #y axis format
  theme(text = element_text(color="grey20"),
        #legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = paste0("Daily rainfall"))

rain_daily_plot

#print last plot to file
ggsave(paste0(filename2, "_check_daily.jpg"), dpi = 300,
       width = 30, height = 20, units = "cm")


###########################

# ANNUAL RAINFAll

rain_annual_ly <- Rain_sum_yr %>% 
  ggplot(aes(x = Year, y = Depth_yr)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_bw(base_size = 10) +
  scale_x_continuous(name= "Year", 
                     #breaks = seq(0, 5000, by = 1000), 
                     minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Annual rainfall (mm)"),
                     breaks = seq(0, 3500, by = 500), 
                     limits = c(0, 3000),
                     minor_breaks = NULL) + #y axis format
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme(text=element_text(color="grey20"),
        #legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  labs(title = paste0(stn_no, " ", stn_name,": Annual rainfall" ))

rain_annual_ly

#print last plot to file
ggsave(paste0(filename2, "_annual_total.jpg"), dpi = 300, 
       width = 30, height = 20, units = "cm")


###########################

# AVERAGE MONTHLY RAINFALL

#summarize
rain_mth_avg <- Rain_sum_mth %>% 
  group_by(Month) %>% 
  summarise(avg_depth = mean(Depth_mth))


#plot

rain_mth_avg_plot <- rain_mth_avg %>%
  ggplot(aes(x = Month, y = avg_depth)) +
  geom_bar(stat = "identity", fill = "skyblue2", alpha = 0.8) +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Month",
                     breaks = seq(1, 12, by = 1), 
                     minor_breaks = NULL, 
                     labels=month.abb) + #x axis format
  scale_y_continuous(name= paste("Average monthly rainfall (mm)"),
                     breaks = seq(0, max(rain_mth_avg$avg_depth), by = 50), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  theme(text = element_text(family = font_family, color = "grey20"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5)) +
  labs(title = "Average monthly rainfall")

rain_mth_avg_plot

#print last plot to file
ggsave(paste0(filename2, "_mth_avg.jpg"), dpi = 300,
       width = 300, height = 210, units = "mm")


###########################
#boxplot label function from USGS
#modified with mean 

ggplot_box_legend <- function(family = font_family){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x, 
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile", 
                          "50th percentile\n(median)",
                          "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text", 
                       list(size = 2.8, 
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label", 
                       list(size = 2.8, 
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.25,
                 color = "grey40") +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values), 
                 width = 0.3, 
                 #fill = "lightgrey",
                 color = "steelblue3",
                 outlier.colour="red") +
    # number of observations
    #geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    #geom_text(aes(x = 1.17, y = 950,
    #              label = "Number of values"),
    #          fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    # mean point
    geom_point(aes(x = 1, y = 390),
               shape = 4, size = 2.5) + 
    geom_text(aes(x = 1.2, 
                  y =  390, 
                  label = "Mean"), 
              vjust = 0.5, fontface = "bold") +
    # quartiles lines
    geom_segment(aes(x = 2.3, xend = 2.3, 
                     y = ggplot_output[["25th percentile"]], 
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, 
                     y = ggplot_output[["25th percentile"]], 
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3, 
                     y = ggplot_output[["75th percentile"]], 
                     yend = ggplot_output[["75th percentile"]])) +
    # interquartile label
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]), 
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.3) +
    # whiskers lines
    geom_segment(aes(x = 1, xend = 1, 
                     y = ggplot_output[["upper_whisker"]], 
                     yend = ggplot_output[["lower_whisker"]]),
                 color = "grey58") +
    # whiskers label
    geom_text(aes(x = c(1.17,1.17), 
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]), 
                  label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
                            "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
              fontface = "bold", vjust = 0.9) +
    # outlier labels
    geom_text(aes(x = c(1.17), 
                  y =  ggplot_output[["lower_dots"]], 
                  label = "Outlier"), 
              vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(1.55), 
                  y =  ggplot_output[["lower_dots"]], 
                  label = ": Value is >1.5 times and"), 
              vjust = 0.5) +
    geom_text(aes(x = 1.17, 
                  y = ggplot_output[["lower_dots"]], 
                  label = "<3 times the interquartile range\nbeyond either end of the box"), 
              vjust = 1.5) +
    # quartiles labels
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]]+1, 
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.6,0.4), 
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 11)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")
  
  return(explain_plot) 
  
}

legend_plot <- ggplot_box_legend()


###########################

# MONTHLY RAINFALL VARIABILITY

#plot boxplot
Rain_sum_mth_box <- Rain_sum_mth %>%
  ggplot(aes(x = Month, y = Depth_mth, group = Month)) +
  geom_boxplot(outlier.colour="red", 
               outlier.shape=20,
               outlier.size=3,
               #fill = "white", 
               color = "steelblue3") +
  theme_bw(base_size = 10) +
  scale_x_continuous(name = "Month",
                     breaks = seq(1, 12, by = 1), 
                     minor_breaks = NULL, 
                     labels=month.abb) + #x axis format
  scale_y_continuous(name= paste("Monthly rainfall (mm)"),
                     breaks = seq(0, max(Rain_sum_mth$Depth_mth), by = 100), 
                     limits = c(0, NA),
                     minor_breaks = NULL) + #y axis format
  stat_summary(fun=mean, aes(shape = "Mean"), #mean value
               geom="point", size=2.5, show.legend = F) +
  stat_boxplot(geom ='errorbar', show.legend = F, width = 0.5,
               color = "grey58") +
  scale_shape_manual(name =NULL, values=c("Mean"=4)) +
  theme(text=element_text(
    color="grey20"),
    panel.grid.major.x = element_blank()) +
  labs(title = "Monthly rainfall variability")


#arrange chart and tables in 1 page
rain_mth_box <- ggarrange(Rain_sum_mth_box, legend_plot,
                          ncol = 2, nrow = 1, widths = c(3,1))

rain_mth_box


#print last plot to file
ggsave(paste0(filename2, "_rain_boxplot_mth.jpg"), 
       dpi = 300, width = 297, height = 210, units = "mm",
       rain_mth_box)

