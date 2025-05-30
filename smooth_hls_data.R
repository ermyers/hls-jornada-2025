## NOTE: Before running this code, please run import_hls_data.R 
## or load the previously-imported HLS data

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# (Optional) Load previously-imported HLS data
load("outputs/hls_v20_ec_towers_clean.RData")

# Load packages
library(lubridate)
library(dplyr)
library(pracma)
library(ggplot2)

#############################
# SMOOTH THE HLS DATA (LOESS)
#############################

hls_data <- hls_center_clean
#hls_data <- cbind(hls_data,EVI_smooth=0)
hls_smooth_data <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(hls_smooth_data) <- c('DATE', 'YEAR', 'DOY', 'PHENOCAM_NAME', 'ECO_STATE',
                          'LOESS_SPAN', 'EVI_smooth', 'GCC_smooth')
loess_span <- 0.03

for (camera in unique(hls_data$PHENOCAM_NAME)){
  subset <- hls_data[hls_data$PHENOCAM_NAME == camera,]
  eco_state <- subset$ECO_STATE[1]
  subset$DATE <- as.numeric(subset$DATE)
  min_date <- min(subset$DATE, na.rm = TRUE)
  max_date <- max(subset$DATE, na.rm = TRUE)
  date_range <- seq(min_date,max_date,1)
  year <- year(as.Date(date_range, origin = "1970-01-01"))
  doy <- yday(as.Date(date_range, origin = "1970-01-01"))
  EVI_smooth <- predict(loess(EVI ~ DATE, data=subset, span=loess_span, control=loess.control(surface="direct")),newdata=date_range)
  GCC_smooth <- predict(loess(GCC ~ DATE, data=subset, span=loess_span, control=loess.control(surface="direct")),newdata=date_range)
  subset_smooth <- data.frame(DATE = as.Date(date_range,origin="1970-01-01"),
                              YEAR = year,
                              DOY = doy,
                              PHENOCAM_NAME = camera,
                              ECO_STATE = eco_state,
                              LOESS_SPAN = loess_span,
                              EVI_smooth = EVI_smooth,
                              GCC_smooth = GCC_smooth)
  hls_smooth_data <- rbind(hls_smooth_data,subset_smooth)
}

rm(hls_center_clean,camera,date_range,doy,EVI_smooth,GCC_smooth,loess_span,eco_state,max_date,min_date,year,subset,subset_smooth)

# Visual comparison of smoothed and non-smoothed results
ggplot() +
  geom_point(data = hls_data[hls_data$PHENOCAM_NAME=="jergrassland2",], aes(x=DATE,y=EVI)) +
  geom_line(data = hls_smooth_data[hls_smooth_data$PHENOCAM_NAME=="jergrassland2",], aes(x=DATE,y=EVI_smooth), color='red') +
  ggtitle("jergrassland2")

ggplot() +
  geom_point(data = hls_data[hls_data$PHENOCAM_NAME=="jernovel2",], aes(x=DATE,y=EVI)) +
  geom_line(data = hls_smooth_data[hls_smooth_data$PHENOCAM_NAME=="jernovel2",], aes(x=DATE,y=EVI_smooth), color='red') +
  ggtitle("jernovel2")

ggplot() +
  geom_point(data = hls_data[hls_data$PHENOCAM_NAME=="jershrubland2",], aes(x=DATE,y=EVI)) +
  geom_line(data = hls_smooth_data[hls_smooth_data$PHENOCAM_NAME=="jershrubland2",], aes(x=DATE,y=EVI_smooth), color='red') +
  ggtitle("jershrubland2")

# Save outputs
save(hls_data, hls_smooth_data, file="outputs/hls_ec_towers_smoothed.RData")
write.csv(hls_data,"outputs/hls_ec_towers.csv", row.names = FALSE)
write.csv(hls_smooth_data,"outputs/hls_ec_towers_smoothed.csv", row.names = FALSE)
