######filtering regencies
rm( list = ls(all = TRUE ) )
#import library
library(stringr)

#get working directory
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

#historical data directory
historical_dir <- paste0(
  substr(script_dir, 1, unlist(gregexpr("/2. script", script_dir))),
  "\\1. historical Indonesia"
)

#import village to pixel table
reference_table <- read.csv(paste0(
  historical_dir,
  "\\village_to_pixel_final gsmap.csv"
))

#load the GSMaP historical data
load(paste0(
  historical_dir,
  "\\final_historical_indo_gsmap_daily.RData"
))

#define the regency
regencies <- c("Karawang")

##check every regency
'%!in%' <- Negate("%in%")
for(i in regencies){
  if (i %!in% reference_table$regency){
    print(i)
  }
}

for(i in regencies){
  regency_table <- reference_table[which(reference_table$regency == i),]
  pixel_regency <- unique(regency_table$pixel_name)
  
  hist_regency <- hist_data_gsmap_Indo_daily[,which(colnames(hist_data_gsmap_Indo_daily) %in% pixel_regency)]
  hist_regency <- cbind(hist_data_gsmap_Indo_daily$date, hist_regency)
  colnames(hist_regency)[1] <- "date"
  
  write.csv(hist_regency, paste0(historical_dir,"\\final_historical_", i,"_gsmap.csv"), row.names = FALSE)
  write.csv(regency_table, paste0(historical_dir, "\\regency_table_", i,"_gsmap.csv"), row.names = FALSE)
}
