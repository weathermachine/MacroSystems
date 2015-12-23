setwd("C:/Users/Nick Reber/Dropbox/Investing Drop")

#download data and metadata
all_data <- read.csv("USCoinGDPStatDownload.Data.csv")
meta_data <- read.csv("USCoinGDPStatDownload.Metadata.csv")
         #rgdp_data <- read.csv("RGDPDownload.csv")

#format dates
#all_data <- left_join(all_data,rgdp_data)
all_data <- all_data %>% 
  mutate(obs_date = mdy(obs_date)) 
#all_data <- na.locf(all_data, fromLast=TRUE, maxgap = 3)

#make tall
tall_data <- melt(all_data, id = c("obs_date")) %>%
  mutate(Code = variable) %>%
  select(-variable)

#filter bad values
tall_data <- tall_data %>%
  filter(value != "NA") %>%
  mutate (value = as.numeric(value))

#add metadata
full_data <- inner_join(tall_data, meta_data, by = c("Code")) %>%
  mutate(Data_Lag = as.integer(Data_Lag),
         Code = as.factor(Code))

#create asof_date
full_data <- full_data %>%
  group_by(Code) %>%
  mutate(asof_date = obs_date %m+% months(Data_Lag)) %>%
  group_by(asof_date)

#select values and asof_date
full_data <- full_data %>%
  select(asof_date,value,Code) 


#Create Target and Factors data frames
target <- full_data %>% 
  filter(Code == "USARGDP") %>%
  spread(Code,value)

stats_data <- full_data %>% 
  filter(Code != "USARGDP") %>%
  spread(Code,value)


#Create RGDP 6m Change
#NEED TO ADD DATES TO THE RGDP CHANGE
target <- target %>%
  mutate(USARGDP_chng_offset = (USARGDP / lag(USARGDP,6)-1)*2,
         USARGDP_chng_offset = lead(USARGDP_chng_offset,3)) %>%
  rollz("USARGDP_chng_offset",120) 



#Create a data frame with the 1x6mma of each code (with different geo/arith for each).
stats_metadata <- meta_data %>%
  filter(!grepl('USARGDP', Code))
numberstats <- n_distinct(stats_metadata$Code)

factors <- stats_data

for (i in 1:numberstats) {
  thisstat <- paste(stats_metadata[i,"Code"])
  thisdifftype <- paste(stats_metadata[i,"DiffType"])
  if (thisdifftype != "Level") {
  factors <- factors %>%
    machange(thisstat,1,6,thisdifftype)
  }
}

factors <- factors %>%
  mutate(USAInitClaims_1x6mma_Inverted= USAInitClaims_1x6mma*-1)

factor_names <- c("USAAutoSales_1x6mma",
                  "USAConsConf_1x6mma",
                  "USAConstExpend_1x6mma",
                  "USADurGoods_1x6mma",
                  "USADurGoodsxTrans_1x6mma",
                  "USAEmpStateManuf",
                  "USAExistHomeSales_1x6mma",
                  "USAHomePermits_1x6mma",
                  "USAHomeStarts_1x6mma",
                  "USAIndProd_1x6mma",
                  "USAInitClaims_1x6mma_Inverted",
                  "USANAHB",
                  "USANewHomeSales_1x6mma",
                  "USANewOrdersManufxTrans_1x6mma",
                  "USAPendHomeSales_1x6mma",
                  "USAPMIChicago",
                  "USAPMIManuf",
                  "USAPMIServices",
                  "USARetSales_1x6mma",
                  "USATippEconOpt",
                  "USATotEmp_1x6mma")
                  

# remove the levels of the stats
factors <- factors %>% 
  melt(id = c("asof_date")) %>%
  filter(variable %in% factor_names) %>%
  spread(variable,value)

#create Z's
factors_z <- factors
numberstats <- n_distinct(factor_names)

for (i in 1:numberstats) {
  thisstat <- paste(factor_names[i])
  factors_z <- factors_z %>%
    rollz(thisstat,120)
}

#keep only the Z's
factors_z <- factors_z %>% 
  melt(id = c("asof_date")) %>%
  filter(grepl('_120mZ', variable)) 
  

#do aggregates NOTE: NEED TO PULL IN METADATA ON CONCEPT AND GROUP BY THOSE AND THEN AGGREGATE
#reZ (NEED TO ADD BETTER REZ LOGIC)
  agg <- factors_z %>%
  group_by (asof_date) %>%
  summarise(Estimate_Z = mean(value, na.rm=TRUE),
            Estimate_NumStats = n_distinct(variable)) %>%
  mutate(Estimate_reZ = Estimate_Z *1.2)
  
#add in target and make final estimate
all <- full_join(agg,target) %>%
  mutate(USARGDP_chng_offset_120mma = rollapply(USARGDP_chng_offset,120,mean,fill=NA,align="right"),
         USARGDP_chng_offset_120msd = rollapply(USARGDP_chng_offset,120,  sd,fill=NA,align="right"),
         USARGDP_chng_offset_120mma = lag(USARGDP_chng_offset_120mma,6),
         USARGDP_chng_offset_120msd = lag(USARGDP_chng_offset_120msd,6),
         Estimate_RGDPTerms = Estimate_reZ * USARGDP_chng_offset_120msd + USARGDP_chng_offset_120mma)

#add stats and export
factors_z <- factors_z %>%
  spread(variable,value)
all <- all %>% inner_join(factors_z)

write.csv(all,"coin_gr.csv")




#visualize
#NEED TO MAKE THE FOR LOOP MORE ROBUST HERE

p <- all %>%
  select(asof_date, USARGDP_chng_offset, Estimate_RGDPTerms) %>%
  gvisLineChart(options=list(width=1400, height=500,legend="top", vAxis="{format:'#.##%'}"))

for (i in 11:31) {
thisp <- all %>%
  select(asof_date, USARGDP_chng_offset_120mZ, i) %>%
  gvisLineChart(options=list(width=1400, height=500,legend="top"))
  p<- gvisMerge(p, thisp, horizontal = FALSE)

}

print(p, file="test.html")  

