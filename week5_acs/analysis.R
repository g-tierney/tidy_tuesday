require(tidyverse)
require(openxlsx)
require(geofacet)
require(maps)
require(stargazer)

acs <- read.csv("week5_acs/acs2015_county_data.csv",stringsAsFactors = F)
acs <- acs %>% rename(fips = CensusId)
votes <- read.xlsx("week5_acs/US_County_Level_Presidential_Results_08-16.xlsx",sheet = 1)
votes <- votes %>% rename(fips = fips_code)


head(votes)


gcounty <- map_data("county")

#add fips
gcounty <- mutate(gcounty, polyname = paste(region, subregion, sep = ","))
gcounty <- left_join(gcounty, county.fips, "polyname")

#add acs data
gcounty <- left_join(gcounty, acs,"fips")
#add vote data
gcounty <- left_join(gcounty,votes,"fips")
gcounty <- gcounty %>% mutate(dem_2016_pct = dem_2016/total_2016*100)

#make county-level dataset
county_level <- inner_join(acs,votes,"fips") %>% mutate(dem_2016_pct = dem_2016/total_2016*100)

#pairwise vote correlations
standardize <- function(x){(x-mean(x,na.rm = T))/sd(x,na.rm = T)}
get_reg_statistic <- function(statistic,y,x,data){
  data <- data[,c(y,x)]
  data <- data %>% mutate_at(x,standardize) %>% #standardize x to make comparisons
    filter_all(all_vars(!is.na(.)))
  
  lm <- lm(paste0(y,"~",x),data = data)

  if(statistic == "correlation"){
    return(cor(data[,x],data[,y]))
  } else if(statistic == "coefficient"){
    coef(lm)[2]
  } else if(statistic == "rsquared"){
    summary(lm)$r.squared
  } else{
    print("Invalid statistic requested.")
  }
  
}
get_reg_statistic("coefficient","dem_2016_pct","Income",gcounty)

#make dataset of ACS variables
correlation_data <- data.frame(var = names(acs),stringsAsFactors = F) %>% filter(!(var %in% c("fips","State","County")),!str_detect(var,"Err$"))
correlation_data <- correlation_data %>% mutate(correlation_dem_2016 = sapply(var,get_reg_statistic,statistic="coefficient",y="dem_2016_pct",data=county_level),
                                                rsquared_dem_2016 = sapply(var,get_reg_statistic,statistic="rsquared",y="dem_2016_pct",data=county_level))
correlation_data %>% arrange(desc(abs(correlation_dem_2016)))

count_level_std <- mutate_at(county_level,vars(one_of(correlation_data$var)),standardize)

full_model <- lm(paste0("dem_2016_pct ~ ",paste0(correlation_data$var,collapse = " + ")),
                 data = count_level_std)
full_model_data <- summary(full_model)$coefficients %>% as.data.frame() %>% rownames_to_column(var = "var")
full_model_data %>% arrange(desc(abs(Estimate)))

results <- inner_join(correlation_data,full_model_data,"var")
results <- results %>% arrange(desc(abs(correlation_dem_2016)))

stargazer(full_model,type = "text")

plot <- ggplot(gcounty) + geom_polygon(aes(long,lat,group = group,fill = ChildPoverty))
plot

ggplot(data = county_level,aes(x=Poverty,y=ChildPoverty)) + geom_point()

require(tidyverse)
require(openxlsx)
require(geofacet)

### Load Data ###
acs <- read.csv("week5_acs/acs2015_county_data.csv",stringsAsFactors = F)
acs <- acs %>% rename(fips = CensusId)
votes <- read.xlsx("week5_acs/US_County_Level_Presidential_Results_08-16.xlsx",sheet = 1)
votes <- votes %>% rename(fips = fips_code)

#make county-level dataset
county_level <- inner_join(acs,votes,"fips")
county_level <- county_level %>% 
  mutate(dem_2016_pct = dem_2016/(dem_2016+gop_2016)*100, #turn vote totals into a percentage
         `Plurality Party` = ifelse(dem_2016_pct>50,"Democrat","Republican")) #to color points

#My election data doesn't include Alaskan Boroughs and Census Areas
my_us_state_grid <- us_state_grid3 %>% filter(code != "AK")

county_level %>% filter(!(State %in% c("District of Columbia"))) %>% #grid does not include DC
  ggplot(aes(x = log(IncomePerCap),y=dem_2016_pct)) + scale_x_continuous(breaks = seq(9,11,by=1)) + 
  geom_point(aes(color = `Plurality Party`),size = .5) + scale_color_manual(values = c("Democrat" = "blue","Republican" = "red")) +
  facet_geo(~State,grid = my_us_state_grid) + 
  theme(plot.title = element_text(hjust = .5,face = "bold"),
        plot.subtitle = element_text(hjust = .5),
        plot.caption = element_text(hjust = 0),
        legend.position = "right") +
  labs(title = "2016 Democratic Presidential Vote Share and Income per Capita by County and State",
       subtitle = "American Community Survey (2015) 5-year Estimates",
       y = "Democrat's Share of the Two-Party Presidential Vote",
       x = "Income per Capita (log scale)",
       caption = "By Graham Tierney, income data from Kaggle and election data from Tony McGovern's Github")

ggsave("week5_acs/vote_income.png",width = 14,height = 7.5)
