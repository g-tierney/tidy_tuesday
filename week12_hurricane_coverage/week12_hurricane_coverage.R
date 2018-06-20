####################
### Prepare Data ###
####################

require(tidyverse)
require(data.table)

hurricanes <- data.table(hurricane = c("Harvey","Irma","Maria"),
                        landfall = as.Date(c("2017-08-25","2017-09-10","2017-09-20")),
                        damage = c(125,64.76,91.61))
media_coverage <- read_csv("tidytuesday/data/week12_mediacloud_hurricanes.csv") 

# Center dates on landfall
media_coverage <- media_coverage %>% 
  mutate(Date = as.Date(Date,format = "%m/%d/%y"),
         harvey_days_from_lf = Date - hurricanes[hurricane == "Harvey",landfall],
         irma_days_from_lf = Date - hurricanes[hurricane == "Irma",landfall],
         maria_days_from_lf = Date - hurricanes[hurricane == "Maria",landfall]) %>% 
  mutate_at(vars(ends_with("from_lf")),as.integer)

# Calculate coverage within 5 days before and 10 days after landfall
coverage_within <- function(name,lower,upper,hc_data=hurricanes,mc_data=media_coverage){
  mentions <- select(mc_data,matches(name))
  lf <- hc_data %>% ungroup() %>% filter(hurricane == name) %>% select(landfall) 
  lf <- lf$landfall[1]
  filter <- lower <= mc_data$Date - lf & mc_data$Date - lf <= upper
  sum(mentions[filter,])
}
hurricanes <- hurricanes %>% group_by(hurricane) %>% 
  mutate(mentions_within_5_10 = coverage_within(hurricane,-5,10))

# Plot coverage by date
ggplot(data = media_coverage,aes(x=Date)) + ylab("Media Coverage") + 
  geom_line(aes(y=Harvey),color="orange") + geom_area(aes(y=Harvey),fill = "orange",alpha = .5) + 
  geom_line(aes(y=Irma),color="magenta") + geom_area(aes(y=Irma),fill = "magenta",alpha = .5) + 
  geom_line(aes(y=Maria),color="cyan") + geom_area(aes(y=Maria),fill = "cyan",alpha = .5)

#set color palet for hurricanes 
hc_colors <- c("Harvey" = "orange","Irma" = "magenta","Maria" = "cyan")

#################
### Plot Data ###
#################

# Plot coverage by days from landfall
mc_lf <- ggplot(data = media_coverage) + ylab("Media Coverage") + xlab("Days from U.S. Landfall") + 
  theme(plot.title = element_text(hjust = .5,face = "bold"),
        plot.subtitle = element_text(hjust = .5),
        plot.caption = element_text(hjust = 0),
        legend.position = "right") + 
  labs(title="Media Coverage Surrounding Hurricane Landfalls",
       caption="Media Coverage is the number of sentences mentioning each hurricane and the place it made landfall among \noutlets in Media Cloud's 'U.S. Top Online News' collection.") + 
  scale_x_continuous(limits = c(-5,10)) +
  geom_line(aes(harvey_days_from_lf,Harvey,color = "Harvey"),show.legend = T) + geom_area(aes(harvey_days_from_lf,y=Harvey,fill="Harvey"),alpha = .5) + 
  geom_line(aes(irma_days_from_lf,Irma,color = "Irma")) + geom_area(aes(irma_days_from_lf,Irma,fill="Irma"),alpha = .5) + 
  geom_line(aes(maria_days_from_lf,Maria,color="Maria")) + geom_area(aes(maria_days_from_lf,Maria,fill="Maria"),alpha = .5) + 
  scale_fill_manual(name= "Hurricanes",values = hc_colors) + scale_color_manual(name = "Hurricanes",values = hc_colors) 
mc_lf
ggsave("week12_hurricane_coverage/coverage_at_landfall.png",mc_lf)

# Plot coverage by damage
mc_damage <- ggplot(data = hurricanes,aes(damage,mentions_within_5_10,color=hurricane)) + 
  theme(plot.title = element_text(hjust = .5,face = "bold"),
        plot.subtitle = element_text(hjust = .5),
        plot.caption = element_text(hjust = 0),
        legend.position = "none") + 
  labs(title = "Hurricane Media Coverage and Damage",
       caption="Media Coverage is the number of sentences mentioning each hurricane and the place it made landfall among outlets in Media \nCloud's 'U.S. Top Online News' collection.") + 
  xlab("Damage (in billions of USD)") + ylab("Media Coverage") + 
  geom_point(size = 10) + geom_text(aes(label=hurricane),vjust = -2,color = "black") +
  scale_y_continuous(limits = c(2000,20000)) + scale_x_continuous(limits = c(60,130)) + 
  scale_color_manual(name = "Hurricanes",values = hc_colors) 
mc_damage
ggsave("week12_hurricane_coverage/coverage_by_damage.png",mc_damage)

