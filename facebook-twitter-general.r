install.packages("ggplot2");
install.packages("tidyverse");
install.packages("lubridate");
install.packages("scales");
library("ggplot2");
library("tidyverse");
library("lubridate");
library("scales");
engagement_weighted_iffy <- read_csv(file.choose());
engagement_weighted_iffy$Average <- (engagement_weighted_iffy$Facebook + engagement_weighted_iffy$Twitter) / 2
colors <- c("Facebook" = "#0072B2", "Twitter" = "#009E73", "Average" = "#E69F00")

##################

ggplot() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +  
	ggtitle("2016-2020 Facebook and Twitter Engagement-weighted Iffy Quotient") +
	labs(x = "Years", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +
	scale_x_datetime(date_labels="%Y",date_breaks  ="1 year") +
    geom_point(aes(x = engagement_weighted_iffy$DateTime, y = engagement_weighted_iffy$Facebook, color="Facebook")) +
	geom_smooth(aes(x = engagement_weighted_iffy$DateTime, y = engagement_weighted_iffy$Facebook, color="Facebook")) +
	geom_point(aes(x = engagement_weighted_iffy$DateTime, y = engagement_weighted_iffy$Twitter, color="Twitter")) +
	geom_smooth(aes(x = engagement_weighted_iffy$DateTime, y = engagement_weighted_iffy$Twitter, color="Twitter")) +
	geom_smooth(aes(x = engagement_weighted_iffy$DateTime, y = engagement_weighted_iffy$Average, color="Average"), size=3) +
	scale_color_manual(values = colors, breaks=c("Facebook", "Twitter", "Average"))
	
ggplot(engagement_weighted_iffy,aes(x=Facebook)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +  
    ggtitle("2016-2020 Facebook Engagement-weighted Iffy Quotient") +
    labs(x = "Values", y = "Frequency") +
    geom_histogram(breaks=seq(4, 24), color="#000000", fill="#0072B2")
	
ggplot(engagement_weighted_iffy,aes(x=Twitter)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +  
    ggtitle("2016-2020 Twitter Engagement-weighted Iffy Quotient") +
    labs(x = "Values", y = "Frequency") +
    geom_histogram(breaks=seq(4, 23), color="#000000", fill="#0072B2")