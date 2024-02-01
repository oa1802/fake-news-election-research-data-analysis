install.packages("ggplot2"); install.packages("tidyverse"); install.packages("lubridate"); install.packages("scales"); library("ggplot2"); library("tidyverse"); library("lubridate"); library("scales");
engagement_weighted_iffy <- read_csv(file.choose());
engagement_weighted_iffy$Average <- (engagement_weighted_iffy$Facebook + engagement_weighted_iffy$Twitter)/2
year2016 <- subset(engagement_weighted_iffy, DateTime >= "2016-01-01 00:00:00" & DateTime <= "2016-12-31 00:00:00");
year2017 <- subset(engagement_weighted_iffy, DateTime >= "2017-01-01 00:00:00" & DateTime <= "2017-12-31 00:00:00");
year2018 <- subset(engagement_weighted_iffy, DateTime >= "2018-01-01 00:00:00" & DateTime <= "2018-12-31 00:00:00");
year2019 <- subset(engagement_weighted_iffy, DateTime >= "2019-01-01 00:00:00" & DateTime <= "2019-12-31 00:00:00");
year2020 <- subset(engagement_weighted_iffy, DateTime >= "2020-01-01 00:00:00" & DateTime <= "2020-12-31 00:00:00");
all_years_average <- subset(engagement_weighted_iffy, DateTime >= "2016-01-01 00:00:00" & DateTime <= "2016-12-31 00:00:00" |
	DateTime >= "2017-01-01 00:00:00" & DateTime <= "2017-12-31 00:00:00" |
	DateTime >= "2018-01-01 00:00:00" & DateTime <= "2018-12-31 00:00:00" |
	DateTime >= "2019-01-01 00:00:00" & DateTime <= "2019-12-31 00:00:00" |
	DateTime >= "2020-01-01 00:00:00" & DateTime <= "2020-12-31 00:00:00"
);
election_years_average <- subset(engagement_weighted_iffy, DateTime >= "2016-01-01 00:00:00" & DateTime <= "2016-12-31 00:00:00" |
	DateTime >= "2018-01-01 00:00:00" & DateTime <= "2018-12-31 00:00:00" |
	DateTime >= "2020-01-01 00:00:00" & DateTime <= "2020-12-31 00:00:00"
);
presidential_election_years_average <- subset(engagement_weighted_iffy, DateTime >= "2016-01-01 00:00:00" & DateTime <= "2016-12-31 00:00:00" |
	DateTime >= "2020-01-01 00:00:00" & DateTime <= "2020-12-31 00:00:00"
);
year(year2016$DateTime) <- 0000; year(year2017$DateTime) <- 0000; year(year2018$DateTime) <- 0000; year(year2019$DateTime) <- 0000; year(year2020$DateTime) <- 0000; year(all_years_average$DateTime) <- 0000; year(election_years_average$DateTime) <- 0000; year(presidential_election_years_average$DateTime) <- 0000;
colors <- c("2016" = "#0072B2", "2017" = "#CC79A7", "2018" = "#D55E00", "2019" = "#56B4E9", "2020" = "#009E73", "Average" = "#E69F00")

starter_plot = ggplot() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(labels=date_format("%B"));	

#########################################

# 2016-2020 Facebook Engagement-weighted Iffy Quotient by Year	
starter_plot +   
	ggtitle("2016-2020 Facebook Engagement-weighted Iffy Quotient by Year") +
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +
    geom_point(aes(x = year2016$DateTime, y = year2016$Facebook, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Facebook, color="2016")) +
	geom_point(aes(x = year2017$DateTime, y = year2017$Facebook, color="2017")) +
	geom_smooth(aes(x = year2017$DateTime, y = year2017$Facebook, color="2017")) +
	geom_point(aes(x = year2018$DateTime, y = year2018$Facebook, color="2018")) +
	geom_smooth(aes(x = year2018$DateTime, y = year2018$Facebook, color="2018")) +
	geom_point(aes(x = year2019$DateTime, y = year2019$Facebook, color="2019")) +
	geom_smooth(aes(x = year2019$DateTime, y = year2019$Facebook, color="2019")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Facebook, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Facebook, color="2020")) +
	geom_smooth(aes(x = all_years_average$DateTime, y = all_years_average$Facebook, color="Average"), size=3) +
	scale_color_manual(values = colors);
	
# 2016, 2018 and 2020 Facebook Engagement-weighted Iffy Quotient by Year
starter_plot +
    ggtitle("2016, 2018 and 2020 Facebook Engagement-weighted Iffy Quotient by Year") +
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +  
    geom_point(aes(x = year2016$DateTime, y = year2016$Facebook, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Facebook, color="2016")) +
	geom_point(aes(x = year2018$DateTime, y = year2018$Facebook, color="2018")) +
	geom_smooth(aes(x = year2018$DateTime, y = year2018$Facebook, color="2018")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Facebook, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Facebook, color="2020")) +
	geom_smooth(aes(x = election_years_average$DateTime, y = election_years_average$Facebook, color="Average"), size=3) +
	scale_color_manual(values = colors);	
	
# 2016 and 2020 Facebook Engagement-weighted Iffy Quotient by Year
starter_plot +
    ggtitle("2016 and 2020 Facebook Engagement-weighted Iffy Quotient by Year") +
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +  
    geom_point(aes(x = year2016$DateTime, y = year2016$Facebook, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Facebook, color="2016")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Facebook, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Facebook, color="2020")) +
	geom_smooth(aes(x = presidential_election_years_average$DateTime, y = presidential_election_years_average$Facebook, color="Average"), size=3) +
	scale_color_manual(values = colors);	
	
# 2016-2020 Twitter Engagement-weighted Iffy Quotient by Year
starter_plot +
    ggtitle("2016-2020 Twitter Engagement-weighted Iffy Quotient by Year") +
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") + 
    geom_point(aes(x = year2016$DateTime, y = year2016$Twitter, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Twitter, color="2016")) +
	geom_point(aes(x = year2017$DateTime, y = year2017$Twitter, color="2017")) +
	geom_smooth(aes(x = year2017$DateTime, y = year2017$Twitter, color="2017")) +
	geom_point(aes(x = year2018$DateTime, y = year2018$Twitter, color="2018")) +
	geom_smooth(aes(x = year2018$DateTime, y = year2018$Twitter, color="2018")) +
	geom_point(aes(x = year2019$DateTime, y = year2019$Twitter, color="2019")) +
	geom_smooth(aes(x = year2019$DateTime, y = year2019$Twitter, color="2019")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Twitter, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Twitter, color="2020")) +
	geom_smooth(aes(x = all_years_average$DateTime, y = all_years_average$Twitter, color="Average"), size=3) +
	scale_color_manual(values = colors);
	
# 2016, 2018 and 2020 Twitter Engagement-weighted Iffy Quotient by Year
starter_plot +
    ggtitle("2016, 2018 and 2020 Twitter Engagement-weighted Iffy Quotient by Year") +
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +  
    geom_point(aes(x = year2016$DateTime, y = year2016$Twitter, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Twitter, color="2016")) +
	geom_point(aes(x = year2018$DateTime, y = year2018$Twitter, color="2018")) +
	geom_smooth(aes(x = year2018$DateTime, y = year2018$Twitter, color="2018")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Twitter, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Twitter, color="2020")) +
	geom_smooth(aes(x = election_years_average$DateTime, y = election_years_average$Twitter, color="Average"), size=3) +
	scale_color_manual(values = colors);
	
# 2016 and 2020 Twitter Engagement-weighted Iffy Quotient by Year
starter_plot +
    ggtitle("2016 and 2020 Twitter Engagement-weighted Iffy Quotient by Year") +  
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +  
    geom_point(aes(x = year2016$DateTime, y = year2016$Twitter, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Twitter, color="2016")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Twitter, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Twitter, color="2020")) +
	geom_smooth(aes(x = presidential_election_years_average$DateTime, y = presidential_election_years_average$Twitter, color="Average"), size=3) +
	scale_color_manual(values = colors);
	
# 2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year
starter_plot +
    ggtitle("2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year") + 
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +  
    geom_point(aes(x = year2016$DateTime, y = year2016$Average, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Average, color="2016")) +
	geom_point(aes(x = year2017$DateTime, y = year2017$Average, color="2017")) +
	geom_smooth(aes(x = year2017$DateTime, y = year2017$Average, color="2017")) +
	geom_point(aes(x = year2018$DateTime, y = year2018$Average, color="2018")) +
	geom_smooth(aes(x = year2018$DateTime, y = year2018$Average, color="2018")) +
	geom_point(aes(x = year2019$DateTime, y = year2019$Average, color="2019")) +
	geom_smooth(aes(x = year2019$DateTime, y = year2019$Average, color="2019")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Average, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Average, color="2020")) +
	geom_smooth(aes(x = all_years_average$DateTime, y = all_years_average$Average, color="Average"), size=3) +
	scale_color_manual(values = colors);
	
# 2016, 2018 and 2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year
starter_plot +
    ggtitle("2016, 2018 and 2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year") + 
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +
    geom_point(aes(x = year2016$DateTime, y = year2016$Average, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Average, color="2016")) +
	geom_point(aes(x = year2018$DateTime, y = year2018$Average, color="2018")) +
	geom_smooth(aes(x = year2018$DateTime, y = year2018$Average, color="2018")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Average, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Average, color="2020")) +
	geom_smooth(aes(x = election_years_average$DateTime, y = election_years_average$Average, color="Average"), size=3) +
	scale_color_manual(values = colors);
	
# 2016 and 2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year
starter_plot +
    ggtitle("2016 and 2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year") +
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +
    geom_point(aes(x = year2016$DateTime, y = year2016$Average, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Average, color="2016")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Average, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Average, color="2020")) +
	geom_smooth(aes(x = presidential_election_years_average$DateTime, y = presidential_election_years_average$Average, color="Average"), size=3) +
	scale_color_manual(values = colors);	
	
#######################################
	
#Facebook Seasonality
install.packages("ggplot2"); install.packages("tidyverse");
install.packages("lubridate");
install.packages("scales")
library("ggplot2");
library("tidyverse");
library("lubridate");
library("scales");
engagement_weighted_iffy <- read_csv(file.choose());
year2016 <- subset(engagement_weighted_iffy, DateTime >= "2016-01-01 00:00:00" & DateTime <= "2016-12-31 00:00:00");
year2017 <- subset(engagement_weighted_iffy, DateTime >= "2017-01-01 00:00:00" & DateTime <= "2017-12-31 00:00:00");
year2018 <- subset(engagement_weighted_iffy, DateTime >= "2018-01-01 00:00:00" & DateTime <= "2018-12-31 00:00:00");
year2019 <- subset(engagement_weighted_iffy, DateTime >= "2019-01-01 00:00:00" & DateTime <= "2019-12-31 00:00:00");
year2020 <- subset(engagement_weighted_iffy, DateTime >= "2020-01-01 00:00:00" & DateTime <= "2020-12-31 00:00:00");
year(year2016$DateTime) <- 0000;
year(year2017$DateTime) <- 0000;
year(year2018$DateTime) <- 0000;
year(year2019$DateTime) <- 0000;
year(year2020$DateTime) <- 0000;
colors <- c("2016" = "#0072B2", "2017" = "#CC79A7", "2018" = "#D55E00", "2019" = "#56B4E9", "2020" = "#009E73")
ggplot() +
    theme_bw() +
    ggtitle("Facebook Engagement-weighted Iffy Quotient by Year") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("") +
    ylab("Percentage engagement with URLs from iffy sites") +
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +
    scale_x_datetime(labels=date_format("%B")) +
    geom_point(aes(x = year2016$DateTime, y = year2016$Facebook, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Facebook, color="2016")) +
	geom_point(aes(x = year2017$DateTime, y = year2017$Facebook, color="2017")) +
	geom_smooth(aes(x = year2017$DateTime, y = year2017$Facebook, color="2017")) +
	geom_point(aes(x = year2018$DateTime, y = year2018$Facebook, color="2018")) +
	geom_smooth(aes(x = year2018$DateTime, y = year2018$Facebook, color="2018")) +
	geom_point(aes(x = year2019$DateTime, y = year2019$Facebook, color="2019")) +
	geom_smooth(aes(x = year2019$DateTime, y = year2019$Facebook, color="2019")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Facebook, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Facebook, color="2020")) +
	scale_color_manual(values = colors);
	
#Twitter Seasonality
install.packages("ggplot2");
install.packages("tidyverse");
install.packages("lubridate");
install.packages("scales")
library("ggplot2");
library("tidyverse");
library("lubridate");
library("scales");
engagement_weighted_iffy <- read_csv(file.choose());
year2016 <- subset(engagement_weighted_iffy, DateTime >= "2016-01-01 00:00:00" & DateTime <= "2016-12-31 00:00:00");
year2017 <- subset(engagement_weighted_iffy, DateTime >= "2017-01-01 00:00:00" & DateTime <= "2017-12-31 00:00:00");
year2018 <- subset(engagement_weighted_iffy, DateTime >= "2018-01-01 00:00:00" & DateTime <= "2018-12-31 00:00:00");
year2019 <- subset(engagement_weighted_iffy, DateTime >= "2019-01-01 00:00:00" & DateTime <= "2019-12-31 00:00:00");
year2020 <- subset(engagement_weighted_iffy, DateTime >= "2020-01-01 00:00:00" & DateTime <= "2020-12-31 00:00:00");
year(year2016$DateTime) <- 0000;
year(year2017$DateTime) <- 0000;
year(year2018$DateTime) <- 0000;
year(year2019$DateTime) <- 0000;
year(year2020$DateTime) <- 0000;
colors <- c("2016" = "#0072B2", "2017" = "#CC79A7", "2018" = "#D55E00", "2019" = "#56B4E9", "2020" = "#009E73")
ggplot() +
    theme_bw() +
    ggtitle("Twitter Engagement-weighted Iffy Quotient by Year") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("") +
    ylab("Percentage engagement with URLs from iffy sites") +
	labs(x = "Months", y = "Percentage engagement with URLs from iffy sites", color = "Legend") +
    scale_x_datetime(labels=date_format("%B")) +
    geom_point(aes(x = year2016$DateTime, y = year2016$Twitter, color="2016")) +
	geom_smooth(aes(x = year2016$DateTime, y = year2016$Twitter, color="2016")) +
	geom_point(aes(x = year2017$DateTime, y = year2017$Twitter, color="2017")) +
	geom_smooth(aes(x = year2017$DateTime, y = year2017$Twitter, color="2017")) +
	geom_point(aes(x = year2018$DateTime, y = year2018$Twitter, color="2018")) +
	geom_smooth(aes(x = year2018$DateTime, y = year2018$Twitter, color="2018")) +
	geom_point(aes(x = year2019$DateTime, y = year2019$Twitter, color="2019")) +
	geom_smooth(aes(x = year2019$DateTime, y = year2019$Twitter, color="2019")) +
	geom_point(aes(x = year2020$DateTime, y = year2020$Twitter, color="2020")) +
	geom_smooth(aes(x = year2020$DateTime, y = year2020$Twitter, color="2020")) +
	scale_color_manual(values = colors);