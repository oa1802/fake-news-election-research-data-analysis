install.packages("ggplot2"); install.packages("tidyverse"); install.packages("lubridate"); install.packages("scales");
library("ggplot2"); library("tidyverse"); library("lubridate"); library("scales");
engagement_weighted_iffy <- read_csv(file.choose());
year2016 <- subset(engagement_weighted_iffy, DateTime >= "2016-01-01 00:00:00" & DateTime <= "2016-12-31 00:00:00");
year2017 <- subset(engagement_weighted_iffy, DateTime >= "2017-01-01 00:00:00" & DateTime <= "2017-12-31 00:00:00");
year2018 <- subset(engagement_weighted_iffy, DateTime >= "2018-01-01 00:00:00" & DateTime <= "2018-12-31 00:00:00");
year2019 <- subset(engagement_weighted_iffy, DateTime >= "2019-01-01 00:00:00" & DateTime <= "2019-12-31 00:00:00");
year2020 <- subset(engagement_weighted_iffy, DateTime >= "2020-01-01 00:00:00" & DateTime <= "2020-12-31 00:00:00");
year2016quarter1 <- subset(engagement_weighted_iffy, DateTime >= "2016-01-01 00:00:00" & DateTime <= "2016-03-31 00:00:00");
year2016quarter2 <- subset(engagement_weighted_iffy, DateTime >= "2016-04-01 00:00:00" & DateTime <= "2016-06-30 00:00:00");
year2016quarter3 <- subset(engagement_weighted_iffy, DateTime >= "2016-07-01 00:00:00" & DateTime <= "2016-09-30 00:00:00");
year2016quarter4 <- subset(engagement_weighted_iffy, DateTime >= "2016-10-01 00:00:00" & DateTime <= "2016-12-31 00:00:00");
year2017quarter1 <- subset(engagement_weighted_iffy, DateTime >= "2017-01-01 00:00:00" & DateTime <= "2017-03-31 00:00:00");
year2017quarter2 <- subset(engagement_weighted_iffy, DateTime >= "2017-04-01 00:00:00" & DateTime <= "2017-06-30 00:00:00");
year2017quarter3 <- subset(engagement_weighted_iffy, DateTime >= "2017-07-01 00:00:00" & DateTime <= "2017-09-30 00:00:00");
year2017quarter4 <- subset(engagement_weighted_iffy, DateTime >= "2017-10-01 00:00:00" & DateTime <= "2017-12-31 00:00:00");
year2018quarter1 <- subset(engagement_weighted_iffy, DateTime >= "2018-01-01 00:00:00" & DateTime <= "2018-03-31 00:00:00");
year2018quarter2 <- subset(engagement_weighted_iffy, DateTime >= "2018-04-01 00:00:00" & DateTime <= "2018-06-30 00:00:00");
year2018quarter3 <- subset(engagement_weighted_iffy, DateTime >= "2018-07-01 00:00:00" & DateTime <= "2018-09-30 00:00:00");
year2018quarter4 <- subset(engagement_weighted_iffy, DateTime >= "2018-10-01 00:00:00" & DateTime <= "2018-12-31 00:00:00");
year2019quarter1 <- subset(engagement_weighted_iffy, DateTime >= "2019-01-01 00:00:00" & DateTime <= "2019-03-31 00:00:00");
year2019quarter2 <- subset(engagement_weighted_iffy, DateTime >= "2019-04-01 00:00:00" & DateTime <= "2019-06-30 00:00:00");
year2019quarter3 <- subset(engagement_weighted_iffy, DateTime >= "2019-07-01 00:00:00" & DateTime <= "2019-09-30 00:00:00");
year2019quarter4 <- subset(engagement_weighted_iffy, DateTime >= "2019-10-01 00:00:00" & DateTime <= "2019-12-31 00:00:00");
year2020quarter1 <- subset(engagement_weighted_iffy, DateTime >= "2020-01-01 00:00:00" & DateTime <= "2020-03-31 00:00:00");
year2020quarter2 <- subset(engagement_weighted_iffy, DateTime >= "2020-04-01 00:00:00" & DateTime <= "2020-06-30 00:00:00");
year2020quarter3 <- subset(engagement_weighted_iffy, DateTime >= "2020-07-01 00:00:00" & DateTime <= "2020-09-30 00:00:00");
year2020quarter4 <- subset(engagement_weighted_iffy, DateTime >= "2020-10-01 00:00:00" & DateTime <= "2020-12-31 00:00:00");
all_years_averages <- data.frame(
	Platform = c("Facebook", "Twitter", "Average"),
	Average = c(mean(engagement_weighted_iffy$Facebook, na.rm = TRUE), mean(engagement_weighted_iffy$Twitter, na.rm = TRUE),mean(c(engagement_weighted_iffy$Facebook, engagement_weighted_iffy$Twitter), na.rm=TRUE))
)
all_years_averages$Platform <- factor(all_years_averages$Platform,
    levels = c("Facebook", "Twitter", "Average"))
annual_averages <- data.frame(
    Year = factor(c("2016", "2017", "2018", "2019", "2020"), levels=c("2016", "2017", "2018", "2019", "2020")),
	Facebook = c(mean(year2016$Facebook, na.rm=TRUE), mean(year2017$Facebook, na.rm=TRUE), mean(year2018$Facebook, na.rm=TRUE), mean(year2019$Facebook, na.rm=TRUE), mean(year2020$Facebook, na.rm=TRUE)),
	Twitter = c(mean(year2016$Twitter, na.rm=TRUE), mean(year2017$Twitter, na.rm=TRUE), mean(year2018$Twitter, na.rm=TRUE), mean(year2019$Twitter, na.rm=TRUE), mean(year2020$Twitter, na.rm=TRUE)),
	Average = c(mean(c(year2016$Facebook, year2016$Twitter), na.rm=TRUE), mean(c(year2017$Facebook, year2017$Twitter), na.rm=TRUE), mean(c(year2018$Facebook, year2018$Twitter), na.rm=TRUE), mean(c(year2019$Facebook, year2019$Twitter), na.rm=TRUE), mean(c(year2020$Facebook, year2020$Twitter), na.rm=TRUE))
)
annual_averages_single_column <- annual_averages %>%
     gather("Platform", "Value", -Year)
annual_averages_single_column$Platform <- factor(annual_averages_single_column$Platform,
    levels = c("Facebook", "Twitter", "Average"))
quarterly_averages <- data.frame(
	Year = factor(c("2016", "2016", "2016", "2016",
		"2017", "2017", "2017", "2017",
		"2018", "2018", "2018", "2018",
		"2019", "2019", "2019", "2019",
		"2020", "2020", "2020", "2020")),
    Quarter = factor(c("Q1 2016", "Q2 2016", "Q3 2016", "Q4 2016",
		"Q1 2017", "Q2 2017", "Q3 2017", "Q4 2017",
		"Q1 2018", "Q2 2018", "Q3 2018", "Q4 2018",
		"Q1 2019", "Q2 2019", "Q3 2019", "Q4 2019",
		"Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020"
	), levels=c("Q1 2016", "Q2 2016", "Q3 2016", "Q4 2016",
		"Q1 2017", "Q2 2017", "Q3 2017", "Q4 2017",
		"Q1 2018", "Q2 2018", "Q3 2018", "Q4 2018",
		"Q1 2019", "Q2 2019", "Q3 2019", "Q4 2019",
		"Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020"
	)),
	Facebook = c(
		mean(year2016quarter1$Facebook, na.rm=TRUE), mean(year2016quarter2$Facebook, na.rm=TRUE), mean(year2016quarter3$Facebook, na.rm=TRUE), mean(year2016quarter4$Facebook, na.rm=TRUE),
		mean(year2017quarter1$Facebook, na.rm=TRUE), mean(year2017quarter2$Facebook, na.rm=TRUE), mean(year2017quarter3$Facebook, na.rm=TRUE), mean(year2017quarter4$Facebook, na.rm=TRUE),
		mean(year2018quarter1$Facebook, na.rm=TRUE), mean(year2018quarter2$Facebook, na.rm=TRUE), mean(year2018quarter3$Facebook, na.rm=TRUE), mean(year2018quarter4$Facebook, na.rm=TRUE),
		mean(year2019quarter1$Facebook, na.rm=TRUE), mean(year2019quarter2$Facebook, na.rm=TRUE), mean(year2019quarter3$Facebook, na.rm=TRUE), mean(year2019quarter4$Facebook, na.rm=TRUE),
		mean(year2020quarter1$Facebook, na.rm=TRUE), mean(year2020quarter2$Facebook, na.rm=TRUE), mean(year2020quarter3$Facebook, na.rm=TRUE), mean(year2020quarter4$Facebook, na.rm=TRUE)
	),
	Twitter = c(
		mean(year2016quarter1$Twitter, na.rm=TRUE), mean(year2016quarter2$Twitter, na.rm=TRUE), mean(year2016quarter3$Twitter, na.rm=TRUE), mean(year2016quarter4$Twitter, na.rm=TRUE),
		mean(year2017quarter1$Twitter, na.rm=TRUE), mean(year2017quarter2$Twitter, na.rm=TRUE), mean(year2017quarter3$Twitter, na.rm=TRUE), mean(year2017quarter4$Twitter, na.rm=TRUE),
		mean(year2018quarter1$Twitter, na.rm=TRUE), mean(year2018quarter2$Twitter, na.rm=TRUE), mean(year2018quarter3$Twitter, na.rm=TRUE), mean(year2018quarter4$Twitter, na.rm=TRUE),
		mean(year2019quarter1$Twitter, na.rm=TRUE), mean(year2019quarter2$Twitter, na.rm=TRUE), mean(year2019quarter3$Twitter, na.rm=TRUE), mean(year2019quarter4$Twitter, na.rm=TRUE),
		mean(year2020quarter1$Twitter, na.rm=TRUE), mean(year2020quarter2$Twitter, na.rm=TRUE), mean(year2020quarter3$Twitter, na.rm=TRUE), mean(year2020quarter4$Twitter, na.rm=TRUE)
	),
    Average = c(
		mean(c(year2016quarter1$Facebook, year2016quarter1$Twitter), na.rm=TRUE), mean(c(year2016quarter2$Facebook, year2016quarter2$Twitter), na.rm=TRUE), mean(c(year2016quarter3$Facebook, year2016quarter3$Twitter), na.rm=TRUE), mean(c(year2016quarter4$Facebook, year2016quarter4$Twitter), na.rm=TRUE),
		mean(c(year2017quarter1$Facebook, year2017quarter1$Twitter), na.rm=TRUE), mean(c(year2017quarter2$Facebook, year2017quarter2$Twitter), na.rm=TRUE), mean(c(year2017quarter3$Facebook, year2017quarter3$Twitter), na.rm=TRUE), mean(c(year2017quarter4$Facebook, year2017quarter4$Twitter), na.rm=TRUE),
		mean(c(year2018quarter1$Facebook, year2018quarter1$Twitter), na.rm=TRUE), mean(c(year2018quarter2$Facebook, year2018quarter2$Twitter), na.rm=TRUE), mean(c(year2018quarter3$Facebook, year2018quarter3$Twitter), na.rm=TRUE), mean(c(year2018quarter4$Facebook, year2018quarter4$Twitter), na.rm=TRUE),
		mean(c(year2019quarter1$Facebook, year2019quarter1$Twitter), na.rm=TRUE), mean(c(year2019quarter2$Facebook, year2019quarter2$Twitter), na.rm=TRUE), mean(c(year2019quarter3$Facebook, year2019quarter3$Twitter), na.rm=TRUE), mean(c(year2019quarter4$Facebook, year2019quarter4$Twitter), na.rm=TRUE),
		mean(c(year2020quarter1$Facebook, year2020quarter1$Twitter), na.rm=TRUE), mean(c(year2020quarter2$Facebook, year2020quarter2$Twitter), na.rm=TRUE), mean(c(year2020quarter3$Facebook, year2020quarter3$Twitter), na.rm=TRUE), mean(c(year2020quarter4$Facebook, year2020quarter4$Twitter), na.rm=TRUE)
	)
)
platform_colors <- c("Facebook" = "#0072B2", "Twitter" = "#009E73", "Average" = "#009E73");
year_colors <- c("2016" = "#0072B2", "2017" = "#CC79A7", "2018" = "#D55E00", "2019" = "#56B4E9", "2020" = "#009E73");

starter_plot_settings <-
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5));

#########################################

# 2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient
ggplot(data=all_years_averages, aes(x=Platform, y=Average, fill=Platform)) +
	starter_plot_settings +
	geom_bar(stat="identity") +
	ggtitle("2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient") +
	labs(x="Platform", y="Percentage engagement with URLs from iffy sites", fill = "Legend") +
	theme(legend.position = "none") +
	scale_fill_manual(values=platform_colors)

# 2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year
#ggplot(data=annual_averages_single_column, aes(x=Year, y=Value, fill=Platform)) +
#	starter_plot_settings +
#    geom_bar(stat="identity", position="dodge") +
#	ggtitle("2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year") +
#	labs(x="Years", y="Percentage engagement with URLs from iffy sites", fill = "Legend") +
#	scale_fill_manual(values=platform_colors)

# 2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year
annual_averages_single_column_no_average = subset(annual_averages_single_column, Platform != "Average")
ggplot(data=annual_averages_single_column_no_average, aes(x=Year, y=Value, fill=Platform)) +
	starter_plot_settings +
    geom_bar(stat="identity", position="dodge") +
	ggtitle("2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Year") +
	labs(x="Years", y="Percentage engagement with URLs from iffy sites", fill = "Legend") +
	scale_fill_manual(values=platform_colors)

#ggplot(data=annual_averages, aes(x=Year, y=Average, fill=Year)) +
#    theme_bw() +
#	ggtitle("Twitter Engagement-weighted Iffy Quotient by Year") +
#    theme(plot.title = element_text(hjust = 0.5)) +
#	xlab("abc") +
#    ylab("Percentage engagement with URLs from iffy sites") +
#	geom_bar(stat="identity") +
#	scale_fill_manual(values=year_colors)
	
# 2016-2020 Facebook Average Engagement-weighted Iffy Quotient by Quarter
ggplot(quarterly_averages, aes(x = Quarter, y= Facebook, fill = Year)) +
	starter_plot_settings +
	geom_bar(stat="identity") +
    ggtitle("2016-2020 Facebook Average Engagement-weighted Iffy Quotient by Quarter") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
	labs(x="Years", y="Percentage engagement with URLs from iffy sites", fill = "Legend") +
    scale_fill_manual(values=year_colors)
	
# 2016-2020 Twitter Average Engagement-weighted Iffy Quotient by Quarter
ggplot(quarterly_averages, aes(x = Quarter, y= Twitter, fill = Year)) +
	starter_plot_settings +
	geom_bar(stat="identity") +
    ggtitle("2016-2020 Twitter Average Engagement-weighted Iffy Quotient by Quarter") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
	labs(x="Years", y="Percentage engagement with URLs from iffy sites", fill = "Legend") +
    scale_fill_manual(values=year_colors)
	
# 2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Quarter
ggplot(quarterly_averages, aes(x = Quarter, y= Average, fill = Year)) +
	starter_plot_settings +
	geom_bar(stat="identity") +
    ggtitle("2016-2020 Facebook and Twitter Average Engagement-weighted Iffy Quotient by Quarter") +
    theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1)) +
	labs(x="Years", y="Percentage engagement with URLs from iffy sites", fill = "Legend") +
    scale_fill_manual(values=year_colors)