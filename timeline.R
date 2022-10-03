library(tidyverse)

dat <- readxl::read_xlsx("master list.xlsx") %>%
  janitor::clean_names()

df <- dat %>% #select(year, bayes_frequentist, author) %>% 
  filter(year > 2010)



# Stacked barplot with multiple groups
ggplot(data=df, aes(x=year, fill=bayes_frequentist)) +
  geom_bar(stat="count")



# Or a fancier type

positions <- c(0.5, -0.5, 1.25, -1.25, 2, -2)
directions <- c(1, -1)

line_pos <- data.frame(
  "year"=unique(df$year),
  "position"=rep(positions, length.out=length(unique(df$year))),
  "direction"=rep(directions, length.out=length(unique(df$year)))
)

df <- merge(x=df, y=line_pos, by="year", all = TRUE)
df <- df[with(df, order(year, bayes_frequentist)), ]

head(df)

month_buffer <-0

month_date_range <- seq(min(df$year)-month_buffer, max(df$year)+month_buffer)
#month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range)

text_offset <- 0.3

df$year_count <- ave(df$year==df$year, df$year, FUN=cumsum)
df$text_position <- (df$year_count * text_offset * df$direction) + df$position
head(df)

timeline_plot<- df %>%
  filter(package == "Yes") %>%
  ggplot(aes(x=year,y=0, #col=bayes_frequentist, 
             label=software)) +
  #labs(col="Type") +
  #scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE) +
  theme_classic() +
  # Plot horizontal black line for timeline
  geom_hline(yintercept=0, 
                              color = "black", size=0.3) +
# Plot vertical segment lines for milestones
  geom_segment(data=df, 
               aes(y=position,yend=0,xend=year), 
               color='black', size=0.2) +
# Plot scatter points at zero and date
  geom_point(aes(y=0), size=1) +
# Don't show axes, appropriately position legend
  theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom"
) +
# Show text for each month
  geom_text(data=month_df, 
            aes(x=month_date_range,
                y=-0.3,label=month_date_range),
            size=4,vjust=1, color='black', angle=0) +
# Show text for each milestone
  geom_text(aes(y=text_position,label=software),size=4)
print(timeline_plot)

