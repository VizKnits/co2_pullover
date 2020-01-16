needs(tidyverse, magrittr, scales, zoo) # load dependencies

temp_data <- read.csv("API_EN.ATM.CO2E.PC_DS2_en_csv_v2_612637.csv") %>% # import dataset
  filter(Country.Name %in% "World") %>% # select global data
  gather(key=Year, value=CO2_Emissions, 5:59) %>% # tidy data format
  select("Year", "CO2_Emissions") %>% 
  mutate(Year = as.numeric(str_replace(Year, "X", ""))) # clean up years

# area chart of the CO2 emissions per capita
ggplot(temp_data, aes(Year, CO2_Emissions, group=1)) +
  geom_area() +
  theme_bw()

## knitting pattern ##
# we will convert the area chart into a tileplot where each tile is representing a knitting stitch
# for the pattern we have 100 rows of 72 columns for one side of the sweater
columns = 72
rows = 100 

# pattern
# first we project the CO2 Emissions on a scale from 1 to the number of stiches we have for the pattern
tcolumn <- data.frame(Year = temp_data$Year, 
                      trow = round(rescale(c(0, temp_data$CO2_Emissions), to = c(1, rows)))[-1])
# now we rescale the years
trow <- data.frame(Year = temp_data$Year, 
                   tcolumn = round(rescale(temp_data$Year, to = c(1, columns))))

# now we match the rescaled data
transformed <- left_join(trow, tcolumn) 

# rescaling to an integer scale always comes with little inaccuracies, we have to live with those
# there are more rows than years, therefore we have years that will be represented by two stitches instead of only one
# we now add these missing rows to the data set and fill them with the value of the previous 
transformed <- left_join(data.frame(tcolumn = 1:max(transformed$tcolumn)), transformed)
transformed$tcolumn <-  na.locf(transformed$tcolumn, fromLast = TRUE)
transformed$trow <-  na.locf(transformed$trow, fromLast = TRUE)
# we now have a data set that tells us how many knitted stitches are supposed to be colored for every column in order to create the plot

# now for the tile plot data set: for the length we want the number of rows, and for every stitch in a row an entry to represent the width
# we also add a column for the pattern we set to zero for now
grid <- data.frame(columns=rep(1:columns, rows), rows = rep(1:rows, each=columns), pattern=0) %>% 
  arrange(rows,columns)

# now we combine the grid data set with the data that tells us how many stitches are to be colored
# for each row in the transformed data set this simple for-loop finds all the rows in the grid data set that matches the knitting row number and has a column number between one and the stitch number that need to be colored, then overwrites the zero in the pattern column to one
for(i in 1:nrow(transformed)){
  grid$pattern[grid$columns %in% transformed$tcolumn[i] & grid$rows %in% 1:transformed$trow[i]] <- 1
}

# last but not least, we add a new row with the column label we want to plot onto the grids
# actually we could just use the column number for this, but there is a little hurdle: when knitting you add row by row like a printer, you start at column one and go through to the last one, and then you'll add a new stitch on the last column which now becomes your first for the new row and knit you way "backwards" to column one of the first row
# this is why we have to reverse the label for every second row:
grid$label <- grid$columns

is.even <- function(x) x %% 2 == 0 # define even numbers
grid[is.even(grid$rows),] <- grid %>% filter(is.even(rows)) %>% group_by(rows) %>% mutate(label = rev(label))

# now for the plot
ggplot(grid, aes(x = columns, y = rows, fill = as.factor(pattern))) + 
  geom_tile(size = 0.5, color = "black") +
  coord_equal() +
  geom_text(aes(label = label)) +
  scale_y_continuous(breaks = seq(0, max(grid$rows), by = 1), expand=c(0,0), sec.axis = dup_axis()) +
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size=22)) +
  scale_fill_manual(values = c("#f2f2f2", "#D89921")) +
  ggtitle("CO2 emissions (metric tons per capita)")

# save the plot as a pdf
ggsave("CO2_sweater.pdf", plot = last_plot(),
       width = 50, height = 56, units = "cm",
       dpi = 300, limitsize = TRUE, device = "pdf")
