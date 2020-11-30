## United States presidential election 2020 result

# install.packages('rvest')
# install.packages('urltools')

## author: 1771087 seokbeom, jeon

## Packages
# if you do not have packages below, use install.packages('<package_name>')
# HTML elements
library(rvest) 
# rvest is possible to crawl static web page crawling
# RSelenium is possible to crawl dynamic web page
library(RSelenium)
# HTTP connection
library(httr)
# Encoding-related
library(urltools)
# Pipe operator
library(dplyr)
# Text preprocessing
library(stringr)
library(tidyverse)



## Reference
#
# Ref. language:  Korean
# url:            https://statkclee.github.io/yonsei/data/R_Web_Crawling.pdf
# about:          R Web Crawling
#
# Ref. language:  Korean 
# url:            https://truman.tistory.com/166
# about:          RSelenium Usage
## Type below on Windows cmd previously located on your 
## java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4545

driver <- remoteDriver(remoteServerAddr='localhost',
                       port=4545L,
                       browserName='chrome')

# open browser
driver$open()

# connect to url
url <- 'https://www.nbcnews.com/politics/2020-elections/president-results'
driver$navigate(url)

# click button to scrap data which i want
driver$findElement(using='xpath',
                   value='//*[@id="live-list-item-1"]/main/div[1]/div[2]/div/div[2]/button')$clickElement()


###### 
## Get data from Web page & Data preprocessing
# read current page sources
src <- driver$getPageSource()[[1]]
html <- read_html(src)

(state_colname <- html_nodes(html,
                            css='#live-list-item-1 > main > div.jsx-2521325328.page-content.relative > div:nth-child(2) > div > div.mt8 > div > div.jsx-2193855077 > div.cell-list > div') %>% html_text())

## state names data
(state_column <- html_nodes(html,
                            css='#live-list-item-1 > main > div.jsx-2521325328.page-content.relative > div:nth-child(2) > div > div.mt8 > div > div.jsx-2193855077 > div.cell-list > ul > li > div > div.dn.db-m') %>% html_text())

dim(state_column) <- c(51, 1)
colnames(state_column) <- state_colname
state_column

# Biden's percentage and votes
(Biden_percent_colname <- html_nodes(html,
                           css='#live-list-item-1 > main > div.jsx-2521325328.page-content.relative > div:nth-child(2) > div > div.mt8 > div > div.jsx-1526806201.scrollable-grid > div > div:nth-child(1) > div.jsx-3384420229.column-group > div:nth-child(1) > div > div') %>% html_text())
(Biden_percent_column <- html_nodes(html,
                                   css='#live-list-item-1 > main > div.jsx-2521325328.page-content.relative > div:nth-child(2) > div > div.mt8 > div > div.jsx-1526806201.scrollable-grid > div > div:nth-child(1) > div.jsx-3384420229.column-group > div:nth-child(1) > div > ul > li') %>% html_text())
(Biden_votes_colname <- html_nodes(html,
                                  css='#live-list-item-1 > main > div.jsx-2521325328.page-content.relative > div:nth-child(2) > div > div.mt8 > div > div.jsx-1526806201.scrollable-grid > div > div:nth-child(1) > div.jsx-3384420229.column-group > div:nth-child(2) > div > div') %>% html_text())
(Biden_votes_column <- html_nodes(html,
                                   css='#live-list-item-1 > main > div.jsx-2521325328.page-content.relative > div:nth-child(2) > div > div.mt8 > div > div.jsx-1526806201.scrollable-grid > div > div:nth-child(1) > div.jsx-3384420229.column-group > div:nth-child(2) > div > ul > li') %>% html_text())


dim(Biden_percent_column) <- c(51, 1)
colnames(Biden_percent_column) <- Biden_percent_colname
Biden_percent_column

B_percent <- as.numeric(sub('%', '', Biden_percent_column))


dim(Biden_votes_column) <- c(51, 1)
colnames(Biden_votes_column) <- Biden_votes_colname
Biden_votes_column

B_votes <- as.numeric(gsub(',','',Biden_votes_column))

dim(B_percent) <- c(51,1)
dim(B_votes) <- c(51,1)

colnames(B_percent) <- 'Biden_Percent'
colnames(B_votes) <- 'Biden_Votes'

df <- data.frame(state_column, B_percent, B_votes)
head(df)

(Trump_percent_column <- html_nodes(html,
                                    css='#live-list-item-1 > main > div.jsx-2521325328.page-content.relative > div:nth-child(2) > div > div.mt8 > div > div.jsx-1526806201.scrollable-grid > div > div:nth-child(2) > div.jsx-3384420229.column-group > div:nth-child(1) > div > ul > li') %>% html_text())

(Trump_vote_column <- html_nodes(html,
                                    css='#live-list-item-1 > main > div.jsx-2521325328.page-content.relative > div:nth-child(2) > div > div.mt8 > div > div.jsx-1526806201.scrollable-grid > div > div:nth-child(2) > div.jsx-3384420229.column-group > div:nth-child(2) > div > ul > li') %>% html_text())

(T_percent <- as.numeric(gsub('%', '', Trump_percent_column)))
(T_votes <- as.numeric(gsub(',', '', Trump_vote_column)))

dim(T_percent) <- c(51,1)
colnames(T_percent) <- 'Trump_Percent'
T_percent
dim(T_votes) <- c(51,1)
colnames(T_votes) <- 'Trump_Votes'

(B_total <- sum(B_votes))
(T_total <- sum(T_votes))

df <- cbind(df, T_percent, T_votes)
df
df[c(T,F,T,F)]
(p_df <- cbind(df[c(T,F,T,F)]))
(v_df <- cbind(df[c(F,T,F,T)]))


(Expected_vote_in <- html_nodes(html,
                                css='#live-list-item-1 > main > div.jsx-2521325328.page-content.relative > div:nth-child(2) > div > div.mt8 > div > div.jsx-3921339373.dn.db-m > div.cell-list > ul > li') %>% html_text())

df <- cbind(df, Expected_vote_in)
df


# Add abbreviation to dataframe
abbrev <- read.csv('us-state-name.csv')[2]
abbrev <- abbrev[,1]
dim(abbrev) <- c(51,1)
colnames(abbrev) <- 'Abbreviations'
abbrev

df <- cbind(df, abbrev)

(df <- df[c(7, 1:6)])

df

# Create csv file
write.csv(df,'united-states-presidential_election_2020.csv', row.names=FALSE)
######

# Close RSelenium Driver
driver$close()

## References
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
# https://cran.r-project.org/web/packages/usmap/vignettes/advanced-mapping.html
#### NEED TO BE ADVANCED WITH FOLLOWING BELOW LINK (not now)
# https://socviz.co/maps.html
# Below was commented after installation
# install.packages('usmap')
library(usmap)
library(ggplot2)

# Colour ocean area as lightblue
# usmap::plot_usmap(regions='states') +
#   ggplot2::labs(title='United States') +
#   ggplot2::theme(panel.background = ggplot2::element_rect(color='black',fill='lightblue'))

# Plot state names 
usmap::plot_usmap(regions = 'states', labels = TRUE)

# result = (Abbreviation, ('B' or 'T'))
result <- cbind(df$Abbreviations, matrix(ifelse(df$Biden_Votes > df$Trump_Votes, 'B', 'T'), ncol=1))
# B or T
result[,2]

# Total votes per candidate
# Biden
B_total
# Trump
T_total

# Character vector which contains state name abbreviations supporting Biden
B_states <- df$Abbreviations[ifelse(result[,2]=='B', TRUE, FALSE)]
# Character vector which contains state name abbreviations supporting Trump
T_states <- df$Abbreviations[ifelse(result[,2]=='T', TRUE, FALSE)]


# A graph showing states supporting Trump
u1 <- usmap::plot_usmap(regions='states', 
                        labels='TRUE', 
                        label_color='black',
                        include=T_states,
                        fill='#ff0000',
                        alpha=0.3,
                        color='#ff0000', #border
                        size=1.25
)
u1
# A graph showing states supporting Biden
u2 <- usmap::plot_usmap(regions='states', 
                        labels='TRUE', 
                        label_color='white',
                        include=B_states,
                        fill='#0000ff',
                        alpha=0.3,
                        color='#0000ff',
                        size=1.25)
u2
#change colname[1] for usmap package

# Create a new dataframe having needed values for usmap package(corrected colnames, fips, etc.)
(df_usmap <- data.frame(fips=usmap::fips(df$Abbreviations), # correct each state code along usmap package
                        abbr=df$Abbreviations,
                        full=df$States, 
                        Biden=df$Biden_Votes, 
                        Trump=df$Trump_Votes,
                        res=factor(result[,2])))

## Reference:
# https://stackoverflow.com/questions/61330372/discrete-values-in-us-map-legend-using-plot-usmap

# Combined
(u3 <- usmap::plot_usmap(data=df_usmap, 
                         values='res',
                         labels=TRUE,
                         label_color = 'white',
                         color='white',
) +
    ggplot2::theme(panel.background = element_rect(colour='black')) +
    ggplot2::scale_fill_manual(values=c(`B`='#0000aa', `T`='#aa0000'), name='res') +
    ggplot2::theme(legend.position = 'right') +
    ggplot2::ggtitle('2020 U.S. presidential election result'))

#####
# https://stackoverflow.com/questions/24387376/r-error-could-not-find-function-multiplot-using-cookbook-example
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## Grouped Bar plot

vote4barplot <- NULL
for(i in 1:51){ # rep(Trump, Biden, 51)
  vote4barplot <- c(vote4barplot, df$Trump_Votes[i], df$Biden_Votes[i])
}

# Data restruction for ggplot
data <- data.frame(
  Candidate=rep(c('Trump','Biden'), 51),
  Votes=vote4barplot, 
  States=rep(df[,1], each=2))

# Check data
head(data)

candidate_colour <- c(Trump='red3', Biden='blue3')

# Print stacked bar plot
data$States <- reorder(data$States, desc(data$States))
head(data)

stacked_barplot <- ggplot(data, aes(fill=Candidate, y=Votes, x=States)) +
  geom_bar(position='stack', stat='identity') +
  scale_fill_manual(values=candidate_colour) +
  coord_flip()  # Horizontalize stacked bar plot 

stacked_barplot

# Summation of votes for Biden, Trump per states
head(data)
vote_summation <- tapply(data[,2], rep(1:51, each=2), FUN=sum)
head(vote_summation)
length(vote_summation) # 51


# Add abbreviation to row names of Summation of votes data
rownames(vote_summation) <- df$Abbreviations
head(vote_summation)
head(data)


# Check summation data is same with previous data
data$Votes[2]

data$Votes[1] + data$Votes[2] == vote_summation[1] # Same


multiplot(u1,u2,u3,stacked_barplot, cols=2)
