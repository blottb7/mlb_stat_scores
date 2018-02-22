library(rvest)

url <- read_html('https://www.fangraphs.com/projections.aspx?pos=c&stats=bat&type=steamer&team=0&lg=all&players=0')
webpage <- html_nodes(url, '.grid_line_regular')
webpage <- html_text(webpage)
new <- data.frame(matrix(webpage, ncol = 25, byrow = TRUE))
#webpage <- tibble(webpage)

webpage <- html_nodes(url, 'tbody td')
webpage <- html_text(webpage)
new <- data.frame(matrix(webpage, ncol = 25, byrow = TRUE))

#this gets all the data on the first page of catchers in one read.
webpage <- html_nodes(url, '#ProjectionBoard1_dg1_ctl00 tbody td')
webpage <- html_text(webpage)
new <- data.frame(matrix(webpage, ncol = 29, byrow = TRUE))

#this also works
web2 <- html_nodes(url, '.grid_line_break , .grid_line_regular , .rgHeader')
web2 <- html_text(web2)
new2 <- data.frame(matrix(webpage, ncol = 29, byrow = TRUE))

#this also works and may be the most succinct
web2 <- html_nodes(url, 'tbody td')
web2 <- html_text(web2)
new2 <- data.frame(matrix(webpage, ncol = 29, byrow = TRUE))

#get second page
url <- read_html('https://www.fangraphs.com/projections.aspx?pos=c&stats=bat&type=steamer&team=0&lg=all&players=0')
web2 <- html_nodes(url, '#ProjectionBoard1')
web2 <- html_text(web2)

web2 <- html_nodes(url, 'tbody td')
web2 <- html_text(web2)
new2 <- data.frame(matrix(webpage, ncol = 29, byrow = TRUE))

#trial and error
duplicated(c('https://www.fangraphs.com/projections.aspx?pos=c&stats=bat&type=steamer&team=0&lg=all&players=0',
'https://www.fangraphs.com/projections.aspx?pos=c&stats=bat&type=steamer&team=0&lg=all&players=0'))
https://www.fangraphs.com/projections.aspx?pos=c&stats=bat&type=steamer&team=0&lg=all&players=0
https://www.fangraphs.com/projections.aspx?pos=c&stats=bat&type=steamer&team=0&lg=all&players=0
https://www.fangraphs.com/projections.aspx?pos=c&stats=bat&type=steamer&team=0&lg=all&players=0
#ProjectionBoard1_dg1_ctl00__1 .grid_line_regular , #ProjectionBoard1_dg1_ctl00__0 .grid_line_regular , #ProjectionBoard1_dg1_ctl00__0 a
