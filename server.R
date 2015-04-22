# server.R
# author: Iga Korneta
# version: 1.0
# created: 08/24/2014
# visualisation of the 2014 West Africa Ebola outbreak: backend server code

# load the libraries
#library(maptools)
#library(rgdal)
library(shiny)
library(XML)
library(ggplot2)
library(plyr)
library(reshape2)

# load the map
#africa <- readOGR(dsn="./data", layer="Africa")
#africa.points <- fortify(africa, region="COUNTRY")
#west.africa.points <- subset(africa.points, long<20 & lat>0 & lat<20)
west.africa.points <- read.csv("./data/westAfrica.csv")
west.africa.points$cases <- rep(0,nrow(west.africa.points))
west.africa.points$deaths <- rep(0,nrow(west.africa.points))

# load the URL
url <- 'http://en.wikipedia.org/wiki/Ebola_virus_epidemic_in_West_Africa'

# source a helper function
source('./helper.R')

# main function
shinyServer(
  function(input, output, session) {    
    
    
    #this is a dynamic slider which theoretically should reset it maximum value once a day
    output$slider <- renderUI({
      invalidateLater(1000*59*60*24, session)
      sliderInput("input_date", 
                  label="Day 1 = March 22, 2014", 
                  min=1, 
                  max=as.numeric(as.Date(Sys.time())-as.Date("2014-03-21")),
                  value=1,
                  step=1,
                  animate=animationOptions(interval=200, loop=TRUE)
                  )      
      })
    
    
    #load and process the Wikipedia data
    #this part should theoretically be reloaded once a day
    #thanks to invalidateLater
    #unfortunately, it is impossible to automatically obtain column names & hence countries
    in.data <- reactive({
      invalidateLater(1000*60*60*24, session)
      #dat <- readHTMLTable(url)[[2]]
      #if (length(colnames(dat))<11) dat <- readHTMLTable(url)[[6]]
      #dat <- dat[2:nrow(dat),c(1, 4:(ncol(dat)-1))]
      dat1 <- readHTMLTable(url)[[5]]
      dat1 <- dat1[2:nrow(dat1),c(1, 4:9)]
      dat2 <- readHTMLTable(url)[[6]]
      dat2 <- dat2[2:nrow(dat2),c(1, 4:7)]
      dat3 <- readHTMLTable(url)[[7]]
      dat3 <- dat3[2:nrow(dat3),c(1, 4:13)]
      dat12 <- merge(dat1, dat2, by.x="V1", by.y="V1")
      #dat30 <- cbind(dat3, V12=rep(0, nrow(dat3)), V13=rep(0, nrow(dat3)))
      dat30 <- dat3
      colnames(dat12) <- c("Date", "Guinea.cases", "Guinea.deaths", "Liberia.cases", "Liberia.deaths",
                         "Sierra.Leone.cases", "Sierra.Leone.deaths", "Nigeria.cases", "Nigeria.deaths", "Senegal.cases", "Senegal.deaths")
      colnames(dat30) <- c("Date", "Guinea.cases", "Guinea.deaths", "Liberia.cases", "Liberia.deaths",
                           "Sierra.Leone.cases", "Sierra.Leone.deaths", "Nigeria.cases", "Nigeria.deaths", "Senegal.cases", "Senegal.deaths")
      dat <- rbind(dat12, dat30)
      dat$Date <- helper(dat$Date)
      dat[,2:ncol(dat)] <- sapply(dat[,2:ncol(dat)], function(x) as.numeric(gsub(",","",x)))
      dat <- arrange(dat, Date)
      dat[1,2:ncol(dat)] <- sapply(dat[1,2:ncol(dat)], function(x) ifelse(is.na(x), 0, x))
      for(i in 2:nrow(dat)){dat[i,2:ncol(dat)] <- ifelse(is.na(dat[i,2:ncol(dat)]), dat[i-1,2:ncol(dat)], dat[i,2:ncol(dat)])}
      dat
    })
    
    
    new.cases <- reactive({
      dat <- in.data()
      new.cases <- dat[1,]
      offset <- data.frame(offset=0)
      for (i in 2:nrow(dat)){
        offset[i,] <- as.numeric(dat[i,1]-dat[i-1,1])
        new.cases[i,1] <- dat[i,1]
        new.cases[i,2:ncol(dat)] <- ifelse(dat[i,2:ncol(dat)]-dat[i-1,2:ncol(dat)]>=0, dat[i,2:ncol(dat)]-dat[i-1,2:ncol(dat)], 0)
      } 
      new.cases[2:nrow(new.cases), 2:ncol(dat)] <- new.cases[2:nrow(new.cases),2:ncol(dat)]/offset[2:nrow(new.cases),1]      
      newdat <- data.frame(Date=seq(from=1, to=as.numeric(as.Date(Sys.time())-as.Date("2014-03-21")), by=1)) 
      newdat <- join(newdat, new.cases, by="Date", match="first")
      newdat[nrow(newdat), 2:ncol(dat)] <- sapply(newdat[nrow(newdat),2:ncol(dat)], function(x) ifelse(is.na(x), 0, x))
      for (i in rev(2:(nrow(newdat)-1))){newdat[i,2:ncol(dat)] <- ifelse(is.na(newdat[i,2:ncol(dat)]), newdat[i+1,2:ncol(dat)], newdat[i,2:ncol(dat)])}
      new.cases <- newdat
      new.cases
    })
    
    
    cum.cases <- reactive({
      dat <- in.data()
      new.cases <- new.cases()
      newdat <- data.frame(Date=seq(from=1, to=as.numeric(as.Date(Sys.time())-as.Date("2014-03-21")), by=1)) 
      newdat <- join(newdat, dat, by="Date")
      for (i in 2:(nrow(new.cases))){
        newdat[i,1] <- new.cases[i,1]
        newdat[i,2:ncol(dat)] <- ifelse(is.na(newdat[i,2:ncol(dat)]), newdat[i-1,2:ncol(dat)] + new.cases[i,2:ncol(dat)], newdat[i,2:ncol(dat)])
      }
      cum.cases <- newdat[-nrow(newdat),]
      cum.cases
    })
    
    
    total.cases <- reactive({
      cum.cases <- melt(cum.cases(), id="Date")
      cum.cases$country <- as.factor(gsub(".(cases|deaths)", "", cum.cases$variable))
      cum.cases$country <- as.factor(gsub("Sierra.Leone", "Sierra Leone", cum.cases$country))
      cum.cases$type <- as.factor(gsub("(Guinea|Nigeria|Liberia|Sierra.Leone|Senegal).", "", cum.cases$variable))
      cum.cases$variable <- "cumulative"
      new.cases <- melt(new.cases(), id="Date")
      new.cases$country <- as.factor(gsub(".(cases|deaths)", "", new.cases$variable))
      new.cases$country <- as.factor(gsub("Sierra.Leone", "Sierra Leone", new.cases$country))
      new.cases$type <- as.factor(gsub("(Guinea|Nigeria|Liberia|Sierra.Leone|Senegal).", "", new.cases$variable))
      new.cases$variable <- "new"
      total.cases <- rbind(cum.cases, new.cases)
      total.cases$date <- total.cases$Date
      total.cases
    })
    
    
    
    #the following part occurs upon widget input
    cum.new.switch <- reactive({
      switch(input$cum.new, "Average new"="new", "Cumulative"="cumulative")      
    })
    
    #data preparation
    current.data <- reactive({
      cum.new.switch.temp <- cum.new.switch()
      africa.data <- west.africa.points
      total.cases <- total.cases()
      
      cases.subset <- subset(total.cases, total.cases$variable==cum.new.switch.temp 
                             & total.cases$type=="cases" 
                             & total.cases$date==as.numeric(input$input_date))
      deaths.subset <- subset(total.cases, total.cases$variable==cum.new.switch.temp 
                              & total.cases$type=="deaths" 
                              & total.cases$date==as.numeric(input$input_date))
      
      for (x in levels(total.cases$country))
      {
        africa.data$cases[africa.data$id==x] <- cases.subset[cases.subset$country==x, "value"]
        africa.data$deaths[africa.data$id==x] <- deaths.subset[deaths.subset$country==x, "value"]
      }
      
      
      africa.data
    })
    
    
    output$cases.map <- renderPlot({
      dat <- in.data()
      cum.new.switch.temp <- cum.new.switch()
      limits <- c(0,ceiling(max(dat[,c(2,4,6,8,10)])/1000)*1000)
      if (cum.new.switch.temp=="new") {limits <- c(0,30)}
      ggplot(current.data(), aes(x = long, y = lat, group = group, fill=cases)) + 
        geom_polygon(size = 1) +  geom_path(color="dark green") + coord_equal() +
        theme_bw()  + 
        scale_fill_gradient(low="white", high="#f03b20", limits=limits)
    })

    
    output$deaths.map <- renderPlot({
      dat <- in.data()
      cum.new.switch.temp <- cum.new.switch()
      limits <- c(0,(ceiling(max(dat[,c(3,5,7,9,11)])/1000))*1000)
      if (cum.new.switch.temp=="new") {limits <- c(0,30)}
      ggplot(current.data(), aes(x = long, y = lat, group = group, fill=deaths)) + 
        geom_polygon(size = 1) +  geom_path(color="dark green") + coord_equal() +
        theme_bw()  + 
        scale_fill_gradient(low="white", high="#f03b20", limits=limits)    
    })     
    
    
  }
)
