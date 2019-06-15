library(shiny)
library(readr)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(lubridate)
library(leaflet)
library(kableExtra)
library(plotly)
library(egg)
library(rsconnect)
##Data Management

taxi <- fread("/Users/wenhuizeng/Desktop/data_challenge 2019/green_tripdata_2015-09.csv")

taxi_clean <- taxi[taxi$Trip_distance>0.5 &taxi$Trip_distance < 100,]
taxi_clean$Pickup_time <- ymd_hms(taxi_clean$lpep_pickup_datetime)
taxi_clean$hour<-as.factor(hour(taxi_clean$Pickup_time))
taxi_clean$weekday<-as.factor(wday(taxi_clean$Pickup_time,label = T))
taxi_clean$airport<-as.character(ifelse(taxi_clean$RateCodeID %in% c(2,3),"Yes","No"))
taxi_clean.1 <- taxi_clean[taxi_clean$Fare_amount>0,]
 taxi_clean.2 <- taxi_clean.1 %>% filter(Pickup_latitude!=0 &Pickup_longitude!=0 & Dropoff_longitude!=0 & Dropoff_latitude!=0)
 taxi_clean.2$JFK <- ifelse((taxi_clean.2$Pickup_longitude > -73.8400 & taxi_clean.2$Pickup_longitude < -73.7581 & taxi_clean.2$Pickup_latitude > 40.62131 & taxi_clean.2$Pickup_latitude < 40.67131)|(taxi_clean.2$Dropoff_longitude > -73.8400 & taxi_clean.2$Dropoff_longitude < -73.7581 & taxi_clean.2$Dropoff_latitude > 40.62131 & taxi_clean.2$Dropoff_latitude < 40.67131),1,0)
 taxi_clean.2$Laguardia <- ifelse((taxi_clean.2$Pickup_longitude > -73.9040 & taxi_clean.2$Pickup_longitude < -73.8640 & taxi_clean.2$Pickup_latitude > 40.7669 & taxi_clean.2$Pickup_latitude < 40.7869)|(taxi_clean.2$Dropoff_longitude > -73.9040 & taxi_clean.2$Dropoff_longitude < -73.8640 & taxi_clean.2$Dropoff_latitude > 40.7669 & taxi_clean.2$Dropoff_latitude < 40.7869),1,0)
 taxi_clean.2$Newark <- ifelse((taxi_clean.2$Pickup_longitude > -74.2145 & taxi_clean.2$Pickup_longitude < -74.1545 & taxi_clean.2$Pickup_latitude > 40.6595 & taxi_clean.2$Pickup_latitude < 40.7195)|(taxi_clean.2$Dropoff_longitude > -74.2145 & taxi_clean.2$Dropoff_longitude < -74.1545 & taxi_clean.2$Dropoff_latitude > 40.62131 & taxi_clean.2$Dropoff_latitude < 40.67131),1,0)
 taxi_clean.2$airport_GIS <- as.factor(ifelse(taxi_clean.2$JFK==1 | taxi_clean.2$Laguardia==1 |taxi_clean.2$Newark==1,"Yes","No"))
 taxi_clean.2$drop_time <- ymd_hms(taxi_clean.2$Lpep_dropoff_datetime)
 taxi_clean.2$t_diff<-difftime(taxi_clean.2$drop_time,taxi_clean.2$Pickup_time,units = "hours")
 taxi_clean.2$t_diff<-as.numeric(taxi_clean.2$t_diff)
 taxi_clean.2$speed <- taxi_clean.2$Trip_distance/taxi_clean.2$t_diff
 taxi_clean.2$tip_pct <- taxi_clean.2$Tip_amount/taxi_clean.2$Fare_amount
 taxi_clean.3 <- taxi_clean.2 %>% filter(Payment_type==1 & Fare_amount>2.5 & Passenger_count > 0 & tip_pct < 0.5 & speed < 70 & t_diff < 1.5)
 taxi_clean.3$tip_YN <- as.factor(ifelse(taxi_clean.3$tip_pct==0,"No","Yes"))
 taxi_clean.3$VendorID<- as.factor(taxi_clean.3$VendorID)
 taxi_clean.3$Passenger_count<- as.factor(taxi_clean.3$Passenger_count)
 taxi_clean.3$Trip_type <- as.factor(taxi_clean.3$Trip_type)
 taxi_clean.3$hour <- as.numeric(taxi_clean.3$hour)
 taxi_clean.3$Hour <- as.factor(ifelse((taxi_clean.3$hour<10 & taxi_clean.3$hour>5)|(taxi_clean.3$hour<20 & taxi_clean.3$hour >16),"rush hour", "normal hour") )
 taxi_clean.3$hour <- as.factor(taxi_clean.3$hour)
 taxi_clean.3$wkd <- as.factor(ifelse(taxi_clean.3$weekday=="Sat" | taxi_clean.3$weekday=="Sun","weekend","workday" ))
 taxi_clean.3$negotiate <- as.factor(ifelse(taxi_clean.3$RateCodeID==5,"Yes","No"))
  taxi_clean.3$boroughs <- as.factor(ifelse(taxi_clean.3$Pickup_longitude > -74.0479 & taxi_clean.3$Pickup_longitude < -73.9067 &
                                             taxi_clean.3$Pickup_latitude > 40.6829 & taxi_clean.3$Pickup_latitude < 40.8820,"Manhattan",
                                           ifelse(taxi_clean.3$Pickup_longitude > -73.9630 & taxi_clean.3$Pickup_longitude < -73.7004 &
                                                    taxi_clean.3$Pickup_latitude > 40.5431 & taxi_clean.3$Pickup_latitude < 40.8007,"Queens",
                                                  ifelse(taxi_clean.3$Pickup_longitude > -74.0421 & taxi_clean.3$Pickup_longitude < -73.8334 &
                                                           taxi_clean.3$Pickup_latitude > 40.5707 & taxi_clean.3$Pickup_latitude < 40.7395,"Brooklyn","Other"
                                                  ))))
 taxi_clean.3$trip_duration<-as.numeric(difftime(taxi_clean.3$drop_time,taxi_clean.3$Pickup_time,units = "min"))

rshiny.data <- taxi_clean.3
borough.list <- as.character(unique((rshiny.data$boroughs)))

ui <- fluidPage(
  
  # App title ----
  titlePanel(h1(strong("New York Taxi Traffic",style = "font-family: 'times'"),align="center")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      radioButtons("group", h3("Analysis Level"),
                   c("inter borough"="all","intra borough" = "borough"
                   )
      ),
      
      conditionalPanel(
        condition = "input.group == 'borough'",
        helpText("Please Enter or Select Borough Name"),
        selectInput("boroughname",h4("Borough Name"), choices=c(borough.list,""),selected=""),
        hr()
     ),
      conditionalPanel(
        condition = "input.group == 'all'",
        helpText("Please Select Your Interested Destination or Time"),
        selectInput("keyfactor",h4("Interested Route"), choices=c("Average Trip Time","hour","week",""),selected="")
      )
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      conditionalPanel(
        condition = ("input.group=='borough'"),
        tabPanel("Figure", 
                 plotOutput("plot",width = "100%")
        )
        
      ),
      conditionalPanel(
        condition = "input.group=='all'",
        tabPanel("Figure", 
                 plotOutput("plot1",width = "100%")
        )
      )
    )
  ))

server <- function(input, output){

  output$plot <- renderPlot({
    if (input$boroughname !=""){
        p1 <- rshiny.data %>%
          filter(boroughs==input$boroughname) %>%
          group_by(hour) %>% summarise(count=n()) %>%
          ggplot()+geom_bar(aes(x=hour,y=count),stat = "identity",fill="blue",size=1)+
          theme_bw() +
          labs(x="Hour of the Day",y="Number of Trips in one hour",title=paste0("Number of Trips during a day ",input$boroughname))+
          theme(text = element_text(face="bold",size=14),legend.position="bottom",
                plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black"))
        p2 <- rshiny.data %>%
          filter(boroughs==input$boroughname) %>%
          group_by(weekday) %>% summarise(count=n()) %>%
          ggplot()+geom_bar(aes(x=weekday,y=count),stat = "identity",fill="blue",size=1)+
          theme_bw() +
          labs(x="Day of Week",y="Number of Trips in a day",title=paste0("Number of Trips during a week ",input$boroughname))+
          theme(text = element_text(face="bold",size=14),legend.position="bottom",
                plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black"))
        
        p3 <- rshiny.data %>%
          filter(boroughs==input$boroughname) %>%
          group_by(hour) %>% summarise(mean_duration=mean(trip_duration),count=n()) %>%
          ggplot()+ geom_line(aes(x=hour,y=mean_duration,group=1),color="red",size=1)+
          theme_bw() +
          labs(x="Hour of the Day",y="Average Trip Time(min) during a day",title=paste0("Average Trip Time during a Day (min) in ",input$boroughname))+
          theme(text = element_text(face="bold",size=14),legend.position="bottom",
          plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black"))
        
        ggarrange(p1,p2, p3,ncol = 1)}
    }, height=850)
  
  output$plot1 <- renderPlot({
    if (input$keyfactor=="Average Trip Time"){ 
  p1 <- rshiny.data %>% group_by(boroughs,hour) %>% summarise(mean_duration=mean(trip_duration)) %>% 
    ggplot()+
    geom_line(aes(x=hour,y=mean_duration,group=boroughs,color=boroughs),size=1)+
    theme_bw()+
    theme(text = element_text(face="bold",size=14),legend.position="bottom",
          plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black"))+
    ggtitle("Average Trip Time in a Day among Different Boroughs")+
    labs(y="Average Trip Time (min)")
  
  ggarrange(p1,ncol = 1)
    } else if (input$keyfactor=="hour"){ 
      p1 <- rshiny.data %>% group_by(boroughs,hour) %>% summarise(`number of trip`=n()) %>% 
        ungroup() %>% group_by(boroughs) %>% mutate(Percent=`number of trip`/sum(`number of trip`)) %>%
        ggplot()+
        geom_line(aes(x=hour,y=Percent,group=boroughs,color=boroughs),size=1)+
        theme_bw()+
        theme(text = element_text(face="bold",size=14),legend.position="bottom",
              plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black"))+
        labs(x="hour of a day",y="Number of Trips",title="Number of Trips in different borough during one day")
      ggarrange(p1,ncol = 1)
    }
    
    else if (input$keyfactor=="week"){ 
      p1 <- rshiny.data %>% group_by(boroughs,weekday) %>% summarise(`number of trip`=n()) %>% 
        ungroup() %>% group_by(boroughs) %>% mutate(Percent=`number of trip`/sum(`number of trip`)) %>%
        ggplot()+
        geom_line(aes(x=weekday,y=Percent,group=boroughs,color=boroughs),size=1)+
        theme_bw()+
        theme(text = element_text(face="bold",size=14),legend.position="bottom",
              plot.title=element_text(size=16, hjust=0.5, face="bold", colour="black"))+
        labs(x="day of the week",y="Number of Trips",title="Number of Trips in different borough during a week")
      ggarrange(p1,ncol = 1)
    }
    
    
    })

  }

shinyApp(ui,server)




