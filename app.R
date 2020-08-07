#Loading required packages
library(shinyEffects)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(sp)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(lubridate)
#Preparing the Data for both maps and values-----------
datacsv <- read_csv("Complete NCDC Data.csv")
#testscsv <- read_csv("Tests Done.csv")
mypolygons <- readRDS("gadm36_NGA_1_sp.rds")
#testscsv <- testscsv %>% 
#  mutate(Date=mdy(Date))
#glimpse(datacsv)

#Create a column for daily tests
#testscsv <- testscsv %>% 
#  mutate(DailyTests = `No. Of Tests`-(lag(`No. Of Tests`)))


#Create a column for days since first case
datacsv <- datacsv %>% 
  arrange(Date) %>% 
  group_by(State) %>% 
  dplyr::mutate(Day=row_number()) %>% 
  ungroup()

#Convert names to syntatic versions
datacsv <- datacsv %>% 
  transmute(Date=Date,State=State,Day=Day,
            ConfirmedCases=`Confirmed Cases`,AdmittedCases=`Cases on Admission`,DischargedCases=`Discharged Cases`,Deaths=Deaths
  )

#Calculate Number of New Cases for each column
datacsv <- datacsv %>% 
 group_by(State) %>% 
  arrange(State,Date) %>% 
  mutate(newc=lag(ConfirmedCases,default = 0),NewConfirmedCases=ConfirmedCases-newc,
         newa=lag(AdmittedCases,default = 0),NewAdmittedCases=AdmittedCases-newa,
         newdi=lag(DischargedCases,default = 0),NewDischargedCases=DischargedCases-newdi,
         newde=lag(Deaths,default = 0),NewDeaths=Deaths-newde) %>% 
 dplyr::select(Date:Deaths,NewConfirmedCases,NewAdmittedCases,NewDischargedCases,NewDeaths) %>% 
  ungroup()
  
#Filter out the latest values for each category
latest <- datacsv %>%
  filter(Date==max(Date)) %>% 
  summarise(ConfirmedCases=sum(ConfirmedCases),
           AdmittedCases = sum(AdmittedCases),
          DischargedCases=sum(DischargedCases),
          Deaths=sum(Deaths),
          NewConfirmedCases=sum(NewConfirmedCases),
          NewDischargedCases=sum(NewDischargedCases),
          NewAdmittedCases=sum(NewAdmittedCases),
          NewDeaths=sum(NewDeaths))

  

#Daily sums of cases from all states
dailydatacsv <- datacsv %>% 
  dplyr::group_by(Day) %>% 
  dplyr::summarise(Date=max(Date),
            `Confirmed Cases`=sum(ConfirmedCases),
            `Admitted Cases`=sum(AdmittedCases),
            `Discharged Cases`=sum(DischargedCases),
            Deaths=sum(Deaths),
            `New Cases`=sum(NewConfirmedCases),
            `New Discharged Cases`=sum(NewDischargedCases),
            `New Admitted Cases`=sum(NewAdmittedCases),
            `New Deaths`=sum(NewDeaths)) %>%
  dplyr::mutate(State="National") %>% 
  dplyr::ungroup()

test <- tibble()
for (stat in unique(datacsv$State)) {
  testdat <- datacsv %>% 
                filter(State==stat) %>% 
                group_by(Day) %>% 
                summarise(Date=max(Date),
                          `Confirmed Cases`=sum(ConfirmedCases),
                          `Admitted Cases`=sum(AdmittedCases),
                          `Discharged Cases`=sum(DischargedCases),
                          Deaths=sum(Deaths),
                          `New Cases`=sum(NewConfirmedCases),
                          `New Discharged Cases`=sum(NewDischargedCases),
                          `New Admitted Cases`=sum(NewAdmittedCases),
                          `New Deaths`=sum(NewDeaths)) %>% 
    mutate(State=stat) %>% 
                ungroup()
  
  test=rbind(test,testdat)
}
dailydatacsv <- rbind(dailydatacsv,test)


dailydatacsv <- dailydatacsv %>% 
  dplyr::group_by(State) %>%
  mutate(Roll_avg_deaths = round(zoo::rollmeanr(`New Deaths`,k=7,fill = NA,),0),
         Roll_avg_cases = round(zoo::rollmeanr(`New Cases`,k=7,fill = NA,),0))

#Plot 5 Number of Cases and deaths by state
plot5 <- plot_ly(data = datacsv %>% 
                   filter(Day==max(Day),NewConfirmedCases!=0),
                 type="bar",
                 y = ~reorder(State,NewConfirmedCases),
                 x = ~NewConfirmedCases,
                 hoverinfo = 'text',
                 text = ~paste0(
                   "State: ",State, 
                   "<br>New Cases: ", NewConfirmedCases))


#Plot 4 Number of Deaths by state
plot6 <- plot_ly(data = datacsv %>% 
                   filter(Day==max(Day),NewDeaths!=0),
                 type="bar",
                 y = ~reorder(State,NewDeaths),
                 x = ~NewDeaths,
                 hoverinfo = 'text',
                 text = ~paste0(
                   "State: ",State, 
                   "<br>New Deaths: ", NewDeaths))
#Plot 5 Number of Cases and deaths by state
plot7 <- plot_ly(data = datacsv %>% 
                   filter(Day==max(Day),ConfirmedCases!=0),
                 type="bar",
                 y = ~reorder(State,ConfirmedCases),
                 x = ~ConfirmedCases,
                 hoverinfo = 'text',
                 text = ~paste0(
                   "State: ",State, 
                   "<br>Cases: ", ConfirmedCases))


#Plot 4 Number of Deaths by state
plot8 <- plot_ly(data = datacsv %>% 
                   filter(Day==max(Day),Deaths!=0),
                 type="bar",
                 y = ~reorder(State,Deaths),
                 x = ~Deaths,
                 hoverinfo = 'text',
                 text = ~paste0(
                   "State: ",State, 
                   "<br>Deaths: ", Deaths))
mapcsv <- datacsv %>%
  filter(Date==max(Date)) %>% 
  mutate(State=ifelse(State=="Nasarawa","Nassarawa",State)) 

#mapcsv <- plyr::join(mapcsv,testscsv,by="State",type="full",match="all")
#mapcsv <- plyr::join(mapcsv,by="State",type="full",match="all")

fulldata <- sp::merge(mapcsv,mypolygons,by.x="State",by.y="NAME_1")

postdate <- max(datacsv$Date)
realdate <- format(postdate+1, "%d-%B-%Y")
chartdate <- format(postdate, "%d-%B-%Y")

#UI Starts here------------------
ui <- dashboardPage(
#Naming and configuring the Header of the dashboard-------------
  dashboardHeader(title = "COVID-19 Dashboard"),
#Naming and configuring the sidebar of the dashboard-----------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Maps",tabName = "Maps",icon = icon("map")),
      menuItem("Charts",tabName = "Charts", icon = icon("th")),
      a("Data Source",href="http://covid19.ncdc.gov.ng",target="_blank"),
      p(paste0("Last Updated: ",realdate))
      #menuItem(selectInput(inputId = "cases",label = "Data to display",choices = selectChoices)),
      #menuItem(actionButton(inputId = "cli",label = "Update"))
    )
  ),
#Naming and configuring the body of the dashboard----
  dashboardBody(
    tabItems(
      tabItem(tabName = "Maps",
#Top row of 4 boxes for summary numbers------------
              fluidRow(
                setShadow(class="small-box"),
          valueBoxOutput("newcases",width = 2),
          valueBoxOutput("newdeaths",width = 2),
          valueBoxOutput("activecases",width = 2),
          valueBoxOutput("confirmedcases",width = 2),
          valueBoxOutput("deaths",width = 2),
          valueBoxOutput("dischargedcases",width = 2)
      ),
#Next row of one box for a map---------------
      fluidRow(
        leafletOutput("mymap1",height = 500)
      )
        
      
      ),
#Charts tab---------------
      tabItem(tabName = "Charts", 
              fluidRow(selectizeInput(inputId = "States",label = "Select State",choices = unique(dailydatacsv$State),selected = "National",multiple = T,options = list(maxItems = 5, placeholder = 'Select a state',closeAfterSelect=F))),
              fluidRow(
                box(plotlyOutput("plot1"),width = 12)
              ),
              fluidRow(
                box(plotlyOutput("plot2"),width = 12)
              ),
              fluidRow(
                box(plotlyOutput("plot3"),width = 12)
              ),
              fluidRow(
                box(plotlyOutput("plot4"),width = 12)
              ),
             fluidRow(
                box(plot5 %>% config(displayModeBar=F) %>% layout(title=list(text=paste0("New Cases by State (",chartdate,")"))) %>%layout(xaxis=list(fixedrange=T,visible=T,title="")) %>% layout(yaxis=list(fixedrange=T,title="",tickmode="linear",nticks=10))
                    ,width = 6),
               box(plot6 %>% config(displayModeBar=F) %>% layout(title=list(text=paste0("New Deaths by State (",chartdate,")"))) %>%layout(xaxis=list(fixedrange=T,title="")) %>% layout(yaxis=list(fixedrange=T,title="",tickmode="linear",nticks=10))
                   ,width = 6)
             ),
             fluidRow(
               box(plot7 %>% config(displayModeBar=F) %>% layout(title=list(text="Total Cases in each State")) %>%layout(xaxis=list(fixedrange=T,visible=T,title="")) %>% layout(yaxis=list(fixedrange=T,title="",tickmode="linear",nticks=10))
                   ,width = 6),
               box(plot8 %>% config(displayModeBar=F) %>% layout(title=list(text="Total Deaths in each State")) %>%layout(xaxis=list(fixedrange=T,title="")) %>% layout(yaxis=list(fixedrange=T,title="",tickmode="linear",nticks=10))
                   ,width = 6)
             )
          )
    )
  )
)

#Server starts here------------------
server <- function(input, output){
  output$newcases <- renderValueBox({
    valueBox(latest$NewConfirmedCases,paste0("New Cases \n(Last 24 Hrs)"),icon =icon("arrow-up"),color = "yellow")
  })
  output$confirmedcases <- renderValueBox({
    valueBox(format(latest$ConfirmedCases,big.mark = ","),paste0("Total Cases"),icon =icon("hospital"),color = "yellow")
  })
  output$dischargedcases <- renderValueBox({
    valueBox(format(latest$DischargedCases,big.mark = ","),paste0("Total Recovered"),icon =icon("home"),color = "green")
 })
  output$newdeaths <- renderValueBox({
    valueBox(format(latest$NewDeaths,big.mark = ","),paste0("New Deaths \n(Last 24 Hrs)"),icon =icon("arrow-up"),color = "red")
  })
  output$activecases <- renderValueBox({
    valueBox(format(latest$AdmittedCases,big.mark = ","),paste0("Total Active Cases"),icon =icon("hospital"),color = "yellow")
  })
  output$deaths <- renderValueBox({
    valueBox(format(latest$Deaths,big.mark = ","),paste0("Total Deaths"),icon =icon("hospital"),color = "red")
 })
  output$plot1 <- renderPlotly({
     plot1 <-  dailydatacsv %>% 
      filter(State %in% input$States) %>% 
      #pivot_longer(cols = c("Confirmed Cases"), names_to = "variable" , values_to = "Cases") %>% 
      ggplot(aes(x=Date,y=`Confirmed Cases`))+
      geom_line(aes(color=State))+
      geom_point(aes(color=State),size=0.6)+
      labs(title = "Confirmed COVID-19 Cases in Nigeria",subtitle = "Daily totals for cases",x="Date",y="Cases")+
       theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 45),legend.position = "right")+
       scale_x_date(breaks = "2 week",date_labels = format( "%d-%B"))
    
    ggplotly(plot1,tooltip = c("State","Date","Confirmed Cases"))%>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=T)) %>% layout(yaxis=list(fixedrange=T))
    
  })
  output$plot2 <- renderPlotly({
      plot2 <- dailydatacsv %>% 
      filter(State %in% input$States) %>% 
      #pivot_longer(cols = c("Deaths"), names_to = "variable" , values_to = "Deaths") %>% 
      ggplot(aes(x=Date,y=Deaths))+
      geom_line(aes(color=State))+
      geom_point(aes(color=State),size=0.6)+
      labs(title = "Confirmed COVID-19 Related Deaths in Nigeria",x="Date",y="Deaths")+
        theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 45),legend.position = "right")+
        scale_x_date(breaks = "2 week",date_labels = format( "%d-%B"))
      
      ggplotly(plot2,tooltip = c("State","Date","Deaths"))%>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=T)) %>% layout(yaxis=list(fixedrange=T))
  })
  output$plot3 <- renderPlotly({
     plot3 <-  dailydatacsv %>% 
      filter(State == "National") %>% 
      #pivot_longer(cols = c("New Cases"), names_to = "variable" , values_to = "New Cases") %>% 
      ggplot(aes(x=Date,y=`New Cases`))+
      geom_col(aes(fill=State))+
      geom_line(aes(y=Roll_avg_cases),size=0.5)+
       #geom_line(data = testscsv, aes(x=Date,y=`No. Of Tests`))+
       #geom_point(data = testscsv, aes(x=Date,y=`No. Of Tests`),size=0.6)+
      labs(title = "Daily totals for new COVID 19 cases in Nigeria",x="Date",y="New Cases")+
       theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 45),legend.position = "none")+
       scale_x_date(breaks = "2 week",date_labels = format( "%d-%B"))
    
     ggplotly(plot3,tooltip=c("Date","New Cases"))%>% style(hoverinfo="none",traces =c(2,3) ) %>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=T)) %>% layout(yaxis=list(fixedrange=T))
  })
  output$plot4 <- renderPlotly({
  plot4 <- dailydatacsv %>% 
      filter(State == "National") %>% 
      #pivot_longer(cols = c("New Deaths"), names_to = "variable" , values_to = "New Deaths") %>% 
      ggplot(aes(x=Date,y=`New Deaths`))+
      geom_col(aes(fill=State))+
      geom_line(aes(y=Roll_avg_deaths),size=0.5)+
      labs(title = "Daily totals for new COVID 19 related deaths in Nigeria",x="Date",y="New Deaths")+
    theme(axis.title.y = element_blank(),axis.text.x = element_text(angle = 45),legend.position = "none")+
    scale_x_date(breaks = "2 week",date_labels = format( "%d-%B"))
    
    ggplotly(plot4,tooltip = c("Date","New Deaths")) %>% style(hoverinfo="none",traces =c(2,3) )%>% config(displayModeBar=F) %>% layout(xaxis=list(fixedrange=T)) %>% layout(yaxis=list(fixedrange=T))
   })
  #Data for the map-----------------------------
  output$mymap1 <- renderLeaflet({
    newx <- fulldata 
    mypolygons@data <- 
      data.frame(mypolygons@data, 
                 newx[match(mypolygons@data[,"NAME_1"], 
                            newx[,"State"]),])
    popup <- 
      paste0(
        "<strong>State: </strong>", mypolygons@data$State, 
        #"<br><strong>Tests Done: </strong>", mypolygons@data$TestsDone,
        "<br><strong>Cases: </strong>", mypolygons@data$ConfirmedCases,
        "<br><strong>Deaths: </strong>", mypolygons@data$Deaths
        ) %>% 
      lapply(htmltools::HTML)
    
    pal <- colorBin("Reds", domain = newx$ConfirmedCases,bins = c(1,50,100,500,1000,5000,10000,max(newx$ConfirmedCases)))
    
    leaflet() %>% 
      addPolygons(
        data=mypolygons, weight = 2, color = "black", 
        fillOpacity = 0.9,  fillColor = ~pal(mypolygons$ConfirmedCases),label = popup) %>%  
      addTiles() %>% 
      addLegend("bottomright", pal = pal, values = newx$ConfirmedCases,
                title = "Number of Cases",
                opacity = 1) 
    
  })

  
}

#Shinyapp----------------------
shinyApp(ui = ui, server = server)

