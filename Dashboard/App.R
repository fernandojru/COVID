library(shinydashboard)
library(shiny)
library(leaflet)
library(dplyr)
library(xts)
library(dygraphs)
library(ggplot2)
library(scales)
library(plotly)

COVID_2<-read.csv("COVID19_Updated.csv")
Date<-as.Date(COVID_2$Date, format="%m/%d/%y") 
COVID_2$Date2<-Date

COVID_updated<-COVID_2 %>% filter(Date2==max(Date2))
COVID_3<-read.csv("COVID_Covariables.csv")

New_count<-function(x)
{
  Daily_cases<-numeric(length(x))
  
  for(i in length(x):2)
  {
    Daily_cases[i]=x[i] - x[i-1]
  }
  return(Daily_cases)
}

load("Forecasts.RData")

#=============================================UI=================================================
header <- dashboardHeader(title = "COVID-19 Outbreak dashboard",
                          dropdownMenu(type="notifications",
                                       notificationItem(text=paste("Last update", max(COVID_2$Date2)), status="info"),
                                       notificationItem(text=paste("Forecast until ",max(rownames(Forecasts$Italy)),
                                                                   " available for Costa Rica, Mexico, Italy & Lebanon"), status="info"),
                                       badgeStatus="primary")
                          )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(tagList(icon("procedures"), "Overview"),tabName = "overview"),
    menuItem(tagList(icon("procedures"),"Countries"),tabName = "country"),
    menuItem(tagList(icon("procedures"),"Statistics"),tabName = "statistics"),
    menuItem(tagList(icon("file-download"),"Data downloads"),tabName = "downloads")
            )
      )

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "overview",
            fluidRow(valueBoxOutput("Total_confirmed",width = 4),
                     valueBoxOutput("Total_Deaths",width = 4),
                     valueBoxOutput("Total_recovered",width = 4)),
            fluidRow(column(width = 6,
                            box(width = NULL,leafletOutput("Map_1"))),
                     column(width = 6,
                            tabBox(width=NULL,
                                   tabPanel("Total cases",dygraphOutput("dygraph_1")),
                                   tabPanel("New cases",dygraphOutput("dygraph_3")))))
            ),
    tabItem(tabName = "country",
            fluidRow(column(width = 6,
                            box(width = NULL,selectInput("Select_country","Select a Country",
                                                         choices = unique(COVID_2$Country.Region),
                                                         multiple = FALSE,selected ="Italy"))),
                     infoBoxOutput("Today_yesterday",width = 6)
                     ),
            fluidRow(column(width = 6,
                            tabBox(width = NULL,
                                   tabPanel("Total cases",dygraphOutput("dygraph_2")),
                                   tabPanel("New cases",dygraphOutput("dygraph_4")))),
                     column(width = 6,
                            box(width = NULL,plotOutput("Plot_1"))))
            ),
    tabItem(tabName = "statistics",
            fluidRow(column(width = 6,
                            box(width = NULL,plotlyOutput("Plot_2"))),
                     column(width = 6,
                            tabBox(width = NULL,
                                   tabPanel("HDI",plotlyOutput("Plot_3")),
                                   tabPanel("Health expenditure",plotlyOutput("Plot_4")),
                                   tabPanel("Measles",plotlyOutput("Plot_5")))))),
    tabItem(tabName = "downloads",
            fluidRow(column(width = 6,
                            box(width = NULL,selectInput("Select_country_download","Select a country",
                                                         choices=unique(COVID_2$Country.Region),
                                                         multiple = FALSE,selected = "Italy"))),
                     column(width = 4,
                            box(width = NULL,downloadButton("Download_data","Download data")))),
            fluidRow(column(width = 6,
                            box(width = NULL,tableOutput("Table_1")))
                     )
            )
    
  )
)

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin="black"
)


#============================================App server=====================================================================
server<-shinyServer(function(session, input, output) {

  #===========================Currently tab=====================
  output$Total_confirmed<-renderValueBox({
                                          valueBox(value = prettyNum(sum(COVID_updated$Confirmed),big.mark = " "),
                                                   subtitle = "Worldwide confirmed",icon=icon("procedures"),
                                                   color="yellow")
  })
  
  output$Total_Deaths<-renderValueBox({
    valueBox(value = prettyNum(sum(COVID_updated$Deaths),big.mark = " "),
             subtitle = "Worldwide deaths",icon=icon("cross"),
             color="red")
  })
  
  output$Total_recovered<-renderValueBox({
    valueBox(value = prettyNum(sum(COVID_updated$Recovered),big.mark = " "),
             subtitle = "Worldwide recovered",icon=icon("user-md"),
             color="green")
  })
  
  output$Map_1<-renderLeaflet({
    leaflet(width = "100%") %>% 
      addProviderTiles("CartoDB.DarkMatter") %>% 
      setView(lng = 0, lat = 10, zoom = 1.5) %>% 
      addCircleMarkers(data = COVID_updated, 
                       lng = ~ Long,
                       lat = ~ Lat,
                       radius = ~ log(Confirmed+1),
                       color = rgb(218/255,65/255,56/255),
                       fillOpacity = ~ ifelse(Confirmed > 0, 1, 0),
                       stroke = FALSE,
                       label = ~ paste(Province.State,",",Country.Region, ": ", Confirmed)
      )
  })
  
  COVID_2_Day<-reactive({
    COVID_2 %>% group_by(Date2) %>% summarise(World_confirmed=sum(Confirmed),
                                                                   World_deaths=sum(Deaths),
                                                                   World_recovered=sum(Recovered))
    })
  
  COVID_Day_confirmed_series<-reactive({
    xts(COVID_2_Day()$World_confirmed, order.by=COVID_2_Day()$Date2)
  })
  
  COVID_Day_deaths_series<-reactive({
    xts(COVID_2_Day()$World_deaths, order.by=COVID_2_Day()$Date2)
  })
  
  COVID_Day_recovered_series<-reactive({
    xts(COVID_2_Day()$World_recovered, order.by=COVID_2_Day()$Date2)
  })
  
  Day_summary<-reactive({
    cbind(COVID_Day_confirmed_series(),COVID_Day_deaths_series(),COVID_Day_recovered_series())
  })
  
  output$dygraph_1<-renderDygraph({
    dygraph(Day_summary(), main = "SARS-COV2-outbreak: Total worldwide cases", 
            xlab="Date", ylab="Total cases") %>% 
      dySeries("COVID_Day_confirmed_series..", "Total cases",drawPoints = TRUE, 
               pointSize = 3, color=rgb(53/255,116/255,199/255)) %>% 
      dySeries("COVID_Day_deaths_series..", "Total deaths",drawPoints = TRUE, 
               pointSize = 3, color=rgb(189/255,55/255,48/255)) %>% 
      dySeries("COVID_Day_recovered_series..", "Total recovered",drawPoints = TRUE, 
               pointSize = 3, color=rgb(69/255,136/255,51/255)) %>% 
      dyRangeSelector()
  })
  
  New_cases<-reactive({
    New_count(COVID_2_Day()$World_confirmed)
  })
  
  New_deaths<-reactive({
    New_count(COVID_2_Day()$World_deaths)
  })
  
  New_recovered<-reactive({
    New_count(COVID_2_Day()$World_recovered)
  })
  
  COVID_New_confirmed_series<-reactive({
    xts(New_cases(), order.by=COVID_2_Day()$Date2)
  })
  
  COVID_New_deaths_series<-reactive({
    xts(New_deaths(), order.by=COVID_2_Day()$Date2)
  })
  
  COVID_New_recovered_series<-reactive({
    xts(New_recovered(), order.by=COVID_2_Day()$Date2)
  })
  
  New_summary<-reactive({
    cbind(COVID_New_confirmed_series(),COVID_New_deaths_series(),COVID_New_recovered_series())
  })
  
  output$dygraph_3<-renderDygraph({
    dygraph(New_summary(), main = "SARS-COV2-outbreak: Total worldwide cases", 
            xlab="Date", ylab="Novel coronavirus cases",width = 750) %>% 
      dySeries("COVID_New_confirmed_series..", "New cases",drawPoints = TRUE, 
               pointSize = 3, color=rgb(53/255,116/255,199/255)) %>% 
      dySeries("COVID_New_deaths_series..", "New deaths",drawPoints = TRUE, 
               pointSize = 3, color=rgb(189/255,55/255,48/255)) %>% 
      dySeries("COVID_New_recovered_series..", "New recovered",drawPoints = TRUE, 
               pointSize = 3, color=rgb(69/255,136/255,51/255)) %>% 
      dyRangeSelector()
  })
    
  
  #===========================Country tab=====================
  COVID_2_Day_Countries<-reactive({
    COVID_2 %>% 
      filter(Country.Region %in% input$Select_country) %>% 
      group_by(Date2) %>% summarise(World_confirmed=sum(Confirmed))
  })
  
  COVID_Day_series_countries<-reactive({
    xts(COVID_2_Day_Countries()$World_confirmed, order.by=COVID_2_Day_Countries()$Date2)
  })
  
  output$dygraph_2<-renderDygraph({
    if(input$Select_country=="Costa Rica")
    {
      dygraph(Forecasts$`Costa Rica`, main="SARS-COV2-outbreak: Total Costa Rica cases",xlab="Date", ylab="Novel coronavirus cases",width = 750)%>%
        dySeries(c('Lower_limit', 'Forecast', 'Upper_limit'),label="Forecast",strokeWidth=2,
                 drawPoints = TRUE, pointSize = 2, color=rgb(189/255,44/255,47/255)) %>%
        dySeries("Actual",drawPoints = TRUE, strokeWidth=2, pointSize = 2,
                 color=rgb(10/255,44/255,119/255)) %>% 
        dyRangeSelector()
    } else if(input$Select_country=="Mexico")
    {
      dygraph(Forecasts$Mexico, main="SARS-COV2-outbreak: Total Mexico cases",xlab="Date", ylab="Novel coronavirus cases",width = 750)%>%
        dySeries(c('Lower_limit', 'Forecast', 'Upper_limit'),label="Forecast",strokeWidth=2,
                 drawPoints = TRUE, pointSize = 2, color=rgb(189/255,44/255,47/255)) %>%
        dySeries("Actual",drawPoints = TRUE, strokeWidth=2, pointSize = 2,
                 color=rgb(43/255,102/255,73/255)) %>% 
        dyRangeSelector()
    } else if(input$Select_country=="Italy")
    {
      dygraph(Forecasts$Italy, main="SARS-COV2-outbreak: Total Italy cases",xlab="Date", ylab="Novel coronavirus cases",width = 750)%>%
        dySeries(c('Lower_limit', 'Forecast', 'Upper_limit'),label="Forecast",strokeWidth=2,
                 drawPoints = TRUE, pointSize = 2, color=rgb(190/255,59/255,61/255)) %>%
        dySeries("Actual",drawPoints = TRUE, strokeWidth=2, pointSize = 2,
                 color=rgb(64/255,143/255,78/255)) %>% 
        dyRangeSelector()
      
    } else if(input$Select_country=="Lebanon")
    {
      dygraph(Forecasts$Lebanon, main="SARS-COV2-outbreak: Total Lebanon cases",xlab="Date", ylab="Novel coronavirus cases",width = 750)%>%
        dySeries(c('Lower_limit', 'Forecast', 'Upper_limit'),label="Forecast",strokeWidth=2,
                 drawPoints = TRUE, pointSize = 2, color=rgb(218/255,55/255,50/255)) %>%
        dySeries("Actual",drawPoints = TRUE, strokeWidth=2, pointSize = 2,
                 color=rgb(73/255,163/255,90/255)) %>% 
        dyRangeSelector()
    } else if(input$Select_country=="Colombia")
    {
      dygraph(Forecasts$Colombia, main="SARS-COV2-outbreak: Total Colombia cases",xlab="Date", ylab="Novel coronavirus cases",width = 750)%>%
        dySeries(c('Lower_limit', 'Forecast', 'Upper_limit'),label="Forecast",strokeWidth=2,
                 drawPoints = TRUE, pointSize = 2, color=rgb(17/255,57/255,141/255)) %>%
        dySeries("Actual",drawPoints = TRUE, strokeWidth=2, pointSize = 2,
                 color=rgb(246/255,209/255,75/255)) %>% 
        dyRangeSelector()
    } else if(input$Select_country=="Chile")
    {
      dygraph(Forecasts$Chile, main="SARS-COV2-outbreak: Total Chile cases",xlab="Date", ylab="Novel coronavirus cases",width = 750)%>%
        dySeries(c('Lower_limit', 'Forecast', 'Upper_limit'),label="Forecast",strokeWidth=2,
                 drawPoints = TRUE, pointSize = 2, color=rgb(196/255,60/255,44/255)) %>%
        dySeries("Actual",drawPoints = TRUE, strokeWidth=2, pointSize = 2,
                 color=rgb(16/255,59/255,160/255)) %>% 
        dyRangeSelector()
      
    } else
    {
      dygraph(COVID_Day_series_countries(),
              main=paste("SARS-COV2-outbreak: Total cases in ",input$Select_country),
              xlab="Date", ylab="Novel coronavirus cases") %>% 
        dySeries("V1", input$Select_country,drawPoints = TRUE, pointSize = 3,
                 color=rgb(120/255,28/255,109/255)) %>% 
        dyRangeSelector()
    }
  })
  
  New_country_cases<-reactive({
    New_count(COVID_2_Day_Countries()$World_confirmed)
  })
  
  COVID_New_series_country<-reactive({
    xts(New_country_cases(), order.by=COVID_2_Day_Countries()$Date2)
  })
  
  output$dygraph_4<-renderDygraph({
    dygraph(COVID_New_series_country(),
            main=paste("SARS-COV2-outbreak: New cases in ",input$Select_country),
            xlab="Date", ylab="New novel cases") %>% 
      dySeries("V1", input$Select_country,drawPoints = TRUE, pointSize = 3,
               color=rgb(120/255,28/255,109/255)) %>% 
      dyRangeSelector()
  })
  
  
  
  COVID_top_countries<-reactive({
    COVID_2 %>% filter(Date2==max(Date2)) %>% 
      group_by(Country.Region) %>% summarise(Total_confirmed=sum(Confirmed)) %>% 
      top_n(10,Total_confirmed) %>% arrange(desc(Total_confirmed))
  })
  
  output$Plot_1<-renderPlot({
    ggplot(data=COVID_top_countries()
           , aes(x=Total_confirmed,y=reorder(Country.Region,Total_confirmed))) +
      geom_bar(stat ="identity",alpha=0.8,fill=rgb(237/255,105/255,37/255)) +
      geom_text(aes(label=Total_confirmed), vjust=0.5, hjust=0.9,color="black", size=3.5) +
      scale_x_continuous(labels = comma) +
      labs(title = paste("Top 10 countries with confirmed cases as of ",max(COVID_2$Date2)),
           x = "Confirmed cases",
           y = "Country") +
      theme_minimal()
  })
  

  Today_confirmed<-reactive({
    COVID_2_Day_Countries()[length(COVID_2_Day_Countries()$World_confirmed),2]
    })
  
  Yesterday_confirmed<-reactive({
    COVID_2_Day_Countries()[length(COVID_2_Day_Countries()$World_confirmed)-1,2]
  })
  
  Before_yesterday_confirmed<-reactive({
    COVID_2_Day_Countries()[length(COVID_2_Day_Countries()$World_confirmed)-2,2]
  })
  
  Today_increase<-reactive({
    as.numeric(Today_confirmed()-Yesterday_confirmed())
  })
  
  Yesterday_increase<-reactive({
    as.numeric(Yesterday_confirmed()-Before_yesterday_confirmed())
  })
  
  Logic_X1<-reactive({
    Today_increase()>Yesterday_increase()
  })
  
  Icon_Today_yesterday<-reactive({
    ifelse(Logic_X1(),"thumbs-down","thumbs-up")
  })
  
  Color_Today_yesterday<-reactive({
    ifelse(Logic_X1(),"red","green")
  })
  
  Inrease_decrease<-reactive({
    ifelse(Logic_X1(),"increase","decrease")
  })
  
  Number_Today_yesterday<-reactive({
    round((abs(Today_increase()-Yesterday_increase())/Yesterday_increase())*100,2)
  })
  
  output$Today_yesterday<-renderInfoBox({
    infoBox(title= paste(Inrease_decrease()," of daily cases in ",input$Select_country," of"),
            value = paste(Number_Today_yesterday(),"%"),
            subtitle = "From yesterday",
            icon = icon(Icon_Today_yesterday()),color = Color_Today_yesterday(),
            fill=TRUE)
  })
  
  
  #===========================Statistics tab=====================
  
  output$Plot_2<-renderPlotly({
    plot_ly(COVID_updated, x = ~Confirmed, y = ~Deaths, z = ~Recovered ) %>% 
      add_markers(text= ~Country.Region ,hoverinfo= "text",
                  marker = list(color=rgb(189/255,55/255,48/255))) %>% 
      layout(title="Confirmed cases Vs. Deaths Vs. Recovered",scene = list(
        xaxis = list(title = 'Confirmed'),
        yaxis = list(title = 'Deaths'),
        zaxis = list(title = 'Recovered'))) 
  })
  
  HDI_Plot<-reactive({
    ggplot(data=COVID_3,aes(x=log(Cases_million),y=Year_2018,
                            size=Recovered_percentage,text=Country.Region)) +
      geom_point(color="black",fill=rgb(237/255,105/255,37/255),shape=21,alpha=0.6) +
      scale_size(range = c(3,15), name="Recovered \n percentage") +
      theme_minimal() + 
      theme(legend.position="bottom") +
      labs(title="HDI Vs. logarithmus of COVID-19 cases by million inhabitants \n and proportion of recovered",
           x="ln(Cases/1M population)",
           y="HDI")
  })
  
  output$Plot_3<-renderPlotly({
    ggplotly(HDI_Plot(),tooltip = c("text"))
  })
  
  Health_expenditure_Plot<-reactive({
    ggplot(data=COVID_3,aes(x=log(Cases_million),y=Expenditure_2016,
                            size=Recovered_percentage,text=Country.Region)) +
      geom_point(color="black",fill=rgb(204/255,197/255,126/255),shape=21,alpha=0.6) +
      scale_size(range = c(3,15), name="Recovered \n percentage") +
      theme_minimal() + 
      theme(legend.position="bottom") +
      labs(title="Health expenditure (% of GDP) Vs. logarithmus of COVID-19 cases by million inhabitants \n and proportion of recovered",
           x="ln(Cases/1M population)",
           y="% of GDP in health")
  })
  
  output$Plot_4<-renderPlotly({
    ggplotly(Health_expenditure_Plot(),tooltip = c("text"))
  })
  
  Measles_Plot<-reactive({
    ggplot(data=COVID_3,aes(x=log(Cases_million),y=Measles_2018,
                            size=Recovered_percentage,text=Country.Region)) +
      geom_point(color="black",fill=rgb(120/255,28/255,109/255),shape=21,alpha=0.6) +
      scale_size(range = c(3,15), name="Recovered \n percentage") +
      theme_minimal() + 
      theme(legend.position="bottom") +
      labs(title="% infants lacking measles immunization Vs. logarithmus of COVID-19 cases by million inhabitants \n and proportion of recovered",
           x="ln(Cases/1M population)",
           y="% of infants")
  })
  
  output$Plot_5<-renderPlotly({
    ggplotly(Measles_Plot(),tooltip = c("text"))
  })
  
  #=================================Downloads=========================
  
  COVID_2_Day_Countries_Download<-reactive({
    COVID_2 %>% 
      filter(Country.Region %in% input$Select_country_download) %>% 
      group_by(Date2) %>% summarise(World_confirmed=sum(Confirmed)) %>% 
      mutate(Date2=as.character(Date2))
  })
  
  COVID_To_Download<-reactive({
    if(input$Select_country_download=="Costa Rica")
    {
      Forecasts$`Costa Rica`
    } else if(input$Select_country_download=="Mexico")
    {
      Forecasts$Mexico
    } else if(input$Select_country_download=="Italy")
    {
      Forecasts$Italy
    } else if(input$Select_country_download=="Lebanon")
    {
      Forecasts$Lebanon
    } else if(input$Select_country_download=="Colombia")
    {
      Forecasts$Colombia
    } else if(input$Select_country_download=="Chile")
    {
      Forecasts$Chile
    } else
    {
      COVID_2_Day_Countries_Download()
    }
  })
  
  output$Table_1<-renderTable({
    COVID_To_Download()
  },rownames = TRUE)
  
  output$Download_data<-downloadHandler(
    filename = function() {
      paste(input$Select_country_download,"_Data_Forecast.csv")
    },
    content = function(file)
    {
      write.csv(COVID_To_Download(),file,row.names = TRUE)
    }
  )
  
  
  
})

shinyApp(ui, server)

  
  
  
  
  
  