library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(httr)

# load files from Canada Nutrient File

url1 <-'https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-18.xls'

GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
covid19 <- read_excel(tf)
covid19$country<-covid19$`Countries and territories`
covid<- select(covid19, c("DateRep","Day","Month","Cases","Deaths","GeoId","country"))
text2<-covid$country
text3<-data_frame(text2)
text3<-table(text3)
text4<-as.data.frame(text3)
state1<-text4$text3
state1<-as.character(state1)


covid_num<-select(covid19, c("Day","Cases", "Deaths"))
a<-covid_num %>%
    adorn_totals("row")
total<-a[5530,]






ui <- dashboardPage(
    dashboardHeader(title = "Covid-19 Dash"),
    dashboardSidebar(
        selectizeInput(
            'state', 'State', choices = state1 ,selected = "China",
            options = list(
                placeholder = 'Type to search for Country', create=TRUE)
        )),
    
    dashboardBody(
        fluidRow(
            valueBoxOutput("cases"),
            valueBoxOutput("reco"),
            valueBoxOutput("death")),
        
        fluidRow(
            box(title = "Total Case", solidHeader = T,
                width = 6, height = 380, collapsible = T,
                plotlyOutput("case1")),
            
            box(title = "Total Death", solidHeader = T,
                width = 6, height = 380, collapsible = T,
                plotlyOutput("death1"))),
        fluidRow(
            box(title = "Total Case", solidHeader=T,
                width = 12, collapsible = T,
                plotlyOutput("wwplot")))
    ) # body
    
)






# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observe({
        
        
        
        
        covid1<- select(covid, c("DateRep","Day","Month","Cases","Deaths","GeoId","country"))%>%
            filter(country==(input$state))
        
        
        
        covid1<-covid1 %>% mutate(DateRep = ymd(DateRep))
        covid1<- covid1[order(covid1$DateRep),]
        
        covid1$cumcase<- cumsum(covid1[, 4])
        covid1$cumcase<-as.numeric(as.character(unlist(covid1$cumcase)))
        
        covid1$cumdeath<- cumsum(covid1[, 5])
        covid1$cumdeath<-as.numeric(as.character(unlist(covid1$cumdeath)))
        
        ##worldwide
        covidw1<-covid19
        covidw1<-covidw1 %>% mutate(DateRep=ymd(DateRep))
        covidw1<-covidw1[order(covidw1$DateRep),]
        
        s<-covidw1[order(covidw1$country),]
        
        s<-s %>% group_by(country) %>%mutate(cumwcase=cumsum(Cases))
        
        s<-filter(s,GeoId !="CN")
        
        
        
        library(dplyr)
        iris %>%
            group_by(Species) %>%
            mutate(cum_sep_len = cumsum(Sepal.Length))
        
        
        
        output$case1 <- renderPlotly({
            
            fig <- plot_ly(covid1, x = ~DateRep, y = ~cumcase, name = ~paste(covid1[2,7]), width = 660, height = 300, type = 'scatter', mode = 'lines',
                           line = list(color = 'orange', width = 4)) %>%
                
                layout(title = ~paste(input$state))%>% plotly::config(displayModeBar = F)%>%layout(
                    xaxis = list(
                        title = 'Date')) %>%
                layout(
                    yaxis=list(title="")) %>% layout(autosize = F)
            fig
        })
        
        
        output$death1 <- renderPlotly({
            
            fig1 <- plot_ly(covid1, x = ~DateRep, y = ~cumdeath, name = ~paste(covid1[2,7]),width = 660, height = 300, type = 'scatter', mode = 'lines',
                            line = list(color = 'red', width = 4)) %>%
                layout(title = ~paste(input$state))%>%  plotly::config(displayModeBar = F) %>% layout(
                    xaxis = list(
                        title = 'Date'
                    )) %>%
                layout(
                    yaxis=list(title="")) %>% layout(autosize = F)
            
            fig1
            
            
        })
        
        output$wwplot <- renderPlotly({
            
            fig2 <-  plot_ly(s, x = ~DateRep, y = ~cumwcase, color = ~country, hoverinfo = "text",width = 1350, height = 380,
                             hovertext = paste("State :", s$country,
                                               "<br> Cases :", s$cumwcase),
                             name = "", type = 'scatter', mode = 'lines',
                             transforms = list(type = 'groupby',
                                               groups = s$country),line = list(width = 3))%>%
                layout(title = ~paste("Total Cases in Worldwide")) %>% 
                layout(xaxis = list(title = 'Date')) %>%
                layout(yaxis=list(title=""))%>%
                plotly::config(displayModeBar = F) %>% layout(showlegend = FALSE)%>% 
                layout(autosize = F)
            
            fig2
            
        })
        
        
        # value boxes
        output$cases <- renderValueBox({
            valueBox(paste0("Cases: ", total$Cases), 
                     "Worldwide", icon = icon("fire"), color = "yellow")
        })
        output$death <- renderValueBox({
            valueBox(paste0("Death: ", total$Deaths),
                     "Worldwide", icon = icon("skull"), color = "red")
        })
        output$reco <- renderValueBox({
            valueBox(paste0("Recovery: ", "82301"),
                     "Worldwide", icon = icon("plus-square"), color = "green")
        })
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)