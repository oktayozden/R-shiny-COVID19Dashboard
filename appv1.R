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

# url1 <-'https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-21.xlsx'
#GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
#covid19 <- read_excel(tf)



url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
#download the dataset from the website to a local temporary file
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
#read the Dataset sheet into “R”
covid19 <- read_excel(tf)

covid19$country<-covid19$countriesAndTerritories
covid<- select(covid19, c("dateRep","day","month","cases","deaths","geoId","country"))
text2<-covid$country
text3<-data_frame(text2)
text3<-table(text3)
text4<-as.data.frame(text3)
state1<-text4$text3
state1<-as.character(state1)


covid_num<-select(covid19, c("day","cases", "deaths"))
a<-covid_num %>%
    adorn_totals("row")%>%slice(n())



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
                width = 6,height = 380, collapsible = T,
                plotlyOutput("wwplot")),
            box(title = "Death Distribution", solidHeader = T,
                width = 6,height = 380, collapsible = T,
                plotlyOutput("pie")))
    ) # body
    
)






# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observe({
        
        
        
        
        covid1<- select(covid, c("dateRep","day","month","cases","deaths","geoId","country"))%>%
            filter(country==(input$state))
        
        
        
        covid1<-covid1 %>% mutate(dateRep = ymd(dateRep))
        covid1<- covid1[order(covid1$dateRep),]
        
        covid1$cumcase<- cumsum(covid1[, 4])
        covid1$cumcase<-as.numeric(as.character(unlist(covid1$cumcase)))
        
        covid1$cumdeath<- cumsum(covid1[, 5])
        covid1$cumdeath<-as.numeric(as.character(unlist(covid1$cumdeath)))
        
        ##worldwide
        covidw1<-covid19
        tar<-covid19$dateRep[1]
        covidw1<-covidw1 %>% mutate(dateRep=ymd(dateRep))
        covidw1<-covidw1[order(covidw1$dateRep),]
        
        s<-covidw1[order(covidw1$country),]
        
        s<-s %>% group_by(country) %>%mutate(cumwcase=cumsum(cases))
        s<-filter(s,geoId !="CN")
        
        ##worldwide Pie Chart
        s1<-s %>% group_by(country) %>%mutate(cumwcase1=cumsum(deaths))
        s2<- filter(s1, dateRep==paste(tar))
        s3<-s2 %>%
            adorn_totals("row")%>%slice(n())
        s3<-s3$cumwcase1
        s2$ratio<-s2$cumwcase1/s3
        s2<-s2[order(s2$ratio,decreasing = TRUE),]
        
        rat<-s2[1:7,]
        
        
        output$case1 <- renderPlotly({
            
            fig <- plot_ly(covid1, x = ~dateRep, y = ~cumcase, name = ~paste(covid1[2,7]), width = 660, height = 300, type = 'scatter', mode = 'lines',
                           line = list(color = 'orange', width = 4)) %>%
                
                layout(title = ~paste(input$state))%>% plotly::config(displayModeBar = F)%>%layout(
                    xaxis = list(
                        title = 'Date')) %>%
                layout(
                    yaxis=list(title="")) %>% layout(autosize = F)
            fig
        })
        
        
        output$death1 <- renderPlotly({
            
            fig1 <- plot_ly(covid1, x = ~dateRep, y = ~cumdeath, name = ~paste(covid1[2,7]),width = 660, height = 300, type = 'scatter', mode = 'lines',
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
            
            fig2 <-  plot_ly(s, x = ~dateRep, y = ~cumwcase, color = ~country, hoverinfo = "text",width = 660, height = 300,
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
        
        output$pie <- renderPlotly({
            attach(s1)
            fig3<- plot_ly(rat, labels= ~country, values=~ratio, type ='pie',
                           textposition = 'inside',
                           textinfo = 'label+percent', width = 700, height = 330,
                           insidetextfont = list(color = '#FFFFFF'),
                           hoverinfo = 'text',
                           text = ~paste(cumwcase1, ' People in', country),
                           marker = list(colors = colors,
                                         line = list(color = '#FFFFFF', width = 9)),
                           showlegend = FALSE) %>% plotly::config(displayModeBar = F)
        })
        
        
        # value boxes
        output$cases <- renderValueBox({
            valueBox(paste0("Cases: ", a$cases), 
                     "Worldwide", icon = icon("fire"), color = "yellow")
        })
        output$death <- renderValueBox({
            valueBox(paste0("Death: ", a$deaths),
                     "Worldwide", icon = icon("skull"), color = "red")
        })
        output$reco <- renderValueBox({
            valueBox(paste0("Recovery: ", "130201"),
                     "Worldwide", icon = icon("plus-square"), color = "green")
        })
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
