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
url1 <-'https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-31.xlsx'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
covid19 <- read_excel(tf)

##url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
#download the dataset from the website to a local temporary file
##GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
#read the Dataset sheet into “R”
##covid19 <- read_excel(tf)

covid19$country<-covid19$countriesAndTerritories
covid<- select(covid19, c("dateRep","day","month","cases","deaths","geoId","country"))
text2<-covid$country
text3<-data_frame(text2)
text3<-table(text3)
text4<-as.data.frame(text3)
state1<-text4$text3
state1<-as.character(state1)
state2<-
    
    covid_num<-select(covid19, c("day","cases", "deaths"))
a<-covid_num %>%
    adorn_totals("row")%>%slice(n())

title <- tags$a(tags$img(
    src="https://raw.githubusercontent.com/oktayozden/R-shiny-COVID19Dashboard/master/covidicon.png",
                         height=45, width=45),'Covid-19 Dash')



ui <- dashboardPage(skin = "black",
      dashboardHeader(title = title,
      tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/oktay-özden-36340297/" ,icon("linkedin"), "My Profile", target="_blank")),
      tags$li(class="dropdown",tags$a(href="https://github.com/oktayozden/R-shiny-COVID19Dashboard", icon("github"), "Source Code", target="_blank"))),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Analytics", tabName="Analytics",icon = icon("chart-line")),
                            selectizeInput("state", 'Select Country', choices = state1 ,selected = "China",
                                           options = list(placeholder = 'Type to search for Country', create=TRUE)),
                            menuItem("Comparison", tabName="Comparison", icon = icon("balance-scale")),
                            checkboxGroupInput("checkGroup1", label = h4("Click for Comparison"), 
                                               choices = list("Turkey" = "Turkey", "Italy" = "Italy", "Spain" = "Spain",
                                                              "China"="China", "Germany"="Germany","Iran"="Iran","France"="France",
                                                              "UK"="United_Kingdom", "USA"="United_States_of_America", "Belgium"="Belgium",
                                                              "Netherlands"="Netherlands","Austria"="Austria", "South Korea"="South_Korea",
                                                              "Canada"="Canada", "Portugal"="Portugal","Israel"="Israel","Brazil"="Brazil",
                                                              "Australia"="Australia","Norway"="Norway","Sweden"="Sweden"),
                                               selected = "Turkey")
                            
                        )
                    ),
                    
                    dashboardBody(tabItems
                                  (tabItem(tabName = "Analytics",
                                           fluidRow(width = 4,
                                                    valueBoxOutput("cases"),
                                                    valueBoxOutput("reco"),
                                                    valueBoxOutput("death")),
                                           
                                           fluidRow(
                                               box(title = "Total Case", solidHeader = TRUE,
                                                   width = 6, height = 380, collapsible = T,
                                                   plotlyOutput("case1")),
                                               
                                               box(title = "Total Death", solidHeader = TRUE,
                                                   width = 6, height = 380, collapsible = T,
                                                   plotlyOutput("death1"))),
                                           fluidRow(
                                               box(title = "Total Case", solidHeader=TRUE,
                                                   width = 6,height = 380, collapsible = T,
                                                   plotlyOutput("wwplot")),
                                               box(title = "Death Distribution", solidHeader = TRUE,
                                                   width = 6,height = 380, collapsible = T,
                                                   plotlyOutput("pie")))),
                                      
                                      
                                      tabItem(tabName = "Comparison",
                                              fluidRow(box(title = "Comparison of Cases among Countries", solidHeader = TRUE,
                                                           width = 12, height = 700, collapsible = T,
                                                           plotlyOutput("comparison"))))
                                  )
                                  
                                  #tabItem(tabName = 'wo',
                                  #fluidRow())
                                  
                                  
                    )
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
        inc<-s
        #incremental speed after first case
        inc1<-inc[!inc$cumwcase==0,]
        inc2<-inc1 %>% group_by(country) %>% mutate(n = row_number())
        inc3<- inc2 %>% filter(country %in% c(paste0(input$checkGroup1)))       
        
        ##binding
        #inc3<-inc3a %>% 
        #   bind_rows(inc3b) %>% bind_rows(inc3c)
        
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
            fig3
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
            valueBox(paste0("Recovery: ", "182315"),
                     "Worldwide", icon = icon("plus-square"), color = "green")
        })
        
        
        #comparison
        output$comparison<- renderPlotly({
            fig30 <-  plot_ly(inc3, x = ~n, y = ~cumwcase, color = ~country, hoverinfo = "text",width = 1600, height = 620,
                              hovertext = paste("State :", inc3$country,
                                                "<br> Cases :", inc3$cumwcase),
                              name = "", type = 'scatter', mode = 'lines',
                              transforms = list(type = 'groupby',
                                                groups = inc3$country),line = list(width = 3))%>%
                layout(title = ~paste("Speed of Increment")) %>% 
                layout(xaxis = list(title = 'Number of Day')) %>%
                layout(yaxis=list(title=""))%>%
                plotly::config(displayModeBar = F) %>% layout(showlegend = FALSE)%>% 
                layout(autosize = F)
            
            fig31<-fig30 %>% layout(fig30, yaxis = list(type = "log"))
            fig31
        })
        
        
        #menuItem("Worldmap", tabName="wo", icon=icon("globe"))
        
        
        
    })  
    
}




# Run the application 
shinyApp(ui = ui, server = server)
