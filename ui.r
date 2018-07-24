library(shiny)
library(shinydashboard)
# library(shinythemes)
library(readxl)
library(car)
library(dplyr)
library(readr)


c=read_excel("FIPSandRegion.xls")%>%
  rename(regionnum=PMRegion)%>%
  mutate(countyfips=as.numeric(Fips))%>%
  filter(countyfips!=1 , countyfips!=0 , countyfips!=5 ,countyfips!=13 ,countyfips!=14 ,countyfips!=31 ,countyfips!=35 ,countyfips!=59)

r=read_csv("totalJobsReg_v14.csv")%>%
  filter(year==2014)%>%
  select(regionnumber)

dashboardPage(skin="black",
  dashboardHeader(title= "SDO Forecast Review"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("Rnum","Region Number:",
                  choices=r$regionnumber),
      selectInput("county","County:",
                  choices=unique(c$Name)),
      menuItem("Forecast Methodology (PDF)", icon = icon("file-code-o"), 
         href = "http://www.colorado.gov/cs/Satellite?blobcol=urldata&blobheadername1=Content-Disposition&blobheadername2=Content-Type&blobheadervalue1=inline%3B+filename%3D%22Forecasts+Methodolofy.pdf%22&blobheadervalue2=application%2Fpdf&blobkey=id&blobtable=MungoBlobs&blobwhere=1251731969473&ssbinary=true")
    )
  ),
  dashboardBody(
              fluidRow(box(title="Total Job Forecast - Region",
                           plotOutput("Rplot")),
                       box(title="Total Population Forecast- Region",
                           plotOutput("RplotPop"))),
              
              fluidRow(box(title="Total Job Forecast - County",
                           plotOutput("plot")),
                       box(title="Total Population Forecast - County",
                           plotOutput("plotPop"))),
   
#              fluidRow(box(title="Total Job and LF Forecast - Region",
#                           plotOutput("plotLFReg")),
#                       box(title="Total Job and LF Forecast - County",
#                           plotOutput("plotLF"))),
            
#              fluidRow(box(title="Growth Rate Chart - Region",
#                           plotOutput("RplotG")),
#                       box(title="Growth Rate Chart - County",
#                           plotOutput("plotG")))
              
#              fluidRow(tabBox(title = "Growth Rate Charts",
#                              tabPanel("County", plotOutput("plotG")), 
#                              tabPanel("Region", plotOutput("RplotG"))),
#                       box(title="Total Job and LF Forecast  - County",
#                              plotOutput("plotLF")))
            fluidRow(box(title = "County Growth Rate Chart",
                          plotOutput("plotG")),
                     box(title="Total Job and LF Forecast  - Region",
                          plotOutput("plotLFReg")))

#              fluidRow(tabBox(title = "Growth Rate Charts",
#                              tabPanel("County", plotOutput("plotG")),
#                              tabPanel("Region", plotOutput("RplotG"))),
#                      tabBox(title="Total Job and LF Forecast",
#                             tabPanel("County", plotOutput("plotLF")),
#                             tabPanel("Region", plotOutput("plotLFReg"))))
                      


      )
)

