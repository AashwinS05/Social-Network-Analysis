# install.packages("shinythemes")
library(shiny)
library(networkD3)
library(sqldf)
library(visNetwork)
library(igraph)
library(sna)
library(shinythemes)
datapath <- setwd("C:\\Mydata\\dellstudio\\US_Stuff\\wrk-study\\Study\\Sem 4\\ABI R\\project 1\\shinyapp_4")

ui <- navbarPage(
  "AashwinSinghal_Social_Network_Analysis",
  inverse = TRUE, theme = shinytheme("cyborg"),
  tabPanel("Load data", fluid = TRUE,
           titlePanel("Choose file and view data"),
           sidebarLayout(
             sidebarPanel(
               
               fileInput(inputId = "Emp",
                         label = " Please Choose file: 'email-Eu-core.txt'",
                         buttonLabel = "Browse...", 
                         placeholder = "No file Selected"),
               fileInput(inputId = "Dept",
                         label = "Please Choose file: 'email-Eu-core-department-labels.txt'",
                         buttonLabel = "Browse...",
                         placeholder = "No file Selected"),
               sliderInput("Number", 
                           "Select N to display:",
                           min = 1, 
                           max = 30,
                           value = 10,
                           sep = '')
               ),
             mainPanel(
               tabsetPanel(
                 type = "tabs",
                 tabPanel("email-Eu-core",tableOutput("view_1")),
                 tabPanel("email-Eu-core-department-labels", tableOutput("view_2"))
                 )
               )
             )
           ),
  tabPanel("Simple Network",
           fluid = TRUE,
           titlePanel(" Network Model 'n' connections from 'email-Eu-Core.txt' data using VisNetwork"),
           sidebarLayout(
             sidebarPanel(
        # fileInput(inputId = "file3",
        #                      label = "Choose file:",
        #                      buttonLabel = "Browse...",
        #                      placeholder = "No file Selected"),
               numericInput("obs_1",
                            "Choose n to display connections:",
                            10)),
        mainPanel(visNetworkOutput("simple"))
        )),
  tabPanel("Emails Summary",
           fluid = TRUE,
           titlePanel("visualization of Email transactions"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("Number_1",
                           "Select No. of Rows to be used - plot:",
                           min = 1000,
                           max = 25571,
                           value = 2500,
                           sep = ''),
               numericInput("obs_2",
                            "Choose Top N Nodes to display - Table:",
                            10)),
             mainPanel(
               tabsetPanel(type = "tabs",
                           tabPanel("Top Employees - Sent",
                                    tabsetPanel(type = "tabs",
                                                tabPanel("table_sent", tableOutput("table_sent")),
                                                tabPanel("Plot_sent", forceNetworkOutput("Plot_sent")))),
                           tabPanel("Top Employees - Received",
                                    tabsetPanel(type ="tabs",
                                                tabPanel("table_Recvd", tableOutput("table_Recvd")),
                                                tabPanel("Plot_Recvd", forceNetworkOutput("Plot_Recvd")))))))
           ),
  tabPanel("Centrality",
           fluid = TRUE,
           titlePanel("Indicators of centrality - Degree & Betweenness"),
           tabsetPanel(
             type = "tabs",
             tabPanel("Degree",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("Number_2",
                                      "Select No. of Rows to be used - plot:",
                                      min = 1000,
                                      max = 25571,
                                      value = 2500,
                                      sep = '')),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Total - Degree", visNetworkOutput("Total - Degree")),
                                      tabPanel("In - Degree", visNetworkOutput("In - Degree")))))),
             tabPanel("Betweenness",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("Number_3",
                                      "Select No. of Rows to be used - plot:",
                                      min = 1000,
                                      max = 25571,
                                      value = 2500,
                                      sep = '')),
                        mainPanel(visNetworkOutput("Betweenness"))))
             )
           ),
  tabPanel("Department Level Analysis",
           fluid = TRUE,
           titlePanel("Visualization Of Department Level Email transactions"),
           tabsetPanel(type = "tabs",
                       tabPanel("Tabular Representation",
                                sidebarLayout(
                                  sidebarPanel(
                                    numericInput("Number_4",
                                                 "Choose top rows to display - Table:",
                                                 10)),
                                  mainPanel(tableOutput("Tabular Representation")))
                                ),
                       tabPanel("Plot_Dept",
                                sidebarLayout(
                                  sidebarPanel(
                                    sliderInput("Number_5",
                                                "Select No. of Rows to be used - plot:",
                                                min = 1000,
                                                max = 25571,
                                                value = 2500,
                                                sep = '')),
                                  mainPanel(visNetworkOutput("Plot_Dept"))))
                       )
           )
  )


