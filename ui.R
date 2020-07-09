

shinyUI(
  dashboardPage(skin='purple',
    dashboardHeader(title='Effect of COVID-19 on Mobility',titleWidth = 400),
    dashboardSidebar(
      # sidebarUserPanel(NULL,image='https://media.kasperskydaily.com/wp-content/uploads/sites/92/2020/02/06101938/coronavirus-phishing-featured.jpg'),
      sidebarUserPanel(NULL,image='coco.jpg'),
      sidebarMenu(
        menuItem('Need to Know!',tabName='info',icon=icon('info-circle')),
        menuItem('Nation Wide Mobility',icon=icon('map-marked'),
                 menuSubItem(tabName = 'us_mobility',selectizeInput('mob.type',
                                                                    'Select Mobility Type',
                                                                    mob.choice)),
                 menuSubItem('Mobility Correlations',tabName= 'corr_us')
                 ),
        menuItem('State Trends',icon=icon('chart-bar'),
                 menuSubItem(tabName = 'state_trends',selectizeInput('selected',
                                            'Select Region to Display',
                                            state.choice))
                 ),
        menuItem('NYC',icon=icon('grin-hearts'),
                 menuSubItem('City Trends',icon=icon('chart-bar'),tabName = 'nyc_trends'),
                 menuSubItem('Track Mobility',icon=icon('chart-line'),tabName = 'nyc_track_mobility')
                 )
                 
        #menuItem("Data", tabName = "data", icon = icon("database"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName='info',
                column(6,
                       box(title = "Note!", background = "maroon",
                           "If charts do not show properly, reload the app, go to 'NYC' and click on 'City Trends'.
                           Then view any chart of interest",
                           width=12),
                       box(width=12,title = "Mobility Types",
                           HTML(paste('<b>Grocery and Pharmacy:</b> mobility trends collected from
                         grocery stores, food warehouses, farmers markets, specialty 
                         food shops, drug stores, and pharmacies.','',
                                      '<b>Parks:</b> mobility trends collected from recreational places such
                         as local parks, national parks, public beaches, marinas,
                         dog parks, plazas, and public gardens.','',
                                      '<b>Transit Stations:</b> mobility trends collected from
                         public transport hubs like subway,
                         bus, and train stations.','',
                                      '<b>Retail:</b> mobility trends collected from restaurants, 
                         cafes, shopping centers, theme parks, museums,
                         libraries, and movie theaters.','',
                                      '<b>Residential:</b> mobility trends collected from places of residence.','',
                                      '<b>Workplace:</b> mobility trends from places of work'
                                      ,sep="<br/>"))
                       ),
                       box(width=12,title='Baseline Definition',
                           HTML('The baseline used as a reference for this data
                         is the median value of the corresponding day
                         of the week in the 5-week period from January
                         3 to February 6 of 2020')
                           )
                       ),
                column(6,
                       box(width=12,title='Percentage Change in Mobility ',
                           HTML('The percentage change in the visits and length
                         of stay with respect to the baseline')),
                       box(width=12,title='Correlation Coefficients between Daily Cases and Mobiloty',
                           HTML("It is the number that describes how people reacted 
                          to the reported daily cases in the previous days.
                          It takes values between -1 and 1. A negative value 
                          indicates that as the reported daily cases increased,
                           people's mobility decreased in the following day. 
                          A positive value indicates that as the reported 
                          daily cases increased, people's mobility 
                          increased in the following day. Zero indicates
                           that there is a weak response, and 
                          one indicates that there is a strong response.")
                       ),
                       box(width=12,title='Data Sources',
                           tagList(a('Google LLC "Google COVID-19 
                              Community Mobility Reports"',
                                     href="https://www.google.com/covid19/mobility/")),
                           HTML("<br/>"),
                           tagList(a("Johns Hopkins Daily Reports",
                                     href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data")),
                           HTML("<br/>"),
                           tagList(a("New York Times COVID-19 Reports",
                                     href="https://github.com/nytimes/covid-19-data")),
                           HTML("<br/>"),
                           tagList(a("Geocodes from Healthcare.gov",
                                     href="https://data.healthcare.gov/dataset/Geocodes-USA-with-Counties/52wv-g36k"))
                           ),
                       )
                    
                ),
        tabItem(tabName = 'us_mobility',
                

                
                column(8,
                       # box(width = NULL,selectizeInput('mob.type',
                       #                                'Select Mobility Type',
                       #                                mob.choice)),
                         box(width=NULL,sliderInput("selected.date",animate=T,
                                                 "Dates:",
                                                 min = as.Date("2020-03-01","%Y-%m-%d"),
                                                 max = as.Date("2020-07-05","%Y-%m-%d"),
                                                 value=as.Date("2020-04-15"),
                                                 timeFormat="%Y-%m-%d")),
                       box(htmlOutput('map'),width=NULL,height=470,
                             title='Percentage of Change in Mobility' )
                         
                         ),
                column(4,
                       box(plotOutput('top.boxplots'),width = NULL,height=305),
                       box(plotOutput('bottom.boxplots'),width = NULL,height=305)
                       )
                ),
        
        tabItem(tabName = 'corr_us',
                fluidRow(box(title='Pairwise Correlation between the Change in Mobility Types in The US',
                                             plotOutput('global.corr')))
                ),
        
        tabItem(tabName = 'state_trends',
                
                
                # fluidRow(column(6,
                #        box(background = "purple",selectizeInput('selected',
                #                           'Select State to Display',
                #                           state.choice))
                # )),
                
                column(6,box(title='Daily Cases',background = "light-blue", solidHeader = TRUE,
                    htmlOutput("case.bar"),width=80,collapsible = T)),
                column(6,box(title='Percentage of Change in Mobility',background = "light-blue", solidHeader = TRUE,
                    plotOutput("mob.trend",height="360px"),width=80,collapsible = T)),
                
                
                column(12,box(width=160, title='Correlations between Daily Cases and Daily Change in Mobility by Type',
                              background = 'navy',
                              valueBoxOutput('retail.corr',width = 2),
                              valueBoxOutput('grc.corr',width = 2),
                              valueBoxOutput('trans.corr',width = 2),
                              valueBoxOutput('park.corr',width = 2),
                              valueBoxOutput('wp.corr',width = 2),
                              valueBoxOutput('resid.corr',width = 2)
                             )
                         )
                # fluidRow(
                #   infoBoxOutput('retail.corr'),
                #   infoBoxOutput('grc.corr'),
                #   infoBoxOutput('trans.corr')
                # ),
                # 
                # fluidRow(
                #   infoBoxOutput('park.corr'),
                #   infoBoxOutput('wp.corr'),
                #   infoBoxOutput('resid.corr')
                # )
                
                # fluidRow(
                #   box(title='Total Cases and Deaths',background = "light-blue", solidHeader = TRUE,
                #     htmlOutput("case.bar"),width=8,collapsible = T)
                # ),
                # fluidRow(
                #   box(title='Mobility Percentage',background = "light-blue", solidHeader = TRUE,
                #       htmlOutput("mob.trend"),width=8,collapsible = T)
                # )
                
                #fluidRow(
                 # box(htmlOutput('hist'), height=300))
                
                
                
        ),
        tabItem(tabName = 'nyc_track_mobility',
                
                column(12,
                       # box(width = NULL,selectizeInput('mob.type',
                       #                                'Select Mobility Type',
                       #                                mob.choice)),
                       box(width=4,selectizeInput('mob.type2',
                                                     'Select Mobility Type',
                                                     mob.choice)),
                       box(width=8,sliderInput("selected.date2",animate=T,
                                                  "Dates:",
                                                  min = as.Date("2020-03-01","%Y-%m-%d"),
                                                  max = as.Date("2020-07-05","%Y-%m-%d"),
                                                  value=as.Date("2020-04-15"),
                                                  timeFormat="%Y-%m-%d")),
                       fluidRow(leafletOutput('nyc_map'))
                       ),
                       # box(htmlOutput('map'),width=NULL,height=525,
                       #     title='Percentage of Change in Mobility' )
                ),
        tabItem(tabName = 'nyc_trends',
                
                column(6,box(title='Daily Cases',background = "light-blue", solidHeader = TRUE,
                             htmlOutput("nyc_cases"),width=80,collapsible = T)),
                column(6,box(title='Percentage of Change in Mobility',background = "light-blue", solidHeader = TRUE,
                             plotOutput("nyc.daily.mob",height="360px"),width=80,collapsible = T)),
                
                
                column(12,box(width=160, title='Correlations between Daily Cases and Daily Change in Mobility by Type',
                              background = 'navy',
                              valueBoxOutput('nyc.retail.corr',width = 2),
                              valueBoxOutput('nyc.grc.corr',width = 2),
                              valueBoxOutput('nyc.trans.corr',width = 2),
                              valueBoxOutput('nyc.park.corr',width = 2),
                              valueBoxOutput('nyc.wp.corr',width = 2),
                              valueBoxOutput('nyc.resid.corr',width = 2)
                )
                )

                )
      )
    )
    
  )
)

