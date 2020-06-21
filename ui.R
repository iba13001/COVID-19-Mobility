

shinyUI(
  dashboardPage(skin='purple',
    dashboardHeader(title='Effect of COVID-19 on Mobility',titleWidth = 400),
    dashboardSidebar(
      # sidebarUserPanel(NULL,image='https://media.kasperskydaily.com/wp-content/uploads/sites/92/2020/02/06101938/coronavirus-phishing-featured.jpg'),
      sidebarUserPanel(NULL,image='coco.jpg'),
      sidebarMenu(
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
        tabItem(tabName = 'us_mobility',
                

                
                column(8,
                       # box(width = NULL,selectizeInput('mob.type',
                       #                                'Select Mobility Type',
                       #                                mob.choice)),
                         box(width=NULL,sliderInput("selected.date",animate=T,
                                                 "Dates:",
                                                 min = as.Date("2020-03-01","%Y-%m-%d"),
                                                 max = as.Date("2020-06-01","%Y-%m-%d"),
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
                                                  max = as.Date("2020-06-01","%Y-%m-%d"),
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

