shinyServer(
  function(input,output,session){
    
    mob_selected_date <- reactive({
      date.mob= us_mobility_grp %>% filter(.,date==input$selected.date)
    })
    
    
    output$map <- renderGvis({
      gvisGeoChart(mob_selected_date(), "Province.State", input$mob.type,
                   options=list(region="US", displayMode="regions", 
                                resolution="provinces",
                                #colorAxis= "{colors: ['#ec7063','#eb984e','#f4d03f','#48c9b0','#3498db','#af7ac5']}",
                                colorAxis= "{colors: ['#037DF3','#49F303']}",
                                backgroundColor= '#e5e8e8',
                                width="780px", height="420px"))
      # using width="auto" and height="auto" to
      # automatically adjust the map size
    })
    
    bottom5 <- reactive({
      
      bottom5mobs=mob_selected_date() %>% top_n(.,-5,get(input$mob.type))
      bottom5states=bottom5mobs$Province.State
      bottom5mobdata=us_mobility %>% filter(.,Province.State %in% bottom5states,
                                            date==input$selected.date) %>% 
                                     rename(.,bottom.selected.mob=input$mob.type)

    })
    
    output$bottom.boxplots <- renderPlot({

      ggplot(bottom5(),aes(x=fct_reorder(Province.State,bottom.selected.mob,FUN=median,
                                     na.rm=TRUE,.desc=TRUE),
                           y=bottom.selected.mob))+
        geom_boxplot()+
        ggtitle('Percentage of Change of Bottom 5 States')+
        xlab('')+
        ylab('')
      
    },height = 290)
    
    top5 <- reactive({
      
      top5mobs=mob_selected_date() %>% top_n(.,5,get(input$mob.type))
      top5states=top5mobs$Province.State
      top5mobdata=us_mobility %>% filter(.,Province.State %in% top5states,
                                            date==input$selected.date) %>% 
        rename(.,top.selected.mob=input$mob.type)
      
    })
    
    output$top.boxplots <- renderPlot({
      
      ggplot(top5(),aes(fct_reorder(Province.State,top.selected.mob,
                                    ,FUN=median,na.rm=TRUE,.desc=TRUE),y=top.selected.mob))+
        geom_boxplot()+
        ggtitle('Percentage of Change of Top 5 States')+
        xlab('')+
        ylab('')
      
    },height = 290)  
    
    output$global.corr <- renderPlot({
      ggcorr(corr.mob.type,method=c("pairwise", "pearson"))
    })
    
    

    selected_state <- reactive({
      if(input$selected!='US'){
        
        state.daily.cases= usa_covid %>% filter(.,Province.State==input$selected) %>% 
          group_by(.,Last.Update) %>% 
          summarise(.,Total.Cases=sum(Confirmed.Cases),
                    Total.Deaths=sum(Deaths)) %>%
          mutate(.,New.Cases=Total.Cases-lag(Total.Cases)) %>%
          filter(.,New.Cases>=0) %>% filter(.,!Last.Update %in% as.Date(c(
            '2020-04-22','2020-04-24','2020-04-29')))
        
      } else{
        
        state.daily.cases=usa_covid %>% group_by(.,Last.Update) %>% 
          summarise(.,Total.Cases=sum(Confirmed.Cases),
                    Total.Deaths=sum(Deaths)) %>%
          mutate(.,New.Cases=Total.Cases-lag(Total.Cases)) %>%
          filter(.,New.Cases>=0) %>% filter(.,!Last.Update %in% as.Date(c(
            '2020-04-22','2020-04-24','2020-04-29')))
      }
      


        # mutate(.,three_day_index=1:nrow(.)%/%3) %>% 
        # group_by(.,three_day_index) %>% 
        # summarise(.,Last.Update=max(Last.Update),New.Cases=mean(New.Cases))
    #   state.daily.cases= usa_covid %>% filter(.,Province.State==input$selected) %>% 
    #     group_by(.,Last.Update) %>% 
    #     summarise(.,Total.Cases=sum(Confirmed.Cases),
    #               Total.Deaths=sum(Deaths)) %>%
    #     mutate(.,New.Cases=Total.Cases-lag(Total.Cases),
    #            New.Deaths=Total.Deaths-lag(Total.Deaths)) %>%
    #     filter(.,New.Cases>=0,New.Deaths>=0) %>%
    #     mutate(.,three_day_index=1:nrow(.)%/%3) %>% 
    #     group_by(.,Last.Update,three_day_index) %>% 
    #     summarise(.,New.Cases=mean(New.Cases),New.Deaths=mean(New.Deaths)) %>% 
    #     #gather(.,Number.Type,Count,Total.Cases:Total.Deaths) %>% 
    #     gather(.,New.Type,New.Count,New.Cases:New.Deaths)
    })
    
    mob_selected_state <- reactive({
      if(input$selected!='US'){
        
        state.daily.mob= us_mobility %>% filter(.,Province.State==input$selected) %>% 
          group_by(.,date) %>% 
          summarise(.,Retail=median(Retail),
                    Grocery=median(Grocery),
                    Transit=median(Transit),
                    Park=median(Park),
                    Workplace=median(Workplace),
                    Residential=median(Residential))
      } else{
        state.daily.mob= us_mobility %>% group_by(.,date) %>% 
          summarise(.,Retail=median(Retail),
                    Grocery=median(Grocery),
                    Transit=median(Transit),
                    Park=median(Park),
                    Workplace=median(Workplace),
                    Residential=median(Residential))
      }
      
      
      #%>% 
        #gather(.,Mobility.Type,Percentage, Retail:Residential)
    })
    
    state.corr <-reactive({
      
      indx.mob=mob_selected_state()
      indx.mob$idx=seq(1,nrow(indx.mob))
      indx.cases=selected_state()
      indx.cases$idx=seq(2,(nrow(indx.cases)+1))
      state.corr.data=indx.cases %>% inner_join(indx.mob,by='idx')
      
      # selected_state()$idx=seq(-6,(nrow(selected_state())-7))
      # state.corr.data=selected_state() %>% inner_join(mob_selected_state(),by='idx')

      # state.corr.data=merge(selected_state() %>% rename(.,date=Last.Update)
      #                       , mob_selected_state(), by="date", all = T)
      
      state.corr.data=state.corr.data %>%
        filter(.,!is.na(New.Cases),
               !is.na(Retail),
               !is.na(Grocery),
               !is.na(Transit),
               !is.na(Park),
               !is.na(Workplace),
               !is.na(Residential))

    })
    
    output$retail.corr <- renderValueBox({
      corr=cor(state.corr()$New.Cases,state.corr()$Retail)
      corr=round(corr,2)
      if(corr>0)
        valueBox(corr,'Retail', icon = icon("chevron-circle-up"),color='green')
      else valueBox(corr,'Retail', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$grc.corr <- renderValueBox({
      corr=cor(state.corr()$New.Cases,state.corr()$Grocery)
      corr=round(corr,2)
      if(corr>0)
        valueBox(corr,'Grocery',  icon = icon("chevron-circle-up"),color='green')
      else valueBox(corr,'Grocery', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$trans.corr <- renderValueBox({
      corr=cor(state.corr()$New.Cases,state.corr()$Transit)
      corr=round(corr,2)
      if(corr>0)
        valueBox( corr,'Transit', icon = icon("chevron-circle-up"),color='green')
      else valueBox( corr,'Transit', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$park.corr <- renderValueBox({
      corr=cor(state.corr()$New.Cases,state.corr()$Park)
      corr=round(corr,2)
      if(corr>0)
        valueBox( corr,'Park', icon = icon("chevron-circle-up"),color='green')
      else valueBox( corr,'Park', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$wp.corr <- renderValueBox({
      corr=cor(state.corr()$New.Cases,state.corr()$Workplace)
      corr=round(corr,2)
      if(corr>0)
        valueBox( corr,'Workplace', icon = icon("chevron-circle-up"),color='green')
      else valueBox( corr,'Workplace', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$resid.corr <- renderValueBox({
      corr=cor(state.corr()$New.Cases,state.corr()$Residential)
      corr=round(corr,2)
      if(corr>0)
        valueBox( corr,'Residential', icon = icon("chevron-circle-up"),color='green')
      else valueBox( corr,'Residential', icon = icon("chevron-circle-down"),color='orange')
    })
    
    
    output$case.bar <- renderGvis({

        gvisLineChart(selected_state(),xvar='Last.Update',yvar='New.Cases',
                      options=list(chartArea= "{'width': '80%', 'height': '80%'}",
                                   legend="{position:'none'}",
                                   bar="{groupWidth:'100%'}",
                                   width="580px", height="360px",
                                   hAxis="{title:'Date'}"))

      # ggplot(data=selected_state(),aes(x=Last.Update,y=c(Total.Cases,Total.Deaths)))+
      #   geom_col()

    })
    
    # output$case.bar <- renderPlot({
    #   
    #   
    #   ggplot(data=selected_state(),aes(x=Last.Update,y=New.Cases))+
    #     geom_col(stat = "identity",fill='#3498db')+
    #     xlab('Date')
    #     
    #   
    # })
    
    
    # output$mob.trend <- renderGvis({
    # 
    # 
    #   gvisScatterChart(mob_selected_state(),xvar='date',yvar=c('Retail','Grocery','Transit',
    #                                                           'Park','Workplace','Residential'),
    #                   options=list(chartArea= "{'width': '80%', 'height': '80%'}",
    #                                #lineWidth="0",
    #                                legend="{position:'top'}",
    #                                bar="{groupWidth:'90%'}",
    #                                width="575px", height="375px",
    #                                hAxis="{title:'Date'}"))
    # 
    # })
    output$mob.trend <- renderPlot({
      
      mob_selected_state=mob_selected_state() %>% 
        gather(.,Mobility.Type,Percentage, Retail:Residential)
      
      ggplot(mob_selected_state,aes(date,Percentage,color=Mobility.Type))+
        geom_point(size=3)+
        geom_smooth(method = "loess")+
        xlab('Date')+
        theme_bw()+
        theme(legend.position = "top",
              panel.border = element_rect(colour = "white"),
              legend.title = element_blank(),
              legend.text = element_text(size=12) )+
        scale_color_manual(values=c('#ec7063','#eb984e','#f4d03f','#48c9b0','#3498db','#af7ac5'))
             # xvar='date',yvar=c('Retail','Grocery','Transit',
             #                                                'Park','Workplace','Residential'),
             #        options=list(chartArea= "{'width': '80%', 'height': '80%'}",
             #                     #lineWidth="0",
             #                     legend="{position:'top'}",
             #                     bar="{groupWidth:'90%'}",
             #                     width="575px", height="375px",
             #                     hAxis="{title:'Date'}"))
      
    })
    
    nyc_mob <- reactive({
      
      ny_mob_date=ny_mobility %>% filter(.,date==input$selected.date2)
      ny_mob_date=ny_mob_date %>% mutate(.,mob=get(input$mob.type2))
      #ny_mob_date$mob=as.numeric(ny_mob_date$mob)
    })
    
    output$nyc.cases <- renderGvis({
      
      gvisLineChart(nyc_covid,xvar='Last.Update',yvar='New.Cases',
                    options=list(chartArea= "{'width': '80%', 'height': '80%'}",
                                 vAxes="[{title:'Daily Cases'}]",
                                 legend="{position:'none'}",
                                 bar="{groupWidth:'100%'}",
                                 width="580px", height="360px",
                                 hAxis="{title:'Date'}"))
      
    })
    
    output$nyc.daily.mob <- renderPlot({
      ggplot(nyc_daily_mob_trend,aes(date,Percentage,color=Mobility.Type))+
        geom_point(size=3)+
        geom_smooth(method = "loess")+
        xlab('Date')+
        ylab('Daily Percentage Change')+
        theme_bw()+
        theme(legend.position = "top",
              panel.border = element_rect(colour = "white"),
              legend.title = element_blank(),
              legend.text = element_text(size=12) )+
        scale_color_manual(values=c('#ec7063','#eb984e','#f4d03f','#48c9b0','#3498db','#af7ac5'))
    })
    
    output$nyc.retail.corr <- renderValueBox({
      corr=cor(nyc.corr$New.Cases,nyc.corr$Retail)
      corr=round(corr,2)
      if(corr>0)
        valueBox(corr,'Retail', icon = icon("chevron-circle-up"),color='green')
      else valueBox(corr,'Retail', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$nyc.grc.corr <- renderValueBox({
      corr=cor(nyc.corr$New.Cases,nyc.corr$Grocery)
      corr=round(corr,2)
      if(corr>0)
        valueBox(corr,'Grocery',  icon = icon("chevron-circle-up"),color='green')
      else valueBox(corr,'Grocery', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$nyc.trans.corr <- renderValueBox({
      corr=cor(nyc.corr$New.Cases,nyc.corr$Transit)
      corr=round(corr,2)
      if(corr>0)
        valueBox( corr,'Transit', icon = icon("chevron-circle-up"),color='green')
      else valueBox( corr,'Transit', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$nyc.park.corr <- renderValueBox({
      corr=cor(nyc.corr$New.Cases,nyc.corr$Park)
      corr=round(corr,2)
      if(corr>0)
        valueBox( corr,'Park', icon = icon("chevron-circle-up"),color='green')
      else valueBox( corr,'Park', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$nyc.wp.corr <- renderValueBox({
      corr=cor(nyc.corr$New.Cases,nyc.corr$Workplace)
      corr=round(corr,2)
      if(corr>0)
        valueBox( corr,'Workplace', icon = icon("chevron-circle-up"),color='green')
      else valueBox( corr,'Workplace', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$nyc.resid.corr <- renderValueBox({
      corr=cor(nyc.corr$New.Cases,nyc.corr$Residential)
      corr=round(corr,2)
      if(corr>0)
        valueBox( corr,'Residential', icon = icon("chevron-circle-up"),color='green')
      else valueBox( corr,'Residential', icon = icon("chevron-circle-down"),color='orange')
    })
    
    output$nyc_map <- renderLeaflet({
      
      static.map <- leaflet() %>% 
        addTiles() %>% 
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lng = -73.99, lat = 40.7, zoom = 10)


      
      # leaflet_ny_mob_date <- leaflet(nyc_mob()) %>%
      #   addTiles() %>%
      #   # addPolygons(data=counties,
      #   #             fillColor = heat.colors(6, alpha = 1),
      #   #             stroke = FALSE) %>%
      #   addCircleMarkers(~long,~lat,
      #     radius = sqrt((0.5*nyc_mob()$mob)**2),
      #                    stroke = FALSE, fillOpacity = 0.7,
      #                    #clusterOptions = markerClusterOptions(),
      #                    popup = paste("<b>",nyc_mob()$county,"</b>","<br>",
      #                                  "Percentage: ", paste0(nyc_mob()$mob,'%'), "<br>")) %>%
      #   #addProviderTiles("Esri.WorldStreetMap") %>%
      #   addProviderTiles("OpenStreetMap.Mapnik") %>%
      #   setView(lng = -73.99, lat = 40.65, zoom = 9)
        #addLegend(position = "bottomleft",pal=pal,values = ny_mob_date$Retail)
        #color='blue' ,values = ny_mob_date$Retail,
        #           title = "Value",
        #           opacity = 1)
    })
    
    observeEvent(c(input$mob.type2,input$selected.date2),{
      
      proxy <- leafletProxy('nyc_map',data=nyc_mob()) %>%
        removeShape(layerId = LETTERS[1:6]) %>% 
        addCircleMarkers(~long,~lat,
                         radius = sqrt((0.5*nyc_mob()$mob)**2),
                         stroke = FALSE, fillOpacity = 0.7,
                         #clusterOptions = markerClusterOptions(),
                         popup = paste("<b>",nyc_mob()$county,"</b>","<br>",
                                       "Percentage: ", paste0(nyc_mob()$mob,'%'), "<br>"),
                         layerId = LETTERS[1:6])
        
        
      
    })
    
    
    
    # observeEvent(input$selected.date2,{
    #   
    #   proxy <- leafletProxy('nyc_map',data=nyc_mob())
    #   
    # })
    
      }
)