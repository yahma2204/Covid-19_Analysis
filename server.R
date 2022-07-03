shinyServer(function(input, output) {
  
  output$plot_overview <- renderPlotly({
    #prepare data
    covid_overview <- covid_clean %>% 
      select(location, variant, date, jumlah_kasus) %>%
      filter(between(date, input$date[1], input$date[2]),
             location == input$country,
             variant == input$variant) %>% 
      group_by(date) %>% 
      summarise(jumlah_kasus=sum(jumlah_kasus)) %>% 
      ungroup() %>% 
      mutate(label=glue("Date: {date}
                    Total Case: {jumlah_kasus}
                    "))
    #plot
    plot_overview <- ggplot(data = covid_overview, mapping = aes(x = date, 
                                                                 y = jumlah_kasus)) +
      geom_line(group=1, col="purple")+
      geom_point(aes(text = label), col="yellow")+
      labs(title = paste("Daily Total Case in", input$country),
           x = "Date",
           y = "Total Case")+
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill="#f08080"))# group=1 hanya akan buat 1 garis
    ggplotly(plot_overview, tooltip = "text")
  })
    
  
  output$map_data <- renderLeaflet({
      
      geo_map <- map_data %>% 
        mutate(rad = sqrt(jumlah_kasus/max(jumlah_kasus)) * 80) %>% 
        # Leaflet
        leaflet(options = leafletOptions(minZoom = 2, maxZoom = 6, )) %>% 
        # Base map layer
        # Lots of other options see https://rstudio.github.io/leaflet/basemaps.html
        addProviderTiles(providers$Esri.NatGeoWorldMap,
                         options = providerTileOptions(opacity = 0.8)) %>%
        addProviderTiles(providers$Stamen.TonerLabels,
                         options = providerTileOptions(opacity = 0.8)) %>% 
        addCircleMarkers(lng = ~longitude, 
                         lat = ~latitude, 
                         radius = ~rad, 
                         popup = ~label,
                         weight = 0.7,
                         stroke = T, 
                         color = "#DC143C",
                         fillColor = "#DC143C", 
                         fillOpacity = 0.5, 
                         group = ~location, 
                         labelOptions = labelOptions(noHide = F))
      geo_map
    })
    
  output$tabel_data <- renderDataTable({
      
      covid_clean
      
    })
    
   output$plot_5_location <- renderPlotly({
     covid_location <- covid_clean %>% 
       filter(between(date, input$date[1], input$date[2])) %>% 
       group_by(location) %>% 
       summarise(jumlah_kasus = sum(jumlah_kasus)) %>% 
       arrange(-jumlah_kasus) %>% 
       head(5) %>% 
       mutate(label=glue("Location: {location}
                    Total Case: {jumlah_kasus}
                    "))
     
      plot_location <- ggplot(data=covid_location, mapping = aes(
        x=jumlah_kasus/1000,
        y=reorder(location, jumlah_kasus),
        text = label))+
        geom_col(aes(fill=jumlah_kasus/1000))+
        scale_fill_gradient(low = "pink", high="navy")+
        labs(title = paste0("Top 5 Country\nbetween ",input$date[1]," to ",input$date[2]),
             x= "Total Case (Thousand)",
             y=NULL,
             fill="Total Case\n(Thousand)")+
        theme(plot.title = element_text(hjust = 0.5))
      ggplotly(plot_location, tooltip = "text")
    })
    
   output$plot_5_variant <- renderPlotly({
     covid_variant <- covid %>% 
       select(location, variant, num_sequences) %>% 
       filter(variant != "non_who",
              location==input$country) %>% 
       group_by(variant) %>% 
       summarise(jumlah_kasus = sum(num_sequences)) %>% 
       arrange(-jumlah_kasus) %>% 
       head(5) %>% 
       mutate(
         label=glue("Variant: {variant}
                    Total Case: {jumlah_kasus}
                    "))
     
      plot_variant <- ggplot(data=covid_variant, mapping = aes(x=jumlah_kasus/1000, y=reorder(variant, jumlah_kasus),
                                                               text = label))+
        geom_col(aes(fill=jumlah_kasus/1000))+
        scale_fill_gradient(low="pink", high="navy")+
        labs(title=paste0("Top 5 Variant in ",input$country),
             x = "Total Case (Thousand)",
             y = NULL,
             fill="Total Case\n(Thousand)")+
        theme(plot.title = element_text(hjust=0.5))
      ggplotly(plot_variant, tooltip = "text")
    })
   
   output$plot_5_top_country <- renderPlotly({
     covid_location <- covid_clean %>% 
       group_by(location) %>% 
       summarise(jumlah_kasus = sum(jumlah_kasus)) %>% 
       arrange(-jumlah_kasus) %>% 
       head(5) %>% 
       mutate(label=glue("Location: {location}
                    Total Case: {jumlah_kasus}
                    "))
     
     plot_location <- ggplot(data=covid_location, mapping = aes(
       x=jumlah_kasus/1000,
       y=reorder(location, jumlah_kasus),
       text = label))+
       geom_col(aes(fill=jumlah_kasus/1000))+
       scale_fill_gradient(low = "pink", high="navy")+
       labs(title = "Top 5 Country",
            x= "Total Case (Thousand)",
            y=NULL,
            fill="Total Case\n(Thousand)")+
       theme(plot.title = element_text(hjust = 0.5),
             legend.position = "none")+
       scale_y_discrete(labels = wrap_format(10))
     ggplotly(plot_location, tooltip = "text")
   })
   
   output$plot_5_top_variant <- renderPlotly({
     covid_variant <- covid %>% 
       select(location, variant, num_sequences) %>% 
       filter(variant != "non_who") %>% 
       group_by(variant) %>% 
       summarise(jumlah_kasus = sum(num_sequences)) %>% 
       arrange(-jumlah_kasus) %>% 
       head(5) %>% 
       mutate(
         label=glue("Variant: {variant}
                    Total Case: {jumlah_kasus}
                    "))
     
     plot_variant <- ggplot(data=covid_variant, mapping = aes(x=jumlah_kasus/1000, y=reorder(variant, jumlah_kasus),
                                                              text = label))+
       geom_col(aes(fill=jumlah_kasus/1000))+
       scale_fill_gradient(low="pink", high="navy")+
       labs(title="Top 5 Variant",
            x = "Total Case (Thousand)",
            y = NULL,
            fill="Total Case\n(Thousand)")+
       theme(plot.title = element_text(hjust=0.5),
             legend.position = "none")
     ggplotly(plot_variant, tooltip = "text")
   })

   
})
