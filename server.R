## server.R ##
shinyServer(function(input, output, session){
  
  # making cleandf reactive
  values <- reactiveValues(cleandf = cleandf, subdf = subdf)
  
  # Getting Keywords
  output$gwords = renderPrint({ input$ggroup })
  output$bwords = renderPrint({ input$bgroup })
  output$bgcount = renderPrint({ values$cleandf %>% group_by(gorb) %>% count() })
  
  # If user clicks action button, dataframe gets updated columns
  observeEvent(input$keyupdate, {
    
    combined_gwords = c(gwords1, input$ggroup)
    combined_bwords = c(bwords1, input$bgroup)
    
    values$cleandf = update_cleandf_new_keywords(combined_gwords, combined_bwords, rawdf)
    values$subdf = update_subdf(values$cleandf)

  })
  
  # reactive setup for top20 graph
  top10f = reactive({
    
    values$cleandf %>% 
                  filter(gorb == switch(input$gfocus, "girl" = "girl", "boy" = "boy", "neutral" = "neutral", "all" = c("girl", "boy", "neutral"))) %>% 
                  group_by_(input$catg_or_manuf) %>% summarise(count=n(),
                  avg_price = mean(price),
                  value = sum(price),
                  popularity = mean(Reviews)*mean(Avg_Rating),
                  gender_ratio = (sum(girly==T)-sum(boyish==T))/count) %>% arrange(desc(get(input$pfocus))) %>% head(20)
  })
  
  # output top20 graph
  output$top20graph = renderPlot(
      top10f() %>% ggplot(aes(x = reorder(get(input$catg_or_manuf), -get(input$pfocus)))) 
      + geom_col(aes(y=get(input$pfocus), fill = gender_ratio), color = "black") 
      + theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))
      + scale_fill_gradientn(limits = c(-1,1), colors = c("blue", "white", "red"))
      + labs(title = "Top 20 Infochart", x = " ", y = " "),
       height = 700
  )
  
  # output boy girl boxplot
  output$bgbox = renderPlot(
    ggplot(values$cleandf, aes(x=gorb, y=price))
    + geom_boxplot(aes(fill = gorb))
    + scale_fill_manual(values=c("lightblue", "pink", "white"))
    + coord_cartesian(ylim = c(0, 75))
    + labs(title = "Prices Boy Vs Girl Toys", x = " ", y = " "),
    height = 500
  )
  
  # output statistics using infoBox
  output$boyBox <- renderInfoBox({
    counts = values$cleandf %>% filter(gorb=="boy") %>% count()
    avgprice = values$cleandf %>% filter(gorb=="boy") %>% summarise(mean(price))
    infoBox("Boy Stats", counts, format(round(avgprice,2), nsmall = 2), icon = icon("male"))
  })
  output$girlBox <- renderInfoBox({
    counts = values$cleandf %>% filter(gorb=="girl") %>% count()
    avgprice = values$cleandf %>% filter(gorb=="girl") %>% summarise(mean(price))
    infoBox("Girl Stats", counts, format(round(avgprice,2), nsmall = 2), icon = icon("female"))
  })
  output$compareBox <- renderInfoBox({
    sameVAR = gb_stat_test(values$cleandf, "Fvar")
    H0vsHA = !gb_stat_test(values$cleandf, "Ttest")
    infoBox(title = paste("Are VAR same? - ", sameVAR), subtitle=paste("Are G>B Prices? - ", H0vsHA), icon = icon("arrows-alt-h"))
  })

  # output cat table datatable 
  output$cattable = DT::renderDataTable({
  datatable(values$subdf, rownames=FALSE) })
  
  # # output cat bubble chart - gvis 
  # output$catbubble = renderGvis({
  #   gvisScatterChart(values$subdf %>% select(avg_b_price, avg_g_price)) })
  #                    # colorvar="V", sizevar="Profit",
  #                    # options=list(hAxis='{minValue:75, maxValue:125}'))
    
  
  # output table datatable
  output$table = DT::renderDataTable({
  datatable(values$cleandf, rownames=FALSE)})
  
  
})