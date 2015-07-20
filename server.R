

# Engineering CLA data boxplots
#server logic


source("set_up.R") # load libraries and set up dataframes for each discipline


shinyServer(function(input, output) {
  
  output$claPlot <- renderPlot({
    
    if(input$discipline == 1){
      df = mech
      graph_title = "Mechanical Engineering CLA Scores"
    }
    else if(input$discipline == 2){
      df = elec
      graph_title = "Electrical Engineering CLA Scores"
    }
    else if(input$discipline == 3){
      df = cmpe
      graph_title = "Computer Engineering CLA Scores"
    } 
    else if(input$discipline == 4){
      df = civl
      graph_title = "Civil Engineering CLA Scores"
    }    
    else if(input$discipline == 5){
      df = chem
      graph_title = "Chemical Engineering CLA Scores"
    } 
    else if(input$discipline == 6){
      df = ench
      graph_title = "Engineering Chemistry CLA Scores"
    } 
    else if(input$discipline == 7){
      df = mine
      graph_title = "Mining Engineering CLA Scores"
    } 
    else if(input$discipline == 8){
      df = geoe
      graph_title = "Geological Engineering CLA Scores"
    } 
    else if(input$discipline == 9){
      df = enph
      graph_title = "Engineering Physics CLA Scores"
    } 
    else if (input$discipline == 10){
      df = mthe
      graph_title = "Math and Engineering CLA Scores"
    }
    else{
      df = cla
      graph_title = "Engineering CLA Scores"
    }
    
    
    #calculate sample sizes:
    n_1 <-  sum(with(df, year ==1 & score_total > 1), na.rm = TRUE)     
    year1 <- paste0("First Year\nn = ", n_1) #text string for xlabel including sample size
 
    n_2 <-  sum(with(df, year ==2 & score_total > 1), na.rm = TRUE)     
    year2 <- paste0("Second Year\nn = ", n_2) #text string for xlabel
    
    n_3 <-  sum(with(df, year ==3 & score_total > 1), na.rm = TRUE)     
    year3 <- paste0("Third Year\nn = ", n_3) #text string for xlabel
    
    n_4 <-  sum(with(df, year ==4 & score_total > 1), na.rm = TRUE)     
    year4 <- paste0("Fourth Year\nn = ", n_4) #text string for xlabel
    
    
## plot description
    ggplot(
      data = df, 
      aes(x = as.factor(year), y = score_total)
      ) +
      stat_boxplot(geom = 'errorbar', stat_params = list(width = 0.3)) + # add perpendicular lines to whiskers
      geom_boxplot() + # geom_boxplot must be after stat_boxplot    
      geom_hline(
        yintercept = c(963, 1097, 1232, 1367),  #boundaries for CLA mastery levels 
        colour = "red", 
        linetype = 'dashed'
        ) +
      coord_cartesian(xlim = c(0.5,5.7),ylim = c(600, 1600)) +
      scale_x_discrete(labels = c(year1, year2, year3, year4)) + #text strings from above with sample sizes
      theme(
        panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
        panel.grid.major.y = element_line("grey"), #change horizonatal line colour (from white)
        panel.background = element_rect("white"), #change background colour
        panel.grid.major.x = element_blank() #remove vertical white lines
        ) +
      labs(title = graph_title, x = "Year", y = "Total CLA+ Score") +
      annotate( # add labels for CLA mastery levels
        "text", 
        fontface = "bold", 
        size = 6,
        alpha = 0.5,
        x = 5, y = c(900, 1030, 1164, 1300, 1430), 
        label = c("Below Basic", "Basic", "Proficient", "Accomplished", "Advanced"), 
        colour = "red"
        ) # end ggplot description
    
    
  }#end plot expression
  ) #end render plot
 
}#end function
) #end shiny server

