#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(scipen = 200)
source("global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    output$roster = DT::renderDataTable(
                                    datatable(df[, c('short_name',
                                               'age',
                                               'nationality',
                                               'league_name',
                                               'club_name',
                                               'year')] %>%
                                            filter(
                                                df$nationality == input$country,
                                                df$league_name == input$league,
                                                df$club_name == input$clubname,
                                                df$year == input$year_panel1
                                                )) %>% 
                                            formatStyle(columns = c('short_name',
                                                                    'age',
                                                                    'nationality',
                                                                    'league_name',
                                                                    'club_name',
                                                                    'year'),
                                                        color = 'grey50',
                                                        backgroundColor = 'orange',
                                                        fontWeight = 'bold'
                                            ),
                                        width = "20%",
                                        server = FALSE,
                                        selection = 'single')
    
    output$roster_title = renderText({
        "Roster of the Selected Team"
    })
    
    
    output$info = renderText({
        "Click a row, and then click the search button again for further information of the player!"
    })
    
    # Basic Info
    
    #valuebox
    output$name <- renderValueBox({
        d = as.character(df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),3])
        valueBox(value = d,
                 subtitle = "Name",
                 icon = icon("user",lib='font-awesome', "fa-2x"),
                 color = "orange")
    })
    
    output$DB <- renderValueBox({ 
        d <- as.character(df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),5])
        valueBox(value = d,
                 subtitle = "Date of Birth",
                 icon = icon("birthday-cake",lib='font-awesome', "fa-2x"),
                 color = "orange")
    })
    
    output$Position <- renderValueBox({ 
        d <- as.character(df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),16])
        valueBox(value = d,
                 subtitle = "Position",
                 icon = icon("crosshairs",lib='font-awesome', "fa-2x"),
                 color = "orange")
    })
    
    output$Club <- renderValueBox({ 
        d <- as.character(df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),9])
        valueBox(value = d,
                 subtitle = "Club",
                 icon = icon("users",lib='font-awesome', "fa-2x"),
                 color = "orange")
    })
    
    output$Value <- renderValueBox({ 
        d <- as.character(df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),14])
        valueBox(value = d,
                 subtitle = "Value",
                 icon = icon("coins",lib='font-awesome', "fa-2x"),
                 color = "orange")
    })
    
    output$Wage <- renderValueBox({ 
        d <- as.character(df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),15])
        valueBox(value = d,
                 subtitle = "Wage",
                 icon = icon("alipay",lib='font-awesome', "fa-2x"),
                 color = "orange")
    })
    
    output$PF <- renderValueBox({ 
        d <- as.character(df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),17])
        valueBox(value = d,
                 subtitle = "Perferred Foot",
                 icon = icon("shoe-prints",lib='font-awesome', "fa-2x"),
                 color = "orange")
    })
    
    output$NT <- renderValueBox({ 
        d <- as.character(df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),8])
        valueBox(value = d,
                 subtitle = "National Team",
                 icon = icon("globe-asia",lib='font-awesome', "fa-2x"),
                 color = "orange")
    })
    
    # Ratingplot
    output$Rating_plot <- renderPlot({
        validate(
            need(input$roster_rows_selected != "", "Please select a player by clicking the row in the table, and then click the orange 'Search' button again!")
        )
        player = df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),]
        if (player$player_positions == 'GK') {
            ability = player[c(12, 29, 30, 31, 32, 33, 34)]
        } else {
            ability = player[c(12, 23, 24, 25, 26, 27, 28)]
        }
        
        x = c(1, 
              0.33, 1, 1.68, 
              0.33, 1, 1.68)
        
        y = c(1.5,
              1, 1, 1,
              0.3, 0.3, 0.3)
        
        
        index_y = c(1.65,
                    0.75, 0.75, 0.75,
                    0.05, 0.05, 0.05)
        
        index = colnames(ability)
        
        Rating_data = data.frame(x_position = x, y_position = y, index_y_position = index_y,
                                 ability_cha = as.character(ability), index_cha = index)
        img <- readPNG("Ratingboard.png")
        Rating_data %>% 
            ggplot() +
            background_image(img) +
            coord_fixed(ratio = 0.8) +
            geom_text(aes(x_position, y_position), label = Rating_data$ability_cha, col = 'white') +
            geom_text(aes(x_position, index_y_position), label = Rating_data$index_cha, col = 'darkorange') +
            xlim(0,2) +
            ylim(0,2) +
            theme(axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_blank(),
                  panel.grid.minor = element_blank(), 
                  panel.grid.major = element_blank(),
                  panel.background = element_blank(),
                  plot.background = element_blank(),
                  legend.position = "none"
            ) +
            xlab(NULL) +
            ylab(NULL)
    }, res = 96,
    bg = "transparent")
    
    output$Real_Overall_plot <- renderPlot({
        validate(
            need(input$roster_rows_selected != "", "Please select a player by clicking the row in the table, and then click the orange 'Search' button again!")
        )
        player = df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),]
        overall_rating = player[, 69:95]
        x = c(5, 
              1.75, 5, 8.2,
              0.7, 2.85, 5, 7.1, 9.2,
              1.75, 5, 8.2,
              0.7, 2.85, 5, 7.1, 9.2,
              0.7, 2.85, 5, 7.1, 9.2,
              0.7, 2.85, 5, 7.1, 9.2)
        y = c(0,
              16.6, 16.6, 16.6,
              14.2, 14.2, 14.2, 14.2, 14.2,
              11.7, 11.7, 11.7,
              8.5, 8.5, 8.5, 8.5, 8.5,
              5.5, 5.5, 5.5, 5.5, 5.5,
              2.5, 2.5, 2.5, 2.5, 2.5)
        overall_rating_data = data.frame(x_position = x, y_position = y, overall_rating_cha = as.character(overall_rating) )
        img <- readJPEG("soccer_field2.png")
        overall_rating_data %>%
            ggplot(aes(x_position, y_position)) +
            background_image(img) +
            geom_text(label = overall_rating_data$overall_rating_cha, col = 'white') +
            xlim(0,10) +
            ylim(0,18) +
            coord_fixed(ratio = 0.8) +
            theme(axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_blank(),
                  panel.grid.minor = element_blank(), 
                  panel.grid.major = element_blank(),
                  panel.background = element_blank(),
                  plot.background = element_blank(),
                  legend.position = "none"
            ) +
            xlab(NULL) +
            ylab(NULL)
    }, res = 96,
    bg = "transparent")
    
    output$Tags_plot <- renderPlot({
        validate(
            need(input$roster_rows_selected != "", "Please select a player by clicking the row in the table, and then click the orange 'Search' button again!")
        )
        player = df[which((df$short_name %in% input$roster_cell_clicked) & (df$year == input$year_panel1)),]
        img <- readPNG("Tagboard.png")
        Tags = player$player_tags
        
        if(is.na(Tags) == T) {
            
            ggplot() +
                background_image(img) +
                coord_fixed(ratio = 0.75)
            
        } else {
            
            Tags_split = strsplit(Tags, split = ',')[[1]]
            Tags_split = gsub('#', '', Tags_split)
            n = length(Tags_split)
            
            if (n>6) {
                n = 6
            }
            
            x = c(0.31, 1, 1.67,
                  0.31, 1, 1.67)
            y = c(3.75, 3.75, 3.75,
                  1.1, 1.1, 1.1)
            Tags_data = data.frame(x_position = x[1:n], y_position = y[1:n], Tags_split_cha = Tags_split[1:n])
            
            Tags_data %>% 
                ggplot() +
                background_image(img) +
                coord_fixed(ratio = 0.3) +
                geom_text(aes(x_position, y_position), label = Tags_data$Tags_split_cha, color = "white") +
                ylim(0,5) +
                xlim(0,2) +
                theme(axis.ticks.y = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.text.x = element_blank(),
                      panel.grid.minor = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.background = element_blank(),
                      plot.background = element_blank(),
                      legend.position = "none"
                ) +
                xlab(NULL) +
                ylab(NULL)
        }
        }, res = 96, bg = "transparent")
    
    ## Map plot
    output$Mapplot_title = renderText({
        "Geographic distribution"
    })
    
    output$Mapplot <- renderPlot({
        validate(
            need(input$position != "", "Please select a position to see the geographic distribution of top 10 players.")
        )
        df1 = df %>%
            select(year, short_name, nationality)
        df1$pos_value = df[,input$position]
        df1 = df1 %>%
            filter(year == input$year) %>%
            left_join(coor,by=c("nationality"="country")) %>%
            filter(!is.na(latitude), !is.na(longitude))
        df1=df1 %>%
            arrange(desc(pos_value))
        df1 = df1[1:10,]
        mp <- NULL
        mapworld <- borders("world", colour = "gray50", fill = NA)
        mp <- ggplot() + mapworld + ylim(-60,90) + xlab(NULL) + ylab(NULL)
        mp2 <- mp + geom_point(aes(x=df1$longitude,y=df1$latitude),color="darkorange") + scale_size(range=c(1.5,1.5))
        mp3 <- mp2 + geom_text_repel(aes(x=df1$longitude,y=df1$latitude,label = df1$short_name), box.padding = 0.5,max.overlaps = Inf, col='white',segment.size = 0.2)
        mp4 <- mp3 + theme(
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.title.x = element_text(colour = "white"),
            axis.title.y = element_text(colour = "white"),
            axis.text.x = element_text(colour = "white"),
            axis.text.y = element_text(colour = "white"),
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            legend.position = "none"
        )
        mp4
    }, res = 96, bg="transparent")
    
    # Scatter plot of wage and rating
    
    output$Scatterplot_title = renderText({
        "Relationship between Wage and Rating"
    })
    
    output$Scatterplot <- renderPlot({
        validate(
            need(input$position != "", "Please select a position to see relationship between Wage and Rating.")
        )
        df2 = df %>%
            filter(year == input$year, wage_eur!=0)
        ggplot(df2) +
            geom_point(aes(x = wage_eur, y = overall), color = "darkorange", alpha = 0.3, size = 0.8) +
            xlab("Wage") +
            ylab("Rating")+
            theme_bw() +
            theme(
                axis.line.x = element_blank(),
                axis.line.y = element_blank(),
                axis.title.x = element_text(colour = "white"),
                axis.title.y = element_text(colour = "white"),
                axis.text.x = element_text(colour = "white"),
                axis.text.y = element_text(colour = "white"),
                panel.grid.major = element_line(colour="grey50"),
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(),
                plot.background = element_blank(),
                legend.position = "none"
            )
    }, res = 96, bg="transparent")  

    # Line plot of wage and value
    
    output$Lineplot_title = renderText({
        "Highest player Value and highest player Wage"
    })
    
    output$Lineplot <- renderPlotly({
        validate(
            need(input$position != "", "Please select a position to see highest player Value and highest player Wage.")
        )
        pos_value = df[,input$position]
        df30 = df
        df30$pos_value = pos_value
        df3 = df30 %>%
            filter(!is.na(wage_eur),!is.na(pos_value)) %>%
            group_by(year) %>%
            summarise(pos_max=max(pos_value), wage_max = max(wage_eur))
        ay <- list(
            tickfont = list(color = "darkorange"),
            overlaying = "y",
            side = "right",
            title = "Wage"
        )
        fig <- plot_ly()
        fig <- fig %>% add_lines(x = ~df3$year, y = ~df3$pos_max, name = "value")
        fig <- fig %>% add_lines(x = ~df3$year, y = ~df3$wage_max, name = "wage", yaxis = "y2",line = list(color = '#48C9B0'))
        fig <- fig %>% layout(
            paper_bgcolor='rgba(0,0,0,0)',
            plot_bgcolor='rgba(0,0,0,0)',
            font = list( color = "white"),
            yaxis = list(
                tickfont=list(color="grey50"),
                side = "left",
                title = "Value"
            ),
            yaxis2 = ay,
            xaxis = list(tickfont=list(color="white"),title="Year"),legend = list(orientation = "v", y = 0.75, x = 1)
        )
        fig
    }) 
    
    }
)
