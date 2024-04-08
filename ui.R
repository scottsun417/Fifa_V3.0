shinyUI(
  fluidPage(theme = shinytheme("superhero"),
            h1(id="title", "JUST FIFA IT!"),
            tags$style(HTML("#title{color: darkorange;}")),
            navbarPage("Let's get started",
                       tabPanel(icon("home"),
                                
                                fluidRow(column(tags$img(src="https://i.pinimg.com/originals/14/92/3a/14923a963f844b0acfa650de4a3353b5.jpg",width="200px",height="260px"),width=2),
                                         column(
                                           
                                           br(),
                                           p("This FIFA dashboard will provide you with features like data query, comparison and distribution, where users can look up basic and advanced information about certain player, 
                                             discover information about the players they interested in and explore something interesting, etc.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                           h4(em("Player Info"),style="color:grey50;text-align:center"),
                                           p("In this panel, the upper part is for selection, and the latter part is used for illustrating the selected player’s personal information.
                                           On the top part of this panel, three correlated selection icons of Country, League and Team are employed to show the roster list for a specific team.",
                                           strong("Choose the country, League and Team, and click the 'Search' button, then the roster list will show up."),
                                           "From the roster list of all players’ names in that team, one can",
                                           strong("click his interested player, and then click the 'Search' button again"),
                                           "to see further information for the selected player. 
                                           In the player information part, we put the player’s basic information (Name, Date of birth, Club, Position, Value, Height, Weight .etc) in the first tab,
                                           and 3 other sections about their Rating, Real overall rating and Tags are shown in other tabs.
                                           This panel focus on selection, dynamic linking and faceting.",style="text-align:justify;color:black;background-color:#d3d3d3;padding:15px;border-radius:10px"),
                                           h4(em("Top 10 Players"),style="color:grey50;text-align:center"),
                                           p("In the second panel, we aim at showing the distribution for the top 10 players of each position.
                                             The radio button is used for selecting specific position, and the time selection slide is dynamically linked with the map and scatter plot. By ",
                                             strong("clicking a specific position and selecting a specific year, and then click the 'Search' button"),
                                             ", the map will change correspondingly, where the geographic distribution will be shown.
                                             The scatter plot of relationship between ratings and wages will change accordingly.
                                             The line plot for the changes of highest wage and value for players in that position among years will update.
                                             This panel utilizes various visualization formats, such as map, scatter plot and line plot.
                                             Dynamic linking is a shiny point for this panel as well.",
                                             style="text-align:justify;color:black;background-color:#d3d3d3;padding:15px;border-radius:10px"),
                                           br(),
                                           p("Click ",strong("More "), "to find more information about FIFA and feel free to give us feedback! (also see the example conclusions)",
                                             style="text-align:justify;color:black;background-color:#d3d3d3;padding:15px;border-radius:10px"),
                                           width=8),
                                         column(
                                           br(),
                                           tags$img(src="https://i.guim.co.uk/img/media/55bcb9436b2a0b88c02d6f5804ad915c087a89f4/0_29_1567_940/master/1567.jpg?width=1200&height=1200&quality=85&auto=format&fit=crop&s=4b3ea416cfaa33ebbe520be0a7e15bc7",width="200px",height="260px"),
                                           br(),
                                           br(),
                                           width=2)),
                                
                                
                                tags$style(".fa-database {color:#E87722}"),
                                h3(p(em("FIFA "),icon("futbol",lib = "font-awesome"),style="color:black;text-align:center")),
                                fluidRow(column(DT::dataTableOutput("RawData"),
                                                width = 12)),
                                
                                hr(),
                                p(em("Developed by"),br("Wenyi Wang, Yifan Du, Yiran Wang, Yuxiao Li, Zhiyi Sun"),style="text-align:center; font-family: times")
                       ),
      
        # Panel 1
        tabPanel("Player Info",
                 fluid = TRUE,
                 h2(id="big-heading", "Let's search for the player!"),
                 tags$style(HTML("#big-heading{color: darkorange;}")),
                 tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
                    color: orange;
                    }
                    .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}

                ###To change text and background color of the `Search` box ###
                    .dataTables_filter input {
                            color: black;
                            background-color: white
                           }

                    thead {
                    color: white;
                    }

                     tbody {
                    color: white;
                    }

                   "
                 )
                 ),
                 
                 # Selection in Sidebar
                 sidebarLayout(
                   
                     sidebarPanel(
                         width = 4,
                         p(strong("Select the player that interests you:")),                         # Country
                         div(style="display: vertical-align:top; width: 200px;",
                             selectInput(inputId = 'country',
                                         "Country:",
                                         choices = unique(as.character(country))
                                         )
                             ),
                         # League
                         div(style="display: vertical-align:top; width: 200px;",
                             selectInput(inputId = 'league',
                                         "League:",
                                         choices = unique(as.character(league_name))
                                         )
                             ),
                         # Team
                         div(style="display: vertical-align:top; width: 200px;",
                             selectInput(inputId = 'clubname',
                                         "Team:",
                                         choices = unique(as.character(team))
                                         )
                             ),
                         # Year
                         div(style="display: vertical-align:top; width: 200px;",
                             sliderInput(inputId = 'year_panel1',
                                         "Select a year:",
                                         min = 2015,
                                         max = 2020,
                                         value = 2020,
                                         step = 1,
                                         ticks = FALSE)
                         ),
                         br(),
                         submitButton("Search!",icon = icon("search"))
                         ),
                     
                     column(
                       width = 8,
                       h4(id="panel1-roster", textOutput("roster_title")),
                       tags$style(HTML("#panel1-roster{color: darkorange;text-align:center;}")),
                       DT::dataTableOutput('roster'),
                       h4(id="panel1-click-notice", textOutput("info")),
                       tags$style(HTML("#panel1-click-notice{color: grey50;}"))
                       )
                     ),
                 
                 # Display in Main panel
                 mainPanel(
                   width = 12,
                   tabsetPanel(
                       # Basic Personal Information
                       tabPanel("Basic Personal Information",
                                br(),
                                tags$style("#name .bg-orange { background-color: #FF8c00 !important; color: #000000 !important; text-align:center;}"),
                                valueBoxOutput("name", width = 3),
                                tags$style("#DB .bg-orange { background-color: #FF8c00 !important; color: #000000 !important; text-align:center;}"),
                                valueBoxOutput("DB", width = 3),
                                tags$style("#Position .bg-orange { background-color: #FF8c00 !important; color: #000000 !important;text-align:center;}"),
                                valueBoxOutput("Position", width = 3),
                                tags$style("#Club .bg-orange { background-color: #FF8c00 !important; color: #000000 !important;text-align:center; }"),
                                valueBoxOutput("Club", width = 3),
                                tags$style("#Value .bg-orange { background-color: #FF8c00 !important; color: #000000 !important;text-align:center; }"),
                                valueBoxOutput("Value", width = 3),
                                tags$style("#Wage .bg-orange { background-color: #FF8c00 !important; color: #000000 !important;text-align:center; }"),
                                valueBoxOutput("Wage", width = 3),
                                tags$style("#PF .bg-orange { background-color: #FF8c00 !important; color: #000000 !important;text-align:center; }"),
                                valueBoxOutput("PF", width = 3),
                                tags$style("#NT .bg-orange { background-color: #FF8c00 !important; color: #000000 !important;text-align:center; }"),
                                valueBoxOutput("NT", width = 3)
                       ),
                          
                       # Rating
                       tabPanel("Rating",
                                br(),
                                plotOutput("Rating_plot")
                                 ),
                        
                       # Real overall rating
                        tabPanel("Real Overall Rating",
                                 br(),
                                 plotOutput("Real_Overall_plot")
                                 ),
                        
                       # Tag
                        tabPanel("Tag",
                                 br(),
                                 plotOutput("Tags_plot")
                                 )              
                         )
                     )
                 )
        ,

        # Panel 2
        tabPanel("Top 10 Players",
                 fluid = TRUE,
                 h1(id="big-heading", "Top 10 Players of different positions"),
                 tags$style(HTML("#big-heading{color: darkorange;}")),

                 sidebarLayout(
                   # Selection part
                   sidebarPanel(width = 5,
                     p(strong("Select the position:")),
                     # Position
                     tagList(
                       tags$style(HTML(".multicol{font-size:12px;
                                                height:auto;
                                                -webkit-column-count: 3;
                                                -moz-column-count: 3;
                                                column-count: 3;}")
                                  ),
                       tags$div(align = "left",
                              class = "multicol",
                       prettyRadioButtons(
                        inputId = "position",
                        label = NULL,
                        choices = c("LS","ST","RS","LW","LF","CF","RF","RW","LAM","CAM","RAM","LM","LCM","CM","RCM","RM","LWB","LDM","CDM","RDM","RWB","LB","LCB","CB","RCB","RB"),
                        animation = "pulse",
                        status = "danger",
                        icon = icon("futbol"),
                        selected = 1
                        )
                       ),
                       br(),
                       div(style="display: vertical-align:top; width: 200px;",
                           sliderInput(inputId = "year",
                                       "Select a year:",
                                       min = 2015,
                                       max = 2020,
                                       value = 2020,
                                       step = 1,
                                       ticks = FALSE)
                           ),
                       br(),
                       submitButton("Search!",icon = icon("search"))
                     )
                   ),
                   
                   # Map part
                   mainPanel(width = 7,
                             h5(id="panel2-Mapplot", textOutput("Mapplot_title")),
                             tags$style(HTML("#panel2-Mapplot{color: white;text-align:center;}")),
                             plotOutput("Mapplot", height="400px")
                   )
                 ),
                   
                   # Scatter and line plot
                   mainPanel(
                     width = 12,
                     column(width = 6, 
                            h5(id="panel2-Scatterplot", textOutput("Scatterplot_title")),
                            tags$style(HTML("#panel2-Scatterplot{color: white;text-align:center;}")),
                            plotOutput("Scatterplot")
                     ),
                     column(width = 6, 
                            h5(id="panel2-Lineplot", textOutput("Lineplot_title")),
                            tags$style(HTML("#panel2-Lineplot{color: white;text-align:center;}")),
                            plotlyOutput("Lineplot")
                     )
                   )
        ),
        
        navbarMenu("More", icon = icon("info-circle"),
                   tabPanel("About", fluid = TRUE,
                            fluidRow(
                              column(4,
                                     h4(p("About the FIFA",style="text-align:center;color:darkorange")),
                                     p("For more information please check the",em("FIFA Video Games - Official EA Site"),"page clicking",
                                       a(href="https://www.ea.com/games/fifa", "Here",target="_blank"),style="text-align:left;color:black"),
                                     br(),
                                     p("Our analysis bases on 2015-2021 FIFA complete player data set, which includes the players data for the Career Mode from FIFA 15 to FIFA 21. For the original data source please check this kaggle page clicking",
                                       a(href="https://www.kaggle.com/stefanoleone992/fifa-21-complete-player-dataset", "Here",target="_blank"),style="text-align:left;color:black")
                              ),
                              HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/C7PvkyEwiRg" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                              br()
                            )
                   ),
                   tabPanel("Conclusion", fluid = TRUE,
                            fluidRow(
                              column(12,
                                     h4(p("Example Conclusions",style="text-align:center;color:darkorange")),
                                     h5(p("From Player Info, we can find which player is the best by the comparison between Rating and OverallRating. Moreover, we can see that there seems a closly relationship between the club the players belong to and their nationality."),
                                        p("From Top 10 Players, we can see from the world map that most of the best players are concentrated in Europe and South America.
                                          We can also find that when the Rating is over 70, the higher the Rating is, the higher the wage will be. 
                                          What's more, for most positions, players wage seem to be higher than their actual value."))  
                                 )
                            )
                   ),
                   tabPanel("Feedback", fluid = TRUE,
                            fluidRow(
                              column(8,
                                     h4(p("PLEASE GIVE US FEEDBACK! THANK YOU!")),
                                     h5(p("Wenyi Wang: wwang584@wisc.edu"),
                                        p("Yifan Du: ydu76@wisc.edu"),
                                        p("Yiran Wang: wang2559@wisc.edu"),
                                        p("Yuxiao Li: li2268@wisc.edu"),
                                        p("Zhiyi Sun: zsun255@wisc.edu")
                                     )
                              ),
                              HTML('<img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTmUh0bcMAyoy6TnjkavXXKHL0qXiZoAP3t9w&usqp=CAU", height="200px"'),
                              br())
                            
                   )
        )
        
    )
  )
)
