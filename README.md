# Fifa_V4.0

An R Shiny app used for interactive visualization to improve the FIFA player experience, in particular to provide a more interactive user experience and to tap into eye-catching information that might be of interest to users, creating a newly designed Shiny application for users to query personally

https://angywang.shinyapps.io/Fifa_Dashboard_ConditionalSelection/

The panel1 still has flaw at present. In panel1, a person can choose country, league and team to search for all players in that team. In our design, when a person choose a country, the league selection part should only show all the leagues in that country when we are choosing a league. And when a person choose a country and a league, the team selection part should only show all teams in that league. However, at present we can not fulfill this logical search in shiny app because it is too difficult an d complicate. In our latest panel1, when we choose a country, the league selection part will still show all leagues for us to choose, which is same for the team selection part. We plan to improve these selection bars and fulfill logical search in our final report.
