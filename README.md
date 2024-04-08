# Portland Food Weeks

Since 2021 I have been using the {rvest} package in R to turn each of The Portland Mercury's food weeks into spreadsheets. It doesn't stop there - I then import the spreadsheet into Google Sheets where my partner and I each rate how badly we want to try each restaurant's dish on a scale of N (No), M (Maybe), Y (Yes). I then make a combined score column and import the sheet into Google Maps. There I can color code each restaurant by the combined score and display the dish's description, etc. for each restaurant. This is how you do it people! This is how you make a plan of attack for these weeks! This is serious business and you're welcome!

## Going Above and Beyond

Starting in 2023 I started also making a Shiny app to help (1) narrow down your choices and (2) plan your food crawl. I use k-means clustering to cluster your burgers of choice into 1-7 clusters (you set the number of clusters). Hit up the restaurants in one cluster per day. They are grouped into clusters based on geographical location. 

*Pizza Week 2024 is Apr. 15 - Apr. 21, 2024. Plan your pizza crawl [here](https://jennifer-jahncke.shinyapps.io/pdx-food-week/).*
