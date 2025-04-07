# Portland Food Weeks

*Edit April 2025: It appears that Everout has blocked web scraping for the past two food weeks. I fear that after 4 years my era of food spreadshets is over. I learned a lot about web scraping, k-means clustering, and shiny apps through the years through this project. Unfortunately, my food crawls are about to become a lot less efficient, I fear! Still, let us go forth and support our local restaurants.*

Since 2021 I have been using the {rvest} package in R to turn each of The Portland Mercury's food weeks into spreadsheets. It doesn't stop there - I then import the spreadsheet into Google Sheets where my partner and I each rate how badly we want to try each restaurant's dish on a scale of N (No), M (Maybe), Y (Yes). I then make a combined score column and import the sheet into Google Maps. There I can color code each restaurant by the combined score and display the dish's description, etc. for each restaurant. 

## Going Above and Beyond

Starting in 2023 I started also making a Shiny app to help (1) narrow down your choices and (2) plan your food crawl. I use k-means clustering to cluster your burgers of choice into 1-7 clusters (you set the number of clusters). Hit up the restaurants in one cluster per day. They are grouped into clusters based on geographical location. 

