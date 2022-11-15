# Etsy Growth

# load packages
pacman::p_load(dplyr, ggplot2, rvest, ggimage, stringr, showtext)

# source data url
url <- "https://www.businessofapps.com/data/etsy-statistics/"

# get buyers table
users <- url |>
  read_html() |>
  html_element("#footable_parent_80660 table") |>
  html_table()

# get sellers table
sellers <- url |>
  read_html() |>
  html_element("#footable_parent_80661 table") |>
  html_table()

# combine dfs
etsy_comb <- users |>
  left_join(sellers, by = "Year") |>
  mutate(buyers_per_seller = round(`Users (mm)` / `Sellers (mm)`))

# create blank df
user_df <- data.frame()

# create df to plot buyers per year
for (i in 1:nrow(etsy_comb)) {
  
  user_it <- data.frame(Year = c(rep(etsy_comb$Year[i], etsy_comb$buyers_per_seller[i])),
                        buyers_per_seller = c(1:etsy_comb$buyers_per_seller[i]),
                        count = c(rep(1, etsy_comb$buyers_per_seller[i])))
  
  user_df <- rbind(user_df, user_it)
  
}

# arrange df for plotting
user_df <- user_df |>
  arrange(-buyers_per_seller)

# import fonts from Google
font_add_google("Poppins", "Poppins")
font_add_google("Fredoka One", "Fredoka One")
showtext_auto()
gfont <- "Poppins"
titlefont <- "Fredoka One"

# create plot
etsy_comb |>
  ggplot(aes(x = Year, y = buyers_per_seller)) +
  geom_col(fill = "#ed861c", width = 0.4, aes(alpha = buyers_per_seller)) +
  geom_image(data = user_df, aes(image = "img/person.png"), size = 0.04) +
  geom_image(aes(y = -1, image = "img/shop.png"), size = 0.09) +
  annotate(geom = "segment", x = 2020.5, xend = 2020.5, y = 13, yend = 19, color = "#1d1d1d", size = 6) +
  annotate(geom = "text", x = 2020.5, y = 16, label = toupper("74% increase in Etsy Sellers"), angle = 90, size = 9, color = "white", family = gfont, fontface = "bold") +
  scale_x_continuous(breaks = c(seq(2012, 2022))) +
  scale_y_continuous(limits = c(-1, 19), breaks = c(seq(0, 19))) +
  scale_alpha(range = c(0.3, 0.9)) +
  labs(title = "Seller Perspective of Etsy's Growth", 
       subtitle = str_wrap("The Etsy marketplace has grown rapidly over the last few years after an influx of users during the pandemic in 2020, but how has this affected long-term Etsy sellers? The 2020 rush of new users was followed by a huge 74% increase in the number of active sellers in 2021 (compared to an increase of 18% in the number of active buyers), creating a level of competition among sellers not seen on Etsy since 2012.", 80),
       caption = "data: businessofapps.com",
       y = "Buyer/Seller Ratio") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = titlefont, color = "#ed861c", size = 60, margin = margin(t=10,b=10,unit="pt")),
        plot.subtitle = element_text(size = 35, lineheight = 0.3),
        text = element_text(family = gfont, size = 40, color = "#1d1d1d"))

# save plot
ggsave("outputs/test.jpg", width = 8, height = 9.5)
