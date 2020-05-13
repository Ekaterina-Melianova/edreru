# learn_gmap1a.R




library(ggmap)
library(RgoogleMaps)

register_google(key = "AIzaSyCgMUoTSCJ2fXSj3Au6XnGwrAyk1BnQbcQ", write = TRUE)



get_googlemap("waco texas", zoom = 12) %>% ggmap()


