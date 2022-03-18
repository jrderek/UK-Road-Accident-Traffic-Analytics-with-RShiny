# UK-Road-Accident-Traffic-Analytics-with-RShiny

This is an Analytics project that give insights into UK Road accident & Traffic data using RShiny Dashboard.

This project will give detailed insights into United Kingdom (UK) long-term road traffic and accident trends between 2000 - 2018 based on estimated Annual average daily flows (AADFs) and volume of traffic (i.e. miles driven per year for each vehicle on a particular road) for vehicle types, road networks/types, geographical regions, and highway authorities in UK.

- N.B: It takes about 8 - 10 minutes for the RShiny Dashboard to load, because of the size of the maps rendered
RShiny Dashboard link (currently Not Active)
- Tmap Generated GIF maps
- 
- Example code
uk_traffic_lad_animation <- tm_shape(uk_lad_shp_map_all_vehs) +
  tm_polygons(col = "avg_traffic_flow", palette =colorRampPalette(brewer.pal(10,"RdPu"))(8), n = 8,
              title = "# Miles Covered") +
tm_facets(along = "year") + tm_style("cobalt") + tm_layout(
    legend.text.size = 1.2, legend.position = c("right", "top"),
    legend.title.size = 1.7, main.title.size = 2.0,
    legend.height = 0.3)
tmap_animation(
  uk_traffic_lad_animation, filename = "uk_shape_file/uk_traffic_lad_animation2.gif",
  delay = 75, width = 850, height = 1000
  )
- UK & London Road Traffics Map Gifs
  

![image](https://user-images.githubusercontent.com/96236642/159071803-896bb2b8-24e4-4fe5-bfe1-0b808619d2b7.png)

![image](https://user-images.githubusercontent.com/96236642/159071820-29373e00-ff0b-43c7-ae2f-3e62808f8aeb.png)


![image](https://user-images.githubusercontent.com/96236642/159071846-2f850902-a869-4b65-ba84-1b405539a98a.png)

![image](https://user-images.githubusercontent.com/96236642/159071862-baa0c0ef-7f37-4918-9c61-da7602038218.png)

- UK & London Road Accidents Map Gifs
  
