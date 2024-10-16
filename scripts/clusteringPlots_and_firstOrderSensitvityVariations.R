########################################################################
### Author: Bhupinderjeet Singh
### Contact: bhupinderjeet.singh@wsu.edu
### Creation date: 10/15/2024
### Purpose: This code is for analyzing the DELSA sensitivity values
### Dependencies: Flow direction file ; set path
### This expecting the file format used with routing model for VIC flows where the first 6 lines have metadata
###



# set the file path to directory containing data and codes 
file_path_results <- "C:/Users/bhupinderjeet.singh/OneDrive - Washington State University (email.wsu.edu)/WSU/Research Proposal/Paper/Sensitivity_analysis/JournalOfHydrology/Proof_read/Codes"

setwd(file_path_results)

CRB_shapefile <- readOGR("C:/Users/bhupinderjeet.singh/OneDrive - Washington State University (email.wsu.edu)/Project/BSYSE_retreat/Snow_dominated_analysis/Shapefiles/CRB/WBDHU2.shp",verbose = FALSE)

PNW_HUC6 <- readOGR("C:/Users/bhupinderjeet.singh/OneDrive - Washington State University (email.wsu.edu)/Project/BSYSE_retreat/Snow_dominated_analysis/Shapefiles/HUC6/pnw_huc6_wgs84.shp",verbose = FALSE)


kmeans_clusters <- read.csv("./data/processed_data/DELSA_kmeans_clusters.csv") %>% 
  mutate(file_name = paste0("flux_",Latitude,"_",Longitude))

plot_KmeansCluster_spatialmap <- ggplot()+
  geom_tile(kmeans_clusters,mapping = aes(x = Longitude,y=Latitude,fill = factor(Cluster_ID)))+
  theme_bw()+
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"),labels = c("Class 1","Class 2","Class 3","Class 4")) +
  geom_polygon(data = fortify(CRB_shapefile), aes(x=long, y=lat, group=group), fill = NA,
               colour = alpha("black", 0.5), size = 0.5)+
  geom_polygon(data = fortify(PNW_HUC6), aes(x=long, y=lat, group=group), fill = NA,
               colour = alpha("black", 0.5), size = 0.5)+
  theme(legend.position = c(0.82,0.8),
        legend.title = element_blank(),
        legend.title.align = 0.5,
        legend.text = element_text(size = 18),
        legend.key.height = unit(0.8,"cm"),
        legend.key.width = unit(0.5,"cm"),
        axis.text = element_text(size= 14, face = "bold",color = "black"),
        axis.title = element_blank(),
        legend.background = element_blank())  +
  jpeg(filename=paste("./plots/KmeansClustering_spatialmap.jpeg", sep=""), bg="white", width=2000, height=2000, pointsize=12, res=300, quality=100)
plot(plot_KmeansCluster_spatialmap)
dev.off()



## plotting the variations in the first order DELSA sensitvity index by cluster and across simulations


fos_max_swe_all_params <- read.csv("./data/processed_data/FOS_max_SWE.csv")


fos_max_swe_and_kmeans_cl <- fos_max_swe_all_params %>% 
  inner_join(.,kmeans_clusters[,c("file_name","Cluster_ID")],by ="file_name") %>% 
  mutate(Cluster_name = case_when(Cluster_ID == 0 ~ "Class_1",
                                  Cluster_ID == 1 ~ "Class_2",
                                  Cluster_ID == 2 ~ "Class_3",
                                  Cluster_ID == 3 ~ "Class_4"))

df_fos_max_swe_kmeans_cl <- fos_max_swe_and_kmeans_cl %>% 
  dplyr::select(-c(file_name,Cluster_ID)) %>% 
  melt(.,id ="Cluster_name" ) %>% 
  ungroup() %>% 
  mutate(param =sub("_\\d+$", "", variable))


boxplot_fos_clusters <- ggplot(df_fos_max_swe_kmeans_cl, aes(x = param, y = abs(value), fill = Cluster_name)) +
  geom_boxplot(alpha =0.9,outlier.shape = NA)+
  facet_wrap( ~Cluster_name,ncol = 1)+
  stat_boxplot(geom='errorbar')+
  theme_bw()+
  scale_fill_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"))+    #"#66C2A5","#FC8D62", "#8DA0CB" ,"#E78AC3" ,"#A6D854", "#FFD92F"
  labs(y = "First order sensitivity")+
  theme(axis.text.x = element_text(size = 14,colour = "black",angle = 90,vjust = 0.5,hjust = 1),
        axis.text.y = element_text(size = 12,colour = "black"),
        axis.title.y.left=element_text(size = 18,colour = "black"),
        axis.title.x = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 16,colour = "black"))+
  jpeg(filename=paste("./plots/Boxplot_FOS_byCluster.jpeg", sep=""), bg="white", width=2000, height=2000, pointsize=12, res=300, quality=100)
plot(boxplot_fos_clusters)
dev.off()

################################################################
##
## monthly values of hydro-meteorological variables
##
################################################################


df_precip_runoff_temp <- read.csv("./data/processed_data/PrecipRunoffTemp_median.csv")

df_precip_runoff_temp$variable <- factor(df_precip_runoff_temp$variable, levels = c("Precipitation ratio", "Runoff ratio", "Mean monthly temperature"))


facet_lineplot_precip_runoff_temp <- ggplot() +
  geom_line(df_precip_runoff_temp, mapping =  aes(x=WY_month, y=Median_value, group=as.factor(Cluster_name),color = Cluster_name),size = 1)+
  theme_bw()+
  scale_color_manual(values = c("#56B4E9","#D55E00","#009E73","#CC79A7"))+ #"blue","red","#028A0F","gray"
  scale_x_discrete(limits = 1:12,labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))+
  facet_wrap(variable~., scales = "free_y",ncol = 1)+
  theme(axis.text.x = element_text(size = 18,colour = "black"),
        axis.text.y = element_text(size = 18,colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y.left =  element_blank(),
        legend.position = c(0.85,0.55),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 16,colour = "black"),
        strip.text = element_text(size = 20,colour = "black")) +
  jpeg(filename=paste("./plots/plot_precip_runoff_temp.png", sep=""), bg="white", width=2500, height=3000, pointsize=12, res=300, quality=100)
plot(facet_lineplot_precip_runoff_temp)
dev.off()
