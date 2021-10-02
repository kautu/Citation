
##
epo.citations <- na.omit(subset(as.tibble(fread('201902_EP_Citations.txt', 
                                                select = c('Citing_pub_date', 'Citing_app_nbr', 'Citing_appln_id', 
                                                           'Cited_pub_date', 'Cited_app_nbr', 'Cited_Appln_id', 
                                                           'Citn_lag_month', 'Citn_Category'))), 
                                Citing_appln_id %in% energy.triadic$Appln_id|Cited_Appln_id %in% energy.triadic$Appln_id) ) %>% 
  distinct(Citing_appln_id, Cited_Appln_id, .keep_all = TRUE)

##
citing.date <- epo.citations[,1:3] %>% 
  distinct(Citing_pub_date, Citing_app_nbr, Citing_appln_id, .keep_all = TRUE) 

#
energy.date <- left_join(energy.triadic, citing.date, by = c('Appln_id' = 'Citing_appln_id')) %>% 
  left_join(node.centrality, by = c('FC' = 'node')) %>% 
  mutate(membership = as.character(membership)) %>% 
  left_join(category.cluster, by = c('membership' = 'node')) %>% 
  distinct(Cluster, membership, FC, Family_id, Appln_id, .keep_all = TRUE) %>% 
  mutate(filing = as.numeric(substring(Citing_app_nbr, 3,6))) %>% 
  left_join(tpf.core, by = 'Family_id')

 
CairoPDF('ClusterBoxplot.pdf', 8, 4.946)   
energy.date %>% 
  group_by(Cluster,membership, Family_id) %>% 
  summarise(filing = min(filing, na.rm = TRUE)) %>% 
  group_by(Cluster,membership) %>% 
  summarise(mean = min(filing, na.rm = TRUE)) %>% 
ggplot(aes(Cluster, mean)) +
  geom_boxplot() +
 #theme(axis.text.x = element_text(angle = 25, hjust = 1))
  coord_flip()
dev.off()
