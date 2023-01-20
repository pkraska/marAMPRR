# dist2022 <- data2022[['si']] %>%
#   filter(sampleType == "B") %>%
#   sf::st_distance(cages) %>%
#   as_tibble() %>%
#   mutate(data2022[['si']] %>%
#            filter(sampleType == "B") %>%
#            select(sampleCode))%>%
#   select(-geometry)
#
# colnames(dist2022) <- c(cages$Name, "sampleCode")
#
# dist2022 <- dist2022 %>%
#   mutate(sampleCode = data2022[['si']] %>%
#            filter(sampleType == "B") %>%
#            select(sampleCode) %>%
#            sf::st_drop_geometry() %>%
#            pull())
# # ,
# # dist2target = case_when(grepl("F08",sampleCode) ~ `MF-0016`,
# #                         grepl("F09", sampleCode) ~ `MF-0027-1`))
#
# dist2022_long <- dist2022 %>%
#   pivot_longer(contains("MF"), values_to = "distance") %>%
#   group_by(sampleCode) %>%
#   arrange(distance) %>%
#   summarize(closest = distance[1], sec_close = distance[2]) %>%
#   write_csv("C:/Users/kraskape/DFO-MPO/Aquaculture Monitoring Program (AMP) - Maritimes/data/2022/2022_dist2cages.csv")
#
