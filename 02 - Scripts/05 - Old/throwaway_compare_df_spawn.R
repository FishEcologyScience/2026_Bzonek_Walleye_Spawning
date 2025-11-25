#saveRDS(df_spawn, "df_spawn_2025-09-13_1_Base.rds")
saveRDS(df_spawn, "df_spawn_2025-10-03_1.rds")

#df_spawn_original <- readRDS("04 - Outputs/df_spawn/df_spawn_2025-09-13_1_Base.rds")
df_spawn_original <- readRDS("04 - Outputs/df_spawn/df_spawn_2025-10-03_1.rds")

identical(df_spawn, df_spawn_original)

df_spawn %>% 
 mutate(mass=mass+1) %>% 
 identical(df_spawn_original)



df_movement_original <- df_movement
identical(df_movement, as.data.frame(df_movement_original))
str(df_movement); str(df_movement_original)

anti_join(df_spawn, df_spawn_original)
