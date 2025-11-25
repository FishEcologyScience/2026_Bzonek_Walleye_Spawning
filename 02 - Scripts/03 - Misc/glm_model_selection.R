m1 <- glm(Total.Count ~ Slope + Light +
            Sand + Gravel + Cobble + Rubble,
            #+ Armourstone,
            family = poisson,
            data = data_habfish_pseudo)

m2 <- glm(Total.Count ~ Slope + Light +
            Sand + Gravel + Cobble + Rubble + SubstrateDiversity,
            #+ Armourstone,
            family = poisson,
            data = data_habfish_pseudo)

m3 <- glm(Total.Count ~ Slope + Light + Year +
            Sand + Gravel + Cobble + Rubble + SubstrateDiversity,
            #+ Armourstone,
            family = poisson,
            data = data_habfish_pseudo)

m4 <- glm(Total.Count ~ Slope + Light + Year +
            Sand + Gravel + Cobble + SubstrateDiversity,
            #+ Armourstone,
            family = poisson,
            data = data_habfish_pseudo)


performance::r2(m1); performance::r2(m2); performance::r2(m3); performance::r2(m4)
anova(m1, m2, m3, m4)
anova(m4, test = "LRT")
performance::compare_performance(m1, m2, m3, m4)
performance::check_model(m4)
