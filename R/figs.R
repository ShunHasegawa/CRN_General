all_dd <- Reduce(function(...) merge(..., by = "sample_id"),
                 list(treat_dd, biomass_dd, leaf_chem_dd, soilnut_dd, 
                      root_CumSum_dd_cst[, c("sample_id", "SA_DC15")], 
                      exudate_raw_dd_cst, phosphtase_dd))


# '-' is hard to handle when used in column names so replace
names(all_dd) <- gsub("-", "_", names(all_dd))

# acid phosphatase activity per root surface area
all_dd <- mutate(all_dd, 
                 total_MUB_root = total_MUB/SA_DC15)

# Microlaena --------------------------------------------------------------

mic_dd <- all_dd %>% 
  filter(species == "Microlaena") %>%
  mutate(CW = co2:water)

# > Factors driven by OAs  ---------------------------------------------

# . Rhizosphere solution --------------------------------------------------
par(mfrow = c(3, 3), mar = c(2, 4, 2, 1))
oa_R_dd <- cbind(CW = mic_dd$CW, mic_dd[, grepl("_R$", names(mic_dd))])
oa_R_dd_mlt <- melt(oa_R_dd, id = "CW")
d_ply(oa_R_dd_mlt, .(variable), 
      function(x) boxplot(value ~ CW, main = unique(x$variable), data = x))
# plot citric malic, and shikimic

create_fig_byOA_wide(data = mic_dd, showlayout = FALSE, 
                     fig_title = "Microlaena-Rhizosphere", 
                     
                     xval1 = "citric_R", pred_xval1 = "citric_R", 
                     xlab1 = expression(Citric~(nmol)),
                     
                     xval2 = "malic_R", pred_xval2 = "malic_R",
                     xlab2 = expression(Malic~(nmol)),
                     
                     xval3 = "shikimic_R", pred_xval3 = "shikimic_R",
                     xlab3 = expression(Shikimic~(nmol)),
                     
                     xval4 = "sqrt(total_R)", pred_xval4 = "total_R",
                     xlab4 = expression(sqrt(Total~exudate~(nmol))),
                     
                     yval1 = "log(total_MUB_root)", 
                     ylab1 = expression(atop("log(Acid phosphatse)", 
                                             (MUB~nmol~h^'-1'~mm^'-2'))),
                     
                     yval2 = 'log(leaf_Mn)', ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                     
                     yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio", 
                     
                     yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil N:P ratio)")

# save plot
p <- recordPlot()

pdf(file = "Output/Figs/Microlaena_by_RhizosphereOA.pdf", 
    width = 6.5, height = 6.5)
replayPlot(p)
dev.off()

save_png600(file = "Output/Figs/Microlaena_by_RhizosphereOA.png", 
            width = 6.5, height = 6.5)
replayPlot(p)
dev.off()

# . plant solution --------------------------------------------------------
par(mfrow = c(3, 3), mar = c(2, 4, 2, 1))
oa_P_dd <- cbind(CW = mic_dd$CW, 
                 mic_dd[, grepl("_P$", names(mic_dd)) & !grepl("^leaf", names(mic_dd))])
oa_P_dd_mlt <- melt(oa_P_dd, id = "CW")
d_ply(oa_P_dd_mlt, .(variable), 
      function(x) boxplot(value ~ CW, main = unique(x$variable), data = x))
# plot fumaric, malic and shikimic

create_fig_byOA_wide(data = mic_dd, showlayout = FALSE,
                     fig_title = "Microlaena-Plant", 
                     
                     xval1 = "fumaric_P", pred_xval1 = "fumaric_P",
                     xlab1 = expression(Fumaric~(nmol)),
                     
                     xval2 = "malic_P", pred_xval2 = "malic_P",
                     xlab2 = expression(Malic~(nmol)),
                     
                     xval3 = "shikimic_P", pred_xval3 = "shikimic_P",
                     xlab3 = expression(Shikimic~(nmol)),
                     
                     xval4 = "sqrt(total_P)", pred_xval4 = "total_P",
                     xlab4 = expression(sqrt(Total~exudate~(nmol))),
                     
                     yval1 = "log(total_MUB_root)", 
                     ylab1 = expression(atop("log(Acid phosphatse)", 
                                             (MUB~nmol~h^'-1'~mm^'-2'))),
                     
                     yval2 = 'log(leaf_Mn)', ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                     
                     yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio", 
                     
                     yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil N:P ratio)")

p <- recordPlot()

pdf(file = "Output/Figs/Microlaena_by_PlantOA.pdf", width = 6.5, height = 6.5)
replayPlot(p)
dev.off()

save_png600(filename = "Output/Figs/Microlaena_by_PlantOA.png", width = 6.5, height = 6.5)
replayPlot(p)
dev.off()

# > Factors driving OAs -----------------------------------------------


# . Rhizosphere solution -------------------------------------------------

par(mfrow = c(2, 2), mar = c(3, 2, 2, 1))
boxplot(sqrt(total_R) ~ CW, data = mic_dd, main = "sqrt(total_R)")
boxplot(SA_DC15 ~ CW, data = mic_dd, main = "SA_DC15")
boxplot(leaf ~ CW, data = mic_dd, main = "leaf_mass")
boxplot(root ~ CW, data = mic_dd, main = "root_mass")

create_fig_OAdriver_wid(data = mic_dd, showlayout = FALSE,
                        fig_title = "Microlaena-Rhizosphere", 
                        
                        xval1 = "SA_DC15", pred_xval1 = "SA_DC15", 
                        xlab1 = expression(Root~surface~area~(mm^2)), 
                        
                        xval2 = "root", pred_xval2 = "root", 
                        xlab2 = "Root mass (mg)", 
                        
                        xval3 = "leaf",pred_xval3 = "leaf", 
                        xlab3 = "Leaf mass (mg)", 
                        
                        yval1 = "citric_R", ylab1 = expression(Citric~(nmol)),
                        
                        yval2 = "malic_R", 
                        ylab2 = expression(Malic~(nmol)),
                        
                        yval3 = "shikimic_R", 
                        ylab3 = expression(Shikimic~(nmol)),
                        
                        yval4 = "sqrt(total_R)", 
                        ylab4 = expression(sqrt(Total~exudate~(nmol))))
p <- recordPlot()

pdf(file = "Output/Figs/Microlaena_rhizoOAdriving_factors.pdf", 
    width = 6, height = 6.5)
replayPlot(p)
dev.off()

save_png600(filename = "Output/Figs/Microlaena_rhizoOAdriving_factors.png",
            width = 6, height = 6.5)
replayPlot(p)
dev.off()


# . Plant solution -------------------------------------------------------
create_fig_OAdriver_wid(data = mic_dd, showlayout = FALSE, 
                        fig_title = "Microlaena-Plant",
                        
                        xval1 = "SA_DC15", pred_xval1 = "SA_DC15", 
                        xlab1 = expression(Root~surface~area~(mm^2)), 
                        
                        xval2 = "root", pred_xval2 = "root", 
                        xlab2 = "Root mass (mg)", 
                        
                        xval3 = "leaf",pred_xval3 = "leaf", 
                        xlab3 = "Leaf mass (mg)", 
                        
                        yval1 = "fumaric_P", ylab1 = expression(Fumaric~(nmol)),
                        
                        yval2 = "malic_P", 
                        ylab2 = expression(Malic~(nmol)),
                        
                        yval3 = "shikimic_P", 
                        ylab3 = expression(Shikimic~(nmol)),
                        
                        yval4 = "sqrt(total_P)", 
                        ylab4 = expression(sqrt(Total~exudate~(nmol))))

p <- recordPlot()

pdf(file = "Output/Figs/Microlaena_plantOAdriving_factors.pdf", 
    width = 6, height = 6.5)
replayPlot(p)
dev.off()

save_png600(filename = "Output/Figs/Microlaena_plantOAdriving_factors.png",
            width = 6, height = 6.5)
replayPlot(p)
dev.off()


# Hakea -------------------------------------------------------------------

hak_dd <- all_dd %>% 
  filter(species == "Hakea") %>%
  mutate(CW = co2:water)


# > Factor driven by OAs --------------------------------------------------

# . Rhizosphere solution --------------------------------------------------
par(mfrow = c(3, 3), mar = c(2, 4, 2, 1))
oa_R_dd <- cbind(CW = hak_dd$CW, hak_dd[, grepl("_R$", names(hak_dd))])
oa_R_dd_mlt <- melt(oa_R_dd, id = "CW")
d_ply(oa_R_dd_mlt, .(variable), 
      function(x) boxplot(value ~ CW, main = unique(x$variable), data = x))
# plot cis_aconitic_R, oxalic_R, trans_aconitic_R

create_fig_byOA_wide(data = hak_dd, showlayout = FALSE,
                     fig_title = "Hakea-Rhizosphere", 
                     
                     xval1 = "cis_aconitic_R", pred_xval1 = "cis_aconitic_R", 
                     xlab1 = expression(Cis*-aconitic~(nmol)),
                     
                     xval2 = "Oxalic_R", pred_xval2 = "Oxalic_R",
                     xlab2 = expression(Oxalic~(nmol)),
                     
                     xval3 = "log(trans_aconitic_R)", 
                     pred_xval3 = "trans_aconitic_R",
                     xlab3 = "log(Trans-aconitic\n(nmol))",
                     
                     xval4 = "log(total_R)", pred_xval4 = "total_R",
                     xlab4 = "log(Total exudate\n(nmol))",
                     
                     yval1 = "total_MUB_root", 
                     ylab1 = expression(atop("Acid phosphatse", 
                                             (MUB~nmol~h^'-1'~mm^'-2'))),
                     
                     yval2 = 'log(leaf_Mn)', ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                     
                     yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio", 
                     
                     yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil N:P ratio)")

p <- recordPlot()

pdf(file = "Output/Figs/Hakea_by_RhizosphereOA.pdf", width = 6.5, height = 6.5)
replayPlot(p)
dev.off()
save_png600(filename = "Output/Figs/Hakea_by_RhizosphereOA.png", 
            width = 6.5, height = 6.5)
replayPlot(p)
dev.off()

# . Plant solution --------------------------------------------------------
par(mfrow = c(3, 3), mar = c(2, 4, 2, 1))
oa_P_dd <- cbind(CW = hak_dd$CW, 
                 hak_dd[, grepl("_P$", names(hak_dd)) & 
                          !grepl("^leaf", names(hak_dd))])
oa_P_dd_mlt <- melt(oa_P_dd, id = "CW")
d_ply(oa_P_dd_mlt, .(variable), 
      function(x) boxplot(value ~ CW, main = unique(x$variable), data = x))

create_fig_byOA_wide(data = hak_dd, showlayout = FALSE,
                     fig_title = "Hakea-Plant", 
                     
                     xval1 = "cis_aconitic_P", pred_xval1 = "cis_aconitic_P", 
                     xlab1 = expression(Cis*-aconitic~(nmol)),
                     
                     xval2 = "Oxalic_P", pred_xval2 = "Oxalic_P",
                     xlab2 = expression(Oxalic~(nmol)),
                     
                     xval3 = "log(trans_aconitic_P)", 
                     pred_xval3 = "trans_aconitic_P",
                     xlab3 = "log(Trans-aconitic\n(nmol))",
                     
                     xval4 = "log(total_P)", pred_xval4 = "total_P",
                     xlab4 = "log(Total exudate\n(nmol))",
                     
                     yval1 = "total_MUB_root", 
                     ylab1 = expression(atop("Acid phosphatse", 
                                             (MUB~nmol~h^'-1'~mm^'-2'))),
                     
                     yval2 = 'log(leaf_Mn)', ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                     
                     yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio", 
                     
                     yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil N:P ratio)")

p <- recordPlot()

pdf(file = "Output/Figs/Hakea_by_PlantOA.pdf", width = 6.5, height = 6.5)
replayPlot(p)
dev.off()
save_png600(filename = "Output/Figs/Hakea_by_PlantOA.png", width = 6.5, height = 6.5)
replayPlot(p)
dev.off()


# > Factors driving OAs ---------------------------------------------------


# . Rhizosphere solution --------------------------------------------------
create_fig_OAdriver_wid(data = hak_dd, showlayout = FALSE, 
                        fig_title = "Hakea-Rhizosphere",
                        
                        xval1 = "SA_DC15", pred_xval1 = "SA_DC15", 
                        xlab1 = expression(Root~surface~area~(mm^2)), 
                        
                        xval2 = "root", pred_xval2 = "root", 
                        xlab2 = "Root mass (mg)", 
                        
                        xval3 = "leaf",pred_xval3 = "leaf", 
                        xlab3 = "Leaf mass (mg)", 
                        
                        yval1 = "cis_aconitic_R", 
                        ylab1 = expression(Cis*-aconitic~(nmol)),
                        
                        yval2 = "Oxalic_R", 
                        ylab2 = expression(Oxalic~(nmol)),
                        
                        yval3 = "log(trans_aconitic_R)", 
                        ylab3 = "log(Trans-aconitic)\n(nmol)",
                        
                        yval4 = "log(total_R)", 
                        ylab4 = "log(Total exudate)\n(nmol)")
p <- recordPlot()

pdf(file = "Output/Figs/Hakea_rhizoOAdriving_factors.pdf", 
    width = 6, height = 6.5)
replayPlot(p)
dev.off()

save_png600(filename = "Output/Figs/Hakea_rhizoOAdriving_factors.png",
            width = 6, height = 6.5)
replayPlot(p)
dev.off()


# . Plant solution --------------------------------------------------------
create_fig_OAdriver_wid(data = hak_dd, showlayout = FALSE, 
                        fig_title = "Hakea-Plant",
                        
                        xval1 = "SA_DC15", pred_xval1 = "SA_DC15", 
                        xlab1 = expression(Root~surface~area~(mm^2)), 
                        
                        xval2 = "root", pred_xval2 = "root", 
                        xlab2 = "Root mass (mg)", 
                        
                        xval3 = "leaf",pred_xval3 = "leaf", 
                        xlab3 = "Leaf mass (mg)", 
                        
                        yval1 = "cis_aconitic_P", 
                        ylab1 = expression(Cis*-aconitic~(nmol)),
                        
                        yval2 = "Oxalic_P", 
                        ylab2 = expression(Oxalic~(nmol)),
                        
                        yval3 = "log(trans_aconitic_P)", 
                        ylab3 = "log(Trans aconitic)\n(nmol)",
                        
                        yval4 = "log(total_P)", 
                        ylab4 = "log(Total exudate)\n(nmol)")
p <- recordPlot()

pdf(file = "Output/Figs/Hakea_plantOAdriving_factors.pdf", 
    width = 6, height = 6.5)
replayPlot(p)
dev.off()

save_png600(filename = "Output/Figs/Hakea_plantOAdriving_factors.png",
            width = 6, height = 6.5)
replayPlot(p)
dev.off()


# Eucalyptus --------------------------------------------------------------

eu_dd <- all_dd %>% 
  filter(species == "Eucalyptus") %>%
  mutate(CW = co2:water)


# > Factors driven by OAs ---------------------------------------------------


# . Rhizosphere solution --------------------------------------------------
par(mfrow = c(3, 3), mar = c(2, 4, 2, 1))
oa_R_dd <- cbind(CW = eu_dd$CW, eu_dd[, grepl("_R$", names(eu_dd))])
oa_R_dd_mlt <- melt(oa_R_dd, id = "CW")
d_ply(oa_R_dd_mlt, .(variable), 
      function(x) boxplot(value ~ CW, main = unique(x$variable), data = x))
# plot fumaric, oxalic and shikimic


create_fig_byOA_wide(data = eu_dd, showlayout = FALSE,
                     fig_title = "Eucalyptus-Rhizosphere", 
                     
                     xval1 = "fumaric_R", pred_xval1 = "fumaric_R", 
                     xlab1 = expression(Fumaric~(nmol)),
                     
                     xval2 = "Oxalic_R", pred_xval2 = "Oxalic_R",
                     xlab2 = expression(Oxalic~(nmol)),
                     
                     xval3 = "shikimic_R", pred_xval3 = "shikimic_R",
                     xlab3 = "Shikimic (nmol))",
                     
                     xval4 = "sqrt(total_R)", pred_xval4 = "total_R",
                     xlab4 = expression(sqrt(Total~exudate~(nmol))),
                     
                     yval1 = "log(total_MUB_root)",
                     ylab1 = expression(atop("log(Acid phosphatse)", 
                                             (MUB~nmol~h^'-1'~mm^'-2'))),
                     
                     yval2 = 'log(leaf_Mn)', ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                     
                     yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio", 
                     
                     yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil N:P ratio)")

p <- recordPlot()

pdf(file = "Output/Figs/Eucalyptus_by_RhizosphereOA.pdf", width = 6.5, height = 6.5)
replayPlot(p)
dev.off()

save_png600(filename = "Output/Figs/Eucalyptus_by_RhizosphereOA.png", 
            width = 6.5, height = 6.5)
replayPlot(p)
dev.off()

# . Plant solution --------------------------------------------------------
par(mfrow = c(3, 3), mar = c(2, 4, 2, 1))
oa_P_dd <- cbind(CW = eu_dd$CW, 
                 eu_dd[, grepl("_P$", names(eu_dd)) & 
                         !grepl("^leaf", names(eu_dd))])
oa_P_dd_mlt <- melt(oa_P_dd, id = "CW")
d_ply(oa_P_dd_mlt, .(variable), 
      function(x) boxplot(sqrt(value) ~ CW, main = unique(x$variable), data = x))

create_fig_byOA_wide(data = eu_dd, showlayout = FALSE,
                     fig_title = "Eucalyptus-Plant", 
                     
                     xval1 = "sqrt(fumaric_P)", pred_xval1 = "fumaric_P", 
                     xlab1 = expression(sqrt(Fumaric~(nmol))),
                     
                     xval2 = "sqrt(Oxalic_P)", pred_xval2 = "Oxalic_P",
                     xlab2 = expression(sqrt(Oxalic~(nmol))),
                     
                     xval3 = "sqrt(shikimic_P)", pred_xval3 = "shikimic_P",
                     xlab3 = expression(sqrt(Shikimic~(nmol))),
                     
                     xval4 = "sqrt(total_P)", pred_xval4 = "total_P",
                     xlab4 = expression(sqrt(Total~(nmol))),
                     
                     yval1 = "log(total_MUB_root)", 
                     ylab1 = expression(atop("log(Acid phosphatse)", 
                                             (MUB~nmol~h^'-1'~mm^'-2'))),
                     
                     yval2 = 'log(leaf_Mn)', 
                     ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                     
                     yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio", 
                     
                     yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil N:P ratio)")

p <- recordPlot()

pdf(file = "Output/Figs/Eucalyptus_by_PlantOA.pdf", width = 6.5, height = 6.5)
replayPlot(p)
dev.off()

png(filename = "Output/Figs/Eucalyptus_by_PlantOA.png", width = 6.5, height = 6.5, 
    res = 600, units = "in")
replayPlot(p)
dev.off()

# > Factors driving OAs -----------------------------------------------------

# . Rhizosphere solution --------------------------------------------------
create_fig_OAdriver_wid(data = eu_dd, showlayout = FALSE, 
                        fig_title = "Eucalyptus-Rhizosphere", 
                        
                        xval1 = "SA_DC15", pred_xval1 = "SA_DC15", 
                        xlab1 = expression(Root~surface~area~(mm^2)), 
                        
                        xval2 = "root", pred_xval2 = "root", 
                        xlab2 = "Root mass (mg)", 
                        
                        xval3 = "leaf",pred_xval3 = "leaf", 
                        xlab3 = "Leaf mass (mg)", 
                        
                        yval1 = "fumaric_R", 
                        ylab1 = expression(Fumaric~(nmol)),
                        
                        yval2 = "Oxalic_R", 
                        ylab2 = expression(Oxalic~(nmol)),
                        
                        yval3 = "shikimic_R", 
                        ylab3 = expression(Shikimic~(nmol)),
                        
                        yval4 = "sqrt(total_R)", 
                        ylab4 = expression(sqrt(Total~exudate~(nmol))))
p <- recordPlot()

pdf(file = "Output/Figs/Eucalyptus_rhizoOAdriving_factors.pdf", 
    width = 6, height = 6.5)
replayPlot(p)
dev.off()

save_png600(filename = "Output/Figs/Eucalyptus_rhizoOAdriving_factors.png",
            width = 6, height = 6.5)
replayPlot(p)
dev.off()


# . Plant solution --------------------------------------------------------

create_fig_OAdriver_wid(data = eu_dd, showlayout = FALSE, 
                        fig_title = "Eucalyptus-Plant", 
                        
                        xval1 = "SA_DC15", pred_xval1 = "SA_DC15", 
                        xlab1 = expression(Root~surface~area~(mm^2)), 
                        
                        xval2 = "root", pred_xval2 = "root", 
                        xlab2 = "Root mass (mg)", 
                        
                        xval3 = "leaf",pred_xval3 = "leaf", 
                        xlab3 = "Leaf mass (mg)", 
                        
                        yval1 = "sqrt(fumaric_P)", 
                        ylab1 = expression(sqrt(Fumaric~(nmol))),
                        
                        yval2 = "sqrt(Oxalic_P)", 
                        ylab2 = expression(sqrt(Oxalic~(nmol))),
                        
                        yval3 = "sqrt(shikimic_P)", 
                        ylab3 = expression(sqrt(Shikimic~(nmol))),
                        
                        yval4 = "sqrt(total_P)", 
                        ylab4 = expression(sqrt(Total~exudate~(nmol))))
p <- recordPlot()

pdf(file = "Output/Figs/Eucalyptus_plantOAdriving_factors.pdf", 
    width = 6, height = 6.5)
replayPlot(p)
dev.off()

save_png600(filename = "Output/Figs/Eucalyptus_plantOAdriving_factors.png",
            width = 6, height = 6.5)
replayPlot(p)
dev.off()



