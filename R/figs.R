all_dd <- Reduce(function(...) merge(..., by = "sample_id"),
                 list(treat_dd, biomass_dd, leaf_chem_dd, soilnut_dd, 
                      root_CumSum_dd_cst[, c("sample_id", "SA_DC15")], 
                      exudate_raw_dd_cst, phosphtase_dd))

# '-' is hard to handle when used in column names so replace
names(all_dd) <- gsub("-", "_", names(all_dd))


# Microlaena --------------------------------------------------------------

mic_dd <- all_dd %>% 
  filter(species == "Microlaena") %>%
  mutate(CW = co2:water)


# * Factors driven by OAs  ---------------------------------------------


# ** total_R --------------------------------------------------------------

pdf(file = "Output/Figs/Microlaena_by_total_R.pdf", width = 3.5, height = 6.5)
create_fig_byOA(data = mic_dd, showlayout = FALSE,
                
                xval1 = "sqrt(total_R)", pred_xval1 = "total_R", 
                xlab1 = expression(sqrt(Rhizosphere~root~exudate~(nmol))),
                
                yval1 = "total_MUB", 
                ylab1 = expression(atop("Acid phosphatse", (MUB~nmol~h^'-1'))),
                
                yval2 = 'log(leaf_Mn)', ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                
                yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio)", 
                
                yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil N:P ratio)")

dev.off()


# # ** total_P ------------------------------------------------------------
par(mfrow = c(2, 2))
boxplot(sqrt(total_P) ~ CW, data = mic_dd)

create_fig_byOA(data = mic_dd, showlayout = TRUE,
                
                xval1 = "sqrt(total_P)", pred_xval1 = "total_P", 
                xlab1 = expression(sqrt(Rhizosphere~root~exudate~(nmol))),
                
                yval1 = "total_MUB", 
                ylab1 = expression(atop("Acid phosphatse", (MUB~nmol~h^'-1'))),
                
                yval2 = 'log(leaf_Mn)', ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                
                yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio)", 
                
                yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil N:P ratio)")


# * Factors driving total_R -----------------------------------------------

par(mfrow = c(2, 2), mar = c(3, 2, 2, 1))
boxplot(sqrt(total_R) ~ CW, data = mic_dd, main = "sqrt(total_R)")
boxplot(SA_DC15 ~ CW, data = mic_dd, main = "SA_DC15")
boxplot(leaf ~ CW, data = mic_dd, main = "leaf_mass")
boxplot(root ~ CW, data = mic_dd, main = "root_mass")

pdf(file = "Output/Figs/OAdrivers_Microlaena.pdf", width = 6.5, height = 3)
create_fig_OAdriver(data = mic_dd, 
                    xval1 = "SA_DC15", xval2 = "root", xval3 = "leaf", 
                    pred_xval1 = "SA_DC15", pred_xval2 = "root", pred_xval3 = "leaf", 
                    xlab1 = expression(Root~surface~area~(mm^2)), 
                    xlab2 = "Root mass (mg)", xlab3 = "Leaf mass (mg)", 
                    yval1 = "sqrt(total_R)", 
                    ylab1 = expression(sqrt(Rhizosphere~root~exudate~(nmol))), 
                    showlayout = FALSE)
dev.off()



# Hakea -------------------------------------------------------------------

hak_dd <- all_dd %>% 
  filter(species == "Hakea") %>%
  mutate(CW = co2:water)


# * Factors driven by total_R ---------------------------------------------

pdf(file = "Output/Figs/Regression_Hakea.pdf", width = 3.5, height = 6.5)

create_fig_byOA(data = hak_dd, showlayout = FALSE,
                
                xval1 = "log(total_R)", pred_xval1 = "total_R", 
                xlab1 = expression(log(Trans_aconitic~(nmol))),
                
                yval1 = "total_MUB", 
                ylab1 = expression(atop("Acid phosphatse", (MUB~nmol~h^'-1'))),
                
                yval2 = 'log(leaf_Mn)', 
                ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                
                yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio", 
                
                yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil_NP_ratio)")

dev.off()



# * Factors driving total_R -----------------------------------------------

par(mfrow = c(2, 2), mar = c(3, 2, 2, 1))
boxplot(log(total_R) ~ CW, data = hak_dd, main = "log(total_R)")
boxplot(log(SA_DC15) ~ CW, data = hak_dd, main = "log(SA_DC15)")
boxplot(leaf ~ CW, data = hak_dd, main = "leaf_mass")
boxplot(leaf ~ CW, data = hak_dd, main = "leaf_mass")
boxplot(root ~ CW, data = hak_dd, main = "root_mass")

pdf(file = "Output/Figs/OAdrivers_Hakea.pdf", width = 6.5, height = 3)
create_fig_OAdriver(data = hak_dd, 
                    xval1 = "log(SA_DC15)", xval2 = "root", xval3 = "leaf", 
                    pred_xval1 = "SA_DC15", pred_xval2 = "root", pred_xval3 = "leaf", 
                    xlab1 = expression(log(Root~surface~area~(mm^2))), 
                    xlab2 = "Root mass (mg)", xlab3 = "Leaf mass (mg)", 
                    yval1 = "log(total_R)", 
                    ylab1 = expression(log(Rhizosphere~root~exudate~(nmol))), 
                    showlayout = FALSE)
dev.off()


# Eucalyptus --------------------------------------------------------------

eu_dd <- all_dd %>% 
  filter(species == "Eucalyptus") %>%
  mutate(CW = co2:water)


# * Factors driven by total_R ---------------------------------------------


pdf(file = "Output/Figs/Regression_Eucalyptus.pdf", width = 3.5, height = 6.5)

create_fig_byOA(data = eu_dd, showlayout = FALSE,
                
                xval1 = "sqrt(total_R)", pred_xval1 = "total_R", 
                xlab1 = expression(sqrt(Rhizosphere~root~exudate~(nmol))),
                
                yval1 = "total_MUB", 
                ylab1 = expression(atop("Acid phosphatse", (MUB~nmol~h^'-1'))),
                
                yval2 = 'log(leaf_Mn)', ylab2 = expression(log(Leaf~Mn~(mg~kg^'-1'))),
                
                yval3 = "leaf_NP_ratio", ylab3 = "Leaf N:P ratio", 
                
                yval4 = "log(Soil_NP_ratio)", ylab4 = "log(Soil N:P ratio)")

dev.off()


# * Factors driving total_R -----------------------------------------------

par(mfrow = c(2, 2), mar = c(3, 2, 2, 1))
boxplot(sqrt(total_R) ~ CW, data = eu_dd, main = "sqrt(total_R)")
boxplot(SA_DC15 ~ CW, data = eu_dd, main = "SA_DC15")
boxplot(leaf ~ CW, data = eu_dd, main = "leaf_mass")
boxplot(root ~ CW, data = eu_dd, main = "root_mass")

pdf(file = "Output/Figs/OAdrivers_Eucalyptus.pdf", width = 6.5, height = 3)
create_fig_OAdriver(data = eu_dd, 
                    xval1 = "SA_DC15", xval2 = "root", xval3 = "leaf", 
                    pred_xval1 = "SA_DC15", pred_xval2 = "root", pred_xval3 = "leaf", 
                    xlab1 = expression(Root~surface~area~(mm^2)), 
                    xlab2 = "Root mass (mg)", xlab3 = "Leaf mass (mg)", 
                    yval1 = "sqrt(total_R)", 
                    ylab1 = expression(sqrt(Rhizosphere~root~exudate~(nmol))), 
                    showlayout = FALSE)
dev.off()

