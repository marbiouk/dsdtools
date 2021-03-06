setwd("~/Andy's Documents/Research projects and data/2012/AK")
all_records_beans <- read.csv("~/Andy's Documents/Research projects and data/2012/AK/all_records_beans.csv")

library("RColorBrewer")
library("beanplot")

par(family="serif")
par(mfrow=c(10,1))
par(mar=c(0,8,0,5))
beanplot(omega_calcite_crm_srtm~Species, xaxt="n", data=all_records_beans, ll=0.04, log="",what=c(0,1,0,1), ylab="Omega\ncalcite\n()",las=1)
par(mar=c(0,8,0,5))
beanplot(crm_srtm_bathy~Species, ylab="Depth (m)\n", data=all_records_beans, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(dissolved_o2_crm_srtm~Species, ylab="Dissolved oxygen\n(ml l-1)", data=all_records_beans, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(poc_crm_srtm~Species, ylab="Particulate organic\n carbon", data=all_records_beans, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(salinity_crm_srtm~Species, ylab="Salinity", data=all_records_beans, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(silicate_crm_srtm~Species, ylab="Silicate\n(mg l-1)", data=all_records_beans, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(slope_crm_srtm~Species, ylab="Slope",data=all_records_beans, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,8,0,5))
beanplot(temperature_crm_srtm~Species, ylab=expression("Temperature ("*degree*"C)"), data=all_records_beans, ll=0.04, log="",what=c(0,1,0,1),las=2)

par(family="serif")
par(mfrow=c(10,1))
par(mar=c(0,8,0,5))
beanplot(omega_aragonite_crm_srtm~Species, xaxt="n", data=all_records_beans, ll=0.04, log="",what=c(0,1,0,1), ylab="Omega\naragonite\n()",las=1)
par(mar=c(0,8,0,5))
beanplot(bpi_broad_crm_srtm~Species, ylab="BPI Broad\n", data=all_records_beans, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(bpi_fine_crm_srtm~Species, ylab="BPI Fine\n", data=all_records_beans, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(eastness_crm_srtm~Species, ylab="Eastness", data=all_records_beans, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(northness_crm_srtm~Species, ylab="Northness", data=all_records_beans, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(profile_curve_crm_srtm~Species, ylab="Profile\nCurve", data=all_records_beans, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(roughness_crm_srtm~Species, ylab="Roughness",data=all_records_beans, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,8,0,5))
beanplot(rugosity_crm_srtm~Species, ylab="Rugosity", data=all_records_beans, ll=0.04, log="",what=c(0,1,0,1),las=2)