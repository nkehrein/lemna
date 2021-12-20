##
## A mechanistic combined toxicokinetic–toxicodynamic (TK/TD) and growth
## model for the aquatic macrophytes Lemna spp. as published by
## Schmitt et al. (2013), DOI: 10.1016/j.ecolmodel.2013.01.017
##
## Parts of the implementation were commented out for reasons of brevity.
##

#
# options(warn=-1)
#
# #********** Control of calculation *********************************************
# # Simulation name  (will be used for naming output files !!!)
# Simname = 'Test'
#
# # Working directory
# workdir = 'C:/Users/zfwsm/Documents/4_R Projects/Lemna Model/Growth model_official'
#
# # F i l e s to be loaded
# # Name ='' for unused files
# Conc_file = 'Peak_2d@t=0.txt'
# Temperature_file = ''
# Irradiation_file = ''
# ##Data_file = 'Data.txt',
#
# # Save results after execution?
# fileout = F       # T=true/F=false
#
# #---- Settings of simulation ---------------------------------------------------
# # Define method to be used
# callfun = 'CS'   # CS = single simulation, DR = Dose response, MC = MC Simulation
# # Set  timepoints for output
# outtimes = seq(0,100,1)
# # DOSE RESPONSE: Define exposure steps/factors (only used for "Dose Response")
# expfact = 10^seq(0,1.5,0.25)
# # MC SIMULATION: Define list of parameters to vary and No of replicates
# MCrep = 100
# parlist = list(
#     parnames=c(
#       'k_G_max',
#       'k_mort',
# #      'Emax',
#       'EC50'),
# #      'b',
# #      'BM50',
# #      'P_up'),
#     pardis=list(
#     c(0.47,0.2,1),
#     c(0.05,0.1,1),
# #    c(1,0,1),
#     c(.48,.1,2))
# #    c(6.4,.1,1),
# #    c(176,.1,1),
# #    c(.3,.2,2))
# )


# Set initial values
vars <- c(
BM       = 50,     # [g_dw/m?]   Dry Biomass dryweight per m2
E        = 1,      # (0-1)      (Toxic) Effect = Factor on growth rate  (Range: 0 - 1, 1=no effect)
M_int    = 0       # [?g]       Amount of toxicant in biomass
)

#---- Parameter Setting --------------------------------------------------------
param <- list(
#
#      - Effect -
Conc     = 1,   #  [any]      Concentration of toxicant (may also be a table)
Emax     = .784,    #   maximum Effect
EC50     = 0.3,     #  [same as conc. data]      Midpoint of effect curve
b        = 4.16,    #  [-]         Slope of effect curve
#
#       - Toxicokinetics -
P_up     = .0054,   # [cm/d]      Permeability for uptake
AperBM   = 1000,   # [cm?/g_dw]   A_leaf / d_leaf = 1/d_leaf (for circular disc, d=0.05 cm) [Ref. HARLAN-022]
Kbm      = .75,   # []          Biomass(fw):water partition coefficient
P_Temp = F,       # Switch for temperature dependence of cuticle permeability
MolWeight = 381,  # Molmass of molecule (determines Q10_permeability)



#       - Fate of biomass -
k_phot_fix  = F,   #  T/F          If True k_G_max is not changed by environmental factors
k_phot_max  = 0.47, #  [1/d]       Maximum growth rate of biomass + kmort     [Ref. F 0191, Harlan-011]
k_resp   = 0.05, #  [1/d]       Rate of mortality                 [Ref. Harlan-011, rough estimate]
k_loss   = 0.0, #  [1/d]       Some rate of loss (e.g. Flow rate)
#
#      - Temperatur dependence -
# k_phot
Temp     = 12,   #  [?C]        Current temperature (may also be a table)
Tmin     = 8.0  , #  [?C]        Minimum growth temperature      [Ref. F 0191, data re-evaluated  incl. kmort(T)]
Tmax     = 40.5 , #  [?C]        Maximum growth temperature      [Ref. F 0191, data re-evaluated  incl. kmort(T)]
Topt     = 26.7 , #  [?C]        Optimum growth temperature      [Ref. F 0191, data re-evaluated  incl. kmort(T)]
# k_resp
t_ref       = 25,   # temperature at which t_mort is effective
Q10         = 2,
#
#      - Light dependence (linear dependence on global radiation (see Hodgeson 1969)
Rad 	    = 15000 , #  [kJ/m?/d]  Radiation  (may also be given as table)
k_0     	= 3 ,     #  [1/d]      Intercept of linear part
a_k     	= 5E-5 ,  #  [(1/d)/(kJ/m?/d)]        Slope of linear part

#      - Phosphorus dependence (Hill like dependence) -
C_P      = 0.3,          #   [mg/L]       Phosporus concentration in water
CP50     = 0.0043,        #   [mg/L]       P-conc. where growth rate is halfened   [Data from L??nd, 1983 evaluated with monod model]
a_P      = 1,        #   []            Hill coefficient
KiP      = 101,         #   [mg/L]       P-inhibition constant for very high P-conc.   [Ref. F 0191]

#      - Nitrogen dependence (Hill like dependence) -
C_N      = 0.6,         #   [mg/L]       Nitrogen concentration in water
CN50     = 0.034,          #   [mg/L]       N-conc. where growth rate is halfened   [Data from L??nd, 1983 evaluated with monod model]
a_N      = 1,        #   []            Hill coefficient
KiN      = 604,         #   [mg/L]       n-inhibition constant for very high P-conc.   [Ref. F 0191]

#      - Density dependence -
BM50     = 176,    #  [g_dw/m?]             Cut off BM   [Ref. F 0191]

#      - Others -
mass_per_frond     = 0.0001,   #  [g_dw/frond]  Dryweight per frond [Ref. HARLAN-022]
BMw2BMd     = 16.7   #  [g_fresh/g_dry]  Fresh- / dryweight [Ref. F 0191]

)



# **************** Call of scripts ********************************************
# +++++++  Do not change the following !!!! +++++++++++++++++++++++++++++++++++

# if (Conc_file!=''){Conc_file = paste(workdir,'/',Conc_file,sep="")}
# if (Temperature_file!=''){Temperature_file = paste(workdir,'/',Temperature_file,sep="")}
# if (Irradiation_file!=''){Irradiation_file = paste(workdir,'/',Irradiation_file,sep="")}
# #Data_file = 'Data.txt',
# filenames = c(
# Conc_file=Conc_file,
# Temperature_file=Temperature_file,
# Irradiation_file=Irradiation_file
# )
#
# # Function to read source files (needs to be defined before first call)
# sourceDir <- function(path, trace = TRUE, ...) {
#    for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
#       if(trace) cat(nm,":")
#       source(file.path(path, nm), ...)
#       if(trace) cat("\n")
#     }
#  }
#
# # Read source files
# sourceDir(paste(workdir,'/SourceFun',sep=""))
#
# if (callfun == 'CS'){
# # Call: calcgrowth(timepoints, vars, param, filenames,graph=T)
#   res <- calcgrowth(outtimes, vars, param, filenames,graph=T)
#   if(fileout){write.table(res,paste(Simname,'_out.dat',sep=''))}
# }
#
# if (callfun=='DR'){
# # Call:  dos_resp('growthmodel',doses,eff_frac,t_recov,timepoints,vars,param,filenames,graph=T)
#    dosres <- dos_resp('growthmodel',expfact,0.01,5,outtimes,vars,param,filenames,graph=F)
#    res <- dosres$Popcurves
#
#    if(fileout){
#       write.table(dosres$Popcurves,paste(outname,'_data.dat',sep=""))
#       write.table(dosres$Concentrations,paste(outname,'_conc.dat',sep=""))
#       write.table(dosres$Effects,paste(outname,'_eval.dat',sep=""))
#       }
# }
#
# if(callfun=='MC'){
# # Call:    MCsim(model,parlist,MCrep,simperiod,vars,param,filenames,graph,eff_frac,t_recov)
#     MCres <- MCsim('growthmodel',parlist,MCrep,outtimes,vars,param,filenames,graph=F,0.01,28)
# }
