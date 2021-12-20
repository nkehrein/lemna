##
## A mechanistic combined toxicokinetic–toxicodynamic (TK/TD) and growth
## model for the aquatic macrophytes Lemna spp. as published by
## Schmitt et al. (2013), DOI: 10.1016/j.ecolmodel.2013.01.017
##
## Parts of the implementation were commented out for reasons of brevity.
##

#library(deSolve)       # Library with ODE-solver
#library(FME)

# This script contains the following functions:

#   calcgrowth(timepoints, vars, param, filenames, graph)   -  To be called for simulation run
#   growthmodel(timepoints, vars, param)                    -  Calls ODE-solver (Called by calcgrowth)

#   growth_ode(t,vars,param)                                -  M O D E L - E Q U A T I O N S !!!!!
#     Supporting functions:
#     P_T(P_up,MolWeight, actTemp)     Temperature dependence of permeability
#     f_R(Rad,k_0,a_k)                 -|
#     f_T(Temp,Topt,Tmax,Tmin)         -|
#     f_T_resp(Temp,t_ref,Q10)         -|
#     f_P(C_P, CP50, a_P, KiP)         -|--  Functions to calculate dependence factors for growth rate
#     f_N(C_N, CN50, a_N, KiN)         -|
#     f_BM(BM, BM50)                   -|
#     f_E(b,EC50,Emax,C_active)        -|
#
#   prepare_param(param,filenames)                           - Loads tables from filenames and includes them in parameter list
#   modeval(result,eff_frac,t_recov)                         -  Calculates endpoint on population level from results

#  For Calculation:     calcgrowth(timepoints, vars, param, filenames,graph=T)


#------- Main function for simulation run --------------------------------------
calcgrowth <- function(timepoints, vars, param, filenames,graph,...)
# !!! CALCGROWTH allways calculates control and treated population !!!
#
# Input:
#  - timepoints: vector with timepoints for output
#  - vars      : named vector with variables of the model
#  - param     : list with parameter values
#  - filenames : vector with names (incl. path) to fiels to be loaded
#  - graph     : logical T/F with/without graphical output
#
# Output:
#  - Dataframe with columes: Time, Dry Biomass Control, Dry Biomass treated, Effect(%),
#                           FrondNo Contro, FrondNo treated, int. Concentration, int. Effect
#
{
# Are data provided as tables? If yes read files and replace parameter value by table
#filenames<-filenames[filenames!='']
#if (length(filenames)!=0) {
#   param <- prepare_param(param, filenames)
#   }

# simulate with effect
result <- growthmodel(timepoints=timepoints, vars=vars,param=param,...)
result$E <- NULL
result$FrondNo <- result[,"BM"]/param$mass_per_frond
# control simulation
#param_cont <- param
#param_cont$Conc <- 0    # Prepare control simulation
#result_cont <- growthmodel(timepoints=timepoints, vars=vars,param=param_cont)

#out<-cbind(   Time          =result[,"time"],
#              Control       =result_cont[,"BM"],
#              Treated       =result[,"BM"],
#              Effect        =(result_cont[,"BM"]-result[,"BM"])/result_cont[,"BM"]*100,
#              FrondNo_Cont  =result_cont[,"BM"]/param$mass_per_frond,
#              FrondNo_Treat =result[,"BM"]/param$mass_per_frond,
#              M_int         =result[,"M_int"],
#              C_int         =result[,"C_int"],
#              Eff_int       =result[,"E"]
#              )

# prepare plot if wanted
#if(graph){
#  windows()
#  plot(out[,'Time'],out[,'Control'],type = "l",xlab='Time [d]',ylab='Biomass [g_dw/m?')
#  lines(out[,'Time'],out[,'Treated'],type = "l",col="red")
#  if (!is.null(param$obsdat)) {points(param$obsdat)}
#  }

return(result)
}

#------ Function that calls ODE-solver -----------------------------------------
growthmodel <- function(timepoints, vars, param, hmax=.1) {
with(as.list(c(vars,param)),{

#plot(0,0,xlim=c(0,max(timepoints)),ylim=c(0,200))

# Solve ODE
out <- as.data.frame(deSolve::ode(y = vars, times = timepoints, func = growth_ode, parms = param, hmax=hmax))
#colnames(out)[4] <- 'C_int'

# Conversion: internal mass -> internal concentration
out[,'C_int'] <- out[,'M_int']/(out[,'BM']*param$BMw2BMd)

return(out)
})
}

# ************  M O D E L - E Q U A T I O N S **********************************
# ODE System and related equations
growth_ode <- function(t,vars,param){
# vars (= Variables of the model) contains:
# BM        [g/m?]      Biomass (is converted into No of individuals at the end
# E         (Toxic)     Effect = Factor on growth rate  (Range: 0 - 1)
# M_int     [?g]        Mass of toxicant in biomass


# Parameters and their units:

#      - Fate of biomass -
#k_phot_fix  T/F         If True k_phot_max is not changed by environmental factors
#k_phot_max  [1/d]       Photosynthesis rate
#k_resp   [1/d]       Respiration rate
#k_loss   [1/d]       Some rate of loss (e.g. Flow rate)
#
#      - Effect -
#Conc     [any]      Concentration of toxicant (may also be a table)
#TDMod    ['direct' or 'delayed'] Kind of model to be used
#Emax     [-]         maximum Effect
#EC50     [same as conc. data]      Midpoint of effect curve
#b        [-]         Slope of effect curve
#k_E_in   [1/d]       Rate of effect effect increase (delayed TD model)
#k_E_out  [1/d]       Rate of effect effect decline
#
#       - Toxicokinetics -
#P_up     [cm/d]      Permeability for uptake
#AperBM   [cm?/g_dw]  A_leaf / d_leaf = 1/d_leaf (for circular disc, d=0.05 cm)
#Kbm      []          Biomass(fw):water partition coefficient
#P_Temp   T/F         Switch for temperature dependence of cuticle permeability
#MolWeight [g/mol]    Molmass of molecule (determines Q10_permeability)
#
#      - Others -
#mass_per_frond     [g_dw/frond]  Dryweight per frond
#BMw2BMd            [g_fresh/g_dry]  Fresh- / dryweight
#
#      - Temperatur dependence -
# k_photrowth  (!!values depend on temp. dependence of k_resp!!)
#Temp     [?C]        Current temperature (may also be a table)
#Tmin     [?C]        Minimum growth temperature
#Tmax     [?C]        Maximum growth temperature
#Topt     [?C]        Optimum growth temperature
#
# k_resp
#t_ref     Temperature at which k_resp is effective
#Q10
#
#
#      - Light dependence (linear dependence on global radiation (see Hodgeson 1969)
#Rad 	    [kJ/m?/d]  Radiation  (may also be given as table)
#k_0     	[1/d]      Intercept of linear part
#a_k     	[(1/d)/(kJ/m?/d)]        Slope of linear part
#
#
#      - Phosphorus dependence (Hill like dependence) -
#C_P      [mg/L]       Phosporus concentration in water
#CP50     [mg/L]       P-conc. where growth rate is halfened   [Ref. F 0191, data re-evaluated with Hill-Model]
#a_P      []            Hill coefficient                       [Ref. F 0191, data re-evaluated with Hill-Model]
#KiP      [mg/L]       P-inhibition constant for very high P-conc.   [Ref. F 0191]
#
#      - Nitrogen dependence (Hill like dependence) -
#C_N      [mg/L]       Nitrogen concentration in water
#CN50     [mg/L]       N-conc. where growth rate is halfened   [Ref. F 0191, data re-evaluated with Hill-Model]
#a_N      []            Hill coefficient                       [Ref. F 0191, data re-evaluated with Hill-Model]
#KiN      [mg/L]       n-inhibition constant for very high P-conc.   [Ref. F 0191]
#
#      - Density dependence -
#BM50     [g_dw/m?]             Cut off BM   [Ref. F 0191]
#

# Default values
TDMod = 'direct'
k_E_in=100
k_E_out=100

with(as.list(c(vars,param)),{
# Determine actual values at timepoints t from tables
if(!is.null(dim(Conc))) {
  actConc <- approx(Conc,y=NULL,t,rule=2)     # Linear interpolation if is table
  actConc <- actConc$y
}
else
  {actConc <- Conc}

if(!is.null(dim(Temp))) {
  actTemp <- approx(Temp,y=NULL,t,rule=2)     # Linear interpolation if is table
  actTemp <- actTemp$y }
else
  {actTemp <- Temp}

if(!is.null(dim(Rad))) {
  actRad <- approx(Rad,y=NULL,t,rule=2)     # Linear interpolation if is table
  actRad <- actRad$y }
else {
  if(Rad>0){actRad <- Rad}
  else{actRad <- calcRad(t,Lat)}
  }

#points(t,BM)

# Calculate internal toxicant concentrations from amount in biomass
BM_fresh <- BM*BMw2BMd
C_int <- M_int/BM_fresh
C_int_u <- abs(C_int/Kbm)   # Unbound internal concentration


# Calculate effective growth rate
if(!k_phot_fix){
  k_phot_eff <- k_phot_max * f_R(actRad, k_0, a_k)
  k_phot_eff <- k_phot_eff * f_T(actTemp,Topt,Tmax,Tmin)
  k_phot_eff <- k_phot_eff * f_P(C_P, CP50, a_P, KiP) * f_N(C_N, CN50, a_N, KiN)
  k_phot_eff <- k_phot_eff * f_BM(BM, BM50)
  k_resp_eff <- k_resp*f_T_resp(actTemp,t_ref,Q10)
}else{
  k_phot_eff <- k_phot_max
  k_resp_eff <- k_resp
}

# Consider toxic effect
f_Eff <- f_E(b,EC50,Emax,C_int_u)
if(TDMod=='delayed'){f_Eff <- E}  # delayed effects could be considered
k_phot_eff <- k_phot_eff*f_Eff

# Biomass
dBMdt <- BM*(k_phot_eff-k_resp_eff-k_loss)
# let population extinct if less than one frond/m?
if(BM<5*mass_per_frond){dBMdt <- 0}

# Effect  (this is the delayed TD model)
dEdt <- k_E_in*f_E(b,EC50,Emax,C_int_u) - k_E_out * E

# TK part: Internal amount of toxicant
P_up_eff = P_up
if(P_Temp){P_up_eff = P_T(P_up,MolWeight, actTemp)}   # Temperature dependence of permeability
dM_intdt <- P_up_eff*AperBM*BM*(actConc-C_int_u) - C_int*BM_fresh*(k_resp_eff+k_loss)

return(list(c(dBMdt, dEdt, dM_intdt)))
})
}


# Calculation of temperature dependent permeability
P_T <- function(P_up,MolWeight, actTemp)
{
Eact <- 0.307*MolWeight/1.4+95          # Activation energy
Q10perm <- exp(10*Eact/0.008314/300^2)   # see Baur Publication 7
return(P_up * Q10perm^((actTemp-20)/10))
}


# Functions for calculating reduction factors for growth rate

# Light
f_R <- function(actRad, k_0, a_k)
{
photfac <- a_k*actRad + k_0
if(photfac >1){photfac=1}
return(photfac)
}

# Temperature
# effect on k_phot
f_T <- function(actTemp,Topt,Tmax,Tmin)
{
Tx<-Tmax
if(actTemp<=Topt){Tx<-Tmin}
return(exp(-2.3*((actTemp-Topt)/(Tx-Topt))^2))
}
# effect on k_resp
f_T_resp <- function(actTemp,t_ref,Q10)
{
Q10^((actTemp-t_ref)/10)
}

# Phosphorus
f_P <- function(C_P, CP50, a_P, KiP)
{
C_P^a_P/(C_P^a_P + CP50^a_P) * KiP/(KiP + C_P)
}

# Nitrogen
f_N <- function(C_N, CN50, a_N, KiN)
{
C_N^a_N/(C_N^a_N + CN50^a_N)* KiN/(KiN + C_N)
}

# Biomass (crowding)
f_BM <- function(BM, BM50)
{
fact<-(BM50 - BM)/BM50
return(fact)
}

# Effect
f_E <- function(b,EC50,Emax,C_active)
{
1-Emax*C_active^b/(EC50^b+C_active^b)
}

#******************************************************************************




#------- Function for reading input files --------------------------------------
# prepare_param <- function(param,filenames)
# # Reads files with input information and add respective tables to parameters
# {
# nam <- names(filenames)
# for (i in (1:NROW(nam))) {
#   nam_i <- nam[i]
#   content <- read.delim(filenames[i], header=T)
#   if(sum(nam_i=='Data_file')>0) {param$obsdat <- content}
#     else {param$obsdat = NULL}
#   if(sum(nam_i=='Conc_file')>0) {param$Conc <- content}
#   if(sum(nam_i=='Temperature_file')>0) {param$Temp <- content}
#   if(sum(nam_i=='Irradiation_file')>0) {param$Rad <- content}
#   }
# return(param)
# }


#------- Evaluation of simulation results --------------------------------------
#modeval <- function(result,control,eff_frac,t_recov){
# Calculates: Maximum Effect, Period of Effect (Time to Recovery) and Effect remaining after t_recov
# Input:
#   result:   Biomass treated
#   control:  Biomass control
#   eff_frac: Maximum relativ deviation from control that is NO effect
#   t_recov:  Time after onset of effect at which remaining effect is determined
#
# Output:
#   MaxEff:     Maximum Effect (%control)
#   E_trecov:   Effect remaining at t_recov after onset
#   t_recov:    Period of Effect (time between onset and disappearence)
#   AuEffCurve: Area under effect curve
#
#
# n_times <- NROW(result)
#
# calcEff <- (control[,'BM']-result[,'BM'])/control[,'BM'] # effect as fraction
#
# # Initialisation of variables
# maxeff_tot<-0
# maxeff_rec<-0
# maxeff<-0
# maxeff_tot_r<-0
# maxeff_rec_r<-0
# maxeff_r<-0
# teff_max<-0
# teff_max_tot<-0
# teff_max_r<-0
# teff_max_tot_r<-0
# AuEC <- 0
# AuEC_tot <- 0
#
# # Evaluation on basis of accumulated values
# t1<-0
# t2<-0
# t1_r<-0
# t2_r<-0
# for (i in (1:n_times)) {
# # do evaluation for biomass
#    if(calcEff[i]>eff_frac){
#       if(t1==0){t1<-result[i,'time']}
#       t2<-result[i,'time']
#       if(calcEff[i]>maxeff){maxeff<-calcEff[i]}    # determin Max. Effect
#       if((t2-t1)>t_recov && calcEff[i]>maxeff_rec){maxeff_rec<-calcEff[i]}
#       if((t2-t1)>teff_max){teff_max<-(t2-t1)}    # determin period of effect
#       AuEC <- AuEC+(calcEff[i]+calcEff[i-1])/(2*(result[i,'time']-result[i-1,'time']))  # determine area under effect curve
#       }
#    else{
#       if(maxeff>maxeff_tot){maxeff_tot<-maxeff}
#       if(teff_max>teff_max_tot){teff_max_tot<-teff_max}
#       if(AuEC>AuEC_tot){AuEC_tot<-AuEC}
#       maxeff<-0
#       teff_max<-0
#       t1<-0
#       t2<-0
#       }
#   }
#
# # repeat this in order to capture effects at end of calculation period
# if(t2>0 && calcEff[i]>maxeff_rec){maxeff_rec<-calcEff[i]}
# if(maxeff>maxeff_tot){maxeff_tot<-maxeff}
#
#
# result <- c(
# MaxEff        =  maxeff_tot*100, # Maximum Effect (%control)
# E_trecov      =  maxeff_rec,     # Effect remaining at t_recov after onset
# t_recov       =  teff_max_tot,   # Period of Effect (time between onset and disappearence)
# AuEffCurve    =  AuEC_tot)       # Area under effect curve
#
# return(result)
# }
