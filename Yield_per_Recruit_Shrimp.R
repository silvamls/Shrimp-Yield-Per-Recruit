#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x##x#x#x
# R code for shrimp yield optimization
# Yield per recruit estimates (Beverton & Holt)
# Isopleth graphics using TropfishR package
# Author: Matheus Lourenco; Silva, MLS
#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x##x#x#x

#cleaning workspace---------
rm(list=ls())
#dev.off()

#packages *just one required
#install.packages("TropFishR")
library(TropFishR)

#reading data set of life history parameters-- Set the path where the worksheet is
setwd("C:/Matheus/Universidade/Mestrado/artigos para avaliação (YR)")

data<-read.csv("Population_parameters.csv",
               sep=";", dec=",", stringsAsFactors = F,encoding = 'latin1', header = TRUE)
#----------------------------------------------------------------------------------------------

#Life history parameters

#----------------------------- Pink parameters -------------------------------------#
#  F.subtilis N #
Winf_fs_n<-c(na.omit(data$Winf.g.[data$Specie=="F.subtilis" & data$Region=="N"]))
Linf_fs_n<-c(na.omit(data$Linf.mm.TL[data$Specie=="F.subtilis" & data$Region=="N"]))/10
K_fs_n<-c(na.omit(data$K.ano.[data$Specie=="F.subtilis" & data$Region=="N"]))
L50_fs_n<-c(na.omit(data$L50.mm.TL[data$Specie=="F.subtilis" & data$Region=="N"]))/10
Lr_fs_n<-c(na.omit(data$Lr.mm.TL[data$Specie=="F.subtilis" & data$Region=="N"]))/10
Lc_fs_n<-c(na.omit(data$Lc.mm.TL [data$Specie=="F.subtilis" & data$Region=="N"]))/10
M_fs_n<-c(na.omit(data$M[data$Specie=="F.subtilis" & data$Region=="N"]))
F_fs_n<-c(na.omit(data$F[data$Specie=="F.subtilis" & data$Region=="N"]))
T0_fs_n<-c(na.omit(data$T0[data$Specie=="F.subtilis" & data$Region=="N"]))
Tmax_fs_n<-c(na.omit(data$Tmax[data$Specie=="F.subtilis" & data$Region=="N"]))

#  F.subtilis NE #
Winf_fs_ne<-c(na.omit(data$Winf.g.[data$Specie=="F.subtilis" & data$Region=="NE"]))
Linf_fs_ne<-c(na.omit(data$Linf.mm.TL[data$Specie=="F.subtilis" & data$Region=="NE"]))/10
K_fs_ne<-c(na.omit(data$K.ano.[data$Specie=="F.subtilis" & data$Region=="NE"]))
L50_fs_ne<-c(na.omit(data$L50.mm.TL[data$Specie=="F.subtilis" & data$Region=="NE"]))/10
Lr_fs_ne<-c(na.omit(data$Lr.mm.TL[data$Specie=="F.subtilis" & data$Region=="NE"]))/10
Lc_fs_ne<-c(na.omit(data$Lc.mm.TL [data$Specie=="F.subtilis" & data$Region=="NE"]))/10
M_fs_ne<-c(na.omit(data$M[data$Specie=="F.subtilis" & data$Region=="NE"]))
F_fs_ne<-c(na.omit(data$F[data$Specie=="F.subtilis" & data$Region=="NE"]))
T0_fs_ne<-c(na.omit(data$T0[data$Specie=="F.subtilis" & data$Region=="NE"]))
Tmax_fs_ne<-c(na.omit(data$Tmax[data$Specie=="F.subtilis" & data$Region=="NE"]))

#  F.paulensis SE #
Winf_fp_se<-c(na.omit(data$Winf.g.[data$Specie=="F.paulensis" & data$Region=="SE"]))
Linf_fp_se<-c(na.omit(data$Linf.mm.TL[data$Specie=="F.paulensis" & data$Region=="SE"]))/10
K_fp_se<-c(na.omit(data$K.ano.[data$Specie=="F.paulensis" & data$Region=="SE"]))
L50_fp_se<-c(na.omit(data$L50.mm.TL[data$Specie=="F.paulensis" & data$Region=="SE"]))/10
Lr_fp_se<-c(na.omit(data$Lr.mm.TL[data$Specie=="F.paulensis" & data$Region=="SE"]))/10
Lc_fp_se<-c(na.omit(data$Lc.mm.TL [data$Specie=="F.paulensis" & data$Region=="SE"]))/10
M_fp_se<-c(na.omit(data$M[data$Specie=="F.paulensis" & data$Region=="SE"]))
F_fp_se<-c(na.omit(data$F[data$Specie=="F.paulensis" & data$Region=="SE"]))
T0_fp_se<-c(na.omit(data$T0[data$Specie=="F.paulensis" & data$Region=="SE"]))
Tmax_fp_se<-c(na.omit(data$Tmax[data$Specie=="F.paulensis" & data$Region=="SE"]))

#  F.paulensis S #
Winf_fp_s<-c(na.omit(data$Winf.g.[data$Specie=="F.paulensis" & data$Region=="S" ]))
Linf_fp_s<-c(na.omit(data$Linf.mm.TL[data$Specie=="F.paulensis" & data$Region=="S" ]))/10
K_fp_s<-c(na.omit(data$K.ano.[data$Specie=="F.paulensis" & data$Region=="S" ]))
L50_fp_s<-c(na.omit(data$L50.mm.TL[data$Specie=="F.paulensis" & data$Region=="S" ]))/10
Lr_fp_s<-c(na.omit(data$Lr.mm.TL[data$Specie=="F.paulensis" & data$Region=="S" ]))/10
Lc_fp_s<-c(na.omit(data$Lc.mm.TL [data$Specie=="F.paulensis" & data$Region=="S" ]))/10
M_fp_s<-c(na.omit(data$M[data$Specie=="F.paulensis" & data$Region=="S" ]))
F_fp_s<-c(na.omit(data$F[data$Specie=="F.paulensis" & data$Region=="S" ]))
T0_fp_s<-c(na.omit(data$T0[data$Specie=="F.paulensis" & data$Region=="S" ]))
Tmax_fp_s<-c(na.omit(data$Tmax[data$Specie=="F.paulensis" & data$Region=="S"]))

# F.brasiliensis SE #
Linf_fb_se<-c(na.omit(data$Linf.mm.TL[data$Specie=="F.brasiliensis" & data$Region=="SE" ]))/10
Winf_fb_se<-c(na.omit(data$Winf.g.[data$Specie=="F.brasiliensis" & data$Region=="SE" ]))
K_fb_se<-c(na.omit(data$K.ano.[data$Specie=="F.brasiliensis" & data$Region=="SE" ]))
L50_fb_se<-c(na.omit(data$L50.mm.TL[data$Specie=="F.brasiliensis" & data$Region=="SE" ]))/10
Lr_fb_se<-c(na.omit(data$Lr.mm.TL[data$Specie=="F.brasiliensis" & data$Region=="SE" ]))/10
Lc_fb_se<-c(na.omit(data$Lc.mm.TL [data$Specie=="F.brasiliensis" & data$Region=="SE" ]))/10
M_fb_se<-c(na.omit(data$M[data$Specie=="F.brasiliensis" & data$Region=="SE" ]))
F_fb_se<-c(na.omit(data$F[data$Specie=="F.brasiliensis" & data$Region=="SE" ]))
T0_fb_se<-c(na.omit(data$T0[data$Specie=="F.brasiliensis" & data$Region=="SE" ]))
Tmax_fb_se<-c(na.omit(data$Tmax[data$Specie=="F.brasiliensis" & data$Region=="SE"]))
#-----------------------------------------------------------------------


#--------------------------- White parameters ----------------------------------#

# litopenaeus schmitti N  #
Winf_ls_n<-c((na.omit(data$Winf.g.[data$Specie=="L.schmitti" & data$Region=="N"])))
Linf_ls_n<-c((na.omit(data$Linf.mm.TL[data$Specie=="L.schmitti" & data$Region=="N"])))/10
K_ls_n<-c((na.omit(data$K.ano.[data$Specie=="L.schmitti"& data$Region=="N"])))
L50_ls_n<-c((na.omit(data$L50.mm.TL[data$Specie=="L.schmitti"& data$Region=="N"])))/10
Lr_ls_n<-c((na.omit(data$Lr.mm.TL [data$Specie=="L.schmitti"& data$Region=="N"])))/10
Lc_ls_n<-c((na.omit(data$Lc.mm.TL [data$Specie=="L.schmitti"& data$Region=="N"])))/10
M_ls_n<-2.18 #(Natural Mortality Tool| Long=2.3,Linf=12.21,k=1.3,Winf=25.9)
F_ls_n<-c((na.omit(data$F[data$Specie=="L.schmitti" & data$Region=="N"])))
T0_ls_n<-c((na.omit(data$T0[data$Specie=="L.schmitti"& data$Region=="N"])))
Tmax_ls_n<-c((na.omit(data$Tmax[data$Specie=="L.schmitti" & data$Region!=" "])))


# litopenaeus schmitti NE #
Winf_ls_ne<-c((na.omit(data$Winf.g.[data$Specie=="L.schmitti" & data$Region=="NE"])))
Linf_ls_ne<-c((na.omit(data$Linf.mm.TL[data$Specie=="L.schmitti" & data$Region=="NE"])))/10
K_ls_ne<-c((na.omit(data$K.ano.[data$Specie=="L.schmitti"& data$Region=="NE"])))
L50_ls_ne<-c((na.omit(data$L50.mm.TL[data$Specie=="L.schmitti"& data$Region=="NE"])))/10
Lr_ls_ne<-c((na.omit(data$Lr.mm.TL [data$Specie=="L.schmitti"& data$Region=="NE" & data$Region!="N"])))/10
Lc_ls_ne<-c((na.omit(data$Lc.mm.TL [data$Specie=="L.schmitti"& data$Region=="NE"])))/10
M_ls_ne<-c((na.omit(data$M[data$Specie=="L.schmitti" & data$Region=="NE"])))
F_ls_ne<-c((na.omit(data$F[data$Specie=="L.schmitti" & data$Region=="NE"])))
T0_ls_ne<-c((na.omit(data$T0[data$Specie=="L.schmitti"& data$Region=="NE"])))
Tmax_ls_ne<-c((na.omit(data$Tmax[data$Specie=="L.schmitti" & data$Region=="NE"])))

# litopenaeus schmitti SE #
Winf_ls_se<-c((na.omit(data$Winf.g.[data$Specie=="L.schmitti" & data$Region=="SE"])))
Linf_ls_se<-c((na.omit(data$Linf.mm.TL[data$Specie=="L.schmitti" & data$Region=="SE"])))/10
K_ls_se<-c((na.omit(data$K.ano.[data$Specie=="L.schmitti"& data$Region=="SE"])))
L50_ls_se<-c((na.omit(data$L50.mm.TL[data$Specie=="L.schmitti"& data$Region=="SE"])))/10
Lr_ls_se<-c((na.omit(data$Lr.mm.TL [data$Specie=="L.schmitti"& data$Region=="SE" & data$Region!="N"])))/10
Lc_ls_se<-c((na.omit(data$Lc.mm.TL [data$Specie=="L.schmitti"& data$Region=="SE"])))/10
M_ls_se<-c((na.omit(data$M[data$Specie=="L.schmitti" & data$Region=="SE"])))
F_ls_se<-c((na.omit(data$F[data$Specie=="L.schmitti" & data$Region=="SE"])))
T0_ls_se<-c((na.omit(data$T0[data$Specie=="L.schmitti"& data$Region=="SE"])))
Tmax_ls_se<-c((na.omit(data$Tmax[data$Specie=="L.schmitti" & data$Region=="SE"])))
#--------------------------------------------------------


#-------------------------- Seabob parameters --------------------------------#

# X.kroyeri N #
Winf_xk_n<-c((na.omit(data$Winf.g.[data$Specie=="X.kroyeri" & data$Region=="N" ])))
Linf_xk_n<-c((na.omit(data$Linf.mm.TL[data$Specie=="X.kroyeri" & data$Region=="N" ])))/10
K_xk_n<-c((na.omit(data$K.ano.[data$Specie=="X.kroyeri" & data$Region=="N" ])))
L50_xk_n<-c((na.omit(data$L50.mm.TL [data$Specie=="X.kroyeri" & data$Region=="N" ])))/10
Lr_xk_n<-c((na.omit(data$Lr.mm.TL [data$Specie=="X.kroyeri" & data$Region=="NE"])))/10
Lc_xk_n<-c((na.omit(data$Lc.mm.TL [data$Specie=="X.kroyeri" & data$Region=="N" ])))/10
M_xk_n<-2.21 #(Natural Mortality Tool| Long= 2.05, Linf=12, k=1.2, Winf=11.05)
F_xk_n<-c((na.omit(data$F[data$Specie=="X.kroyeri"& data$Region=="N"])))
T0_xk_n<-c((na.omit(data$T0[data$Specie=="X.kroyeri" & data$Region=="N"])))
Tmax_xk_n<-c((na.omit(data$Tmax[data$Specie=="X.kroyeri"& data$Region=="NE"])))


# X.kroyeri NE #
Winf_xk_ne<-c((na.omit(data$Winf.g.[data$Specie=="X.kroyeri" & data$Region=="NE" ])))
Linf_xk_ne<-c((na.omit(data$Linf.mm.TL[data$Specie=="X.kroyeri" & data$Region=="NE" ])))/10
K_xk_ne<-c((na.omit(data$K.ano.[data$Specie=="X.kroyeri" & data$Region=="NE"])))
L50_xk_ne<-c((na.omit(data$L50.mm.TL [data$Specie=="X.kroyeri" & data$Region=="NE" ])))/10
Lr_xk_ne<-c((na.omit(data$Lr.mm.TL[data$Specie=="X.kroyeri" & data$Region=="NE" ])))/10
Lc_xk_ne<-c((na.omit(data$Lc.mm.TL [data$Specie=="X.kroyeri" & data$Region=="NE" ])))/10
M_xk_ne<-c((na.omit(data$M[data$Specie=="X.kroyeri" & data$Region=="NE" ])))
F_xk_ne<-c((na.omit(data$F[data$Specie=="X.kroyeri" & data$Region=="NE"])))
T0_xk_ne<-c((na.omit(data$T0[data$Specie=="X.kroyeri" & data$Region=="NE" ])))
Tmax_xk_ne<-c((na.omit(data$Tmax[data$Specie=="X.kroyeri" & data$Region=="NE"])))

# X.kroyeri  SE #
Winf_xk_se<-c((na.omit(data$Winf.g.[data$Specie=="X.kroyeri" & data$Region=="SE"])))
Linf_xk_se<-c((na.omit(data$Linf.mm.TL[data$Specie=="X.kroyeri" & data$Region=="SE" ])))/10
K_xk_se<-c((na.omit(data$K.ano.[data$Specie=="X.kroyeri" & data$Region=="SE" ])))
L50_xk_se<-c((na.omit(data$L50.mm.TL [data$Specie=="X.kroyeri" & data$Region=="SE" ])))/10
Lr_xk_se<-c((na.omit(data$Lr.mm.TL [data$Specie=="X.kroyeri" & data$Region=="SE" ])))/10
Lc_xk_se<-c((na.omit(data$Lc.mm.TL [data$Specie=="X.kroyeri" & data$Region=="SE" ])))/10
M_xk_se<-c((na.omit(data$M[data$Specie=="X.kroyeri" & data$Region=="SE" ])))
F_xk_se<-c((na.omit(data$F[data$Specie=="X.kroyeri" & data$Region=="SE" ])))
T0_xk_se<-c((na.omit(data$T0[data$Specie=="X.kroyeri" & data$Region=="SE" ])))
Tmax_xk_se<-c((na.omit(data$Tmax[data$Specie=="X.kroyeri" & data$Region=="SE" ])))

#------------------------------------------------------------



#----------- Yield per recruit estimates for each specie ----------------#
#{# F.subtilis N #
  
  #-----------------------------------------------------------------------
  #Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
  Tr_fs_n <- 0 - (log(1 - Lr_fs_n/mean(Linf_fs_n))/mean(K_fs_n))
  Tc_fs_n<- 0 - (log(1 - Lc_fs_n/mean(Linf_fs_n))/mean(K_fs_n))
  #-----------------------------------------------------------------------
  
  cat("\n","Input Parameters F.subtilis N","\n","Linf =",mean(Linf_fs_n),"& sd=",sd(Linf_fs_n),"\n",
      "Winf=",mean(Winf_fs_n),"& sd=",sd(Winf_fs_n),"\n","K=",mean(K_fs_n),"& sd=",sd(K_fs_n),"\n", 
      "L50=",mean(L50_fs_n),"& sd=",sd(L50_fs_n),"\n","Lr=",mean(Lr_fs_n),"& sd=",sd(Lr_fs_n),"\n",
      "Lc=",mean(Lc_fs_n),"& sd=",sd(Lc_fs_n),"\n","M=",mean(M_fs_n),"& sd=",sd(M_fs_n),"\n","T0=",(0),"\n",
      "F=",mean(F_fs_n),"& sd=",sd(F_fs_n),"\n","Tmax=",mean(Tmax_fs_n),"& sd=",sd(Tmax_fs_n),"\n",
      "Tr=",mean(Tr_fs_n),"& sd=",sd(Tr_fs_n),"\n","Tc=",mean(Tc_fs_n),"& sd=",sd(Tc_fs_n) ,"\n")
  
  #Avalialble fishing Mortality (F)
  F1<-na.exclude((data$Author[data$F==F_fs_n[1] & data$Specie=="F.subtilis" & data$Region== "N"]))
  F2<-na.exclude((data$Author[data$F==F_fs_n[2] & data$Specie=="F.subtilis" & data$Region== "N"]))
  
  # Lc 
  Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fs_n[1] & data$Specie=="F.subtilis" & data$Region!= ""]))
  Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fs_n[2] & data$Specie=="F.subtilis" & data$Region!= ""]))
  
  #F and author
  cat("\n","Mortality F=", F_fs_n[1]," Author="); print(F1[1])
  cat("Mortality F=", F_fs_n[2]," Author="); print(F2[1]) ; cat("\n")
  #Lc and author
  cat("Lc=", Lc_fs_n[1]," Author="); print(Lc1[1])
  cat("Lc=", Lc_fs_n[2]," Author="); print(Lc2[1]) ; cat("\n")
  
  #----------------------------------------------------------------------------------
  
  fs_n_list<- list(Winf=mean(Winf_fs_n),Linf = mean(Linf_fs_n),K = mean(K_fs_n),t0 =0,
                                                    M=mean(M_fs_n),tr =mean(Tr_fs_n))
  #----------------------------------------------------------------------------------
  #graphic isopleth
  fs_n_yr<-predict_mod(fs_n_list, FM_change = seq(0,3,0.1),
                         Lc_change=seq(1,14,0.1),type="ypr")
 
  jpeg("Shrimp_YPR.png",width=29,height=17,
         units="cm",
         res=600,
         quality=300,
         antialias="cleartype")
  
  par(mfrow=c(3,4),mar=c(2.2,1.82,0.74,0.6),oma=c(1.4,1.6,0.7,0.3),xpd=T,
      lty=1 ,lwd=1.2, cex.lab=1,cex.axis=1, bty="L",las=1)
  
  plot(fs_n_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1 = "",xlab = "")
  title("A", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)
  
  #title("F. subtilis_N", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

  #F information
  lines(x=c(mean(F_fs_n),mean(F_fs_n)),y=c(min(fs_n_yr$Lc),mean(Lc_fs_n)),lty=3,lwd=1)
  lines(x=c(min(fs_n_yr$FM_change),mean(F_fs_n)),y=c(mean(Lc_fs_n),mean(Lc_fs_n)),lty=3,lwd=1)
  text(x=1*(mean(F_fs_n)),y=0.35*min(fs_n_yr$Lc),expression(F[avg]),cex=1)

  #Lc information
  par(xpd=T,srt=90)
  text(x=min(fs_n_yr$FM_change)-0.21,y=mean(Lc_fs_n),expression(Lc["avg"]),cex=1)
  par(xpd=T,srt=360)
  #average point
  points(mean(F_fs_n), mean(Lc_fs_n),col="black",pch=20)
  
  #---------------------------------------------------------------------------------------
  
  #  F.subtilis NE #

  #-------------------------------------------------------------------------
  #Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
  Tr_fs_ne <- 0 - (log(1 - Lr_fs_ne/mean(Linf_fs_ne))/mean(K_fs_ne))
  Tc_fs_ne<- 0 - (log(1 - Lc_fs_ne/mean(Linf_fs_ne))/mean(K_fs_ne))
  #-------------------------------------------------------------------------
  
  cat("\n","Input Parameters F.subtilis NE","\n","Linf =",mean(Linf_fs_ne),"& sd=",sd(Linf_fs_ne),"\n",
      "Winf=",mean(Winf_fs_ne),"& sd=",sd(Winf_fs_ne),"\n","K=",mean(K_fs_ne),"& sd=",sd(K_fs_ne),"\n", 
      "L50=",mean(L50_fs_ne),"& sd=",sd(L50_fs_ne),"\n","Lr=",mean(Lr_fs_ne),"& sd=",sd(Lr_fs_ne),"\n",
      "Lc=",mean(Lc_fs_ne),"& sd=",sd(Lc_fs_ne),"\n","M=",mean(M_fs_ne),"& sd=",sd(M_fs_ne),"\n","T0=",(0),"\n",
      "F=",mean(F_fs_ne),"& sd=",sd(F_fs_ne),"\n","Tmax=",mean(Tmax_fs_ne),"& sd=",sd(Tmax_fs_ne),"\n",
      "Tr=",mean(Tr_fs_ne),"& sd=",sd(Tr_fs_ne),"\n","Tc=",mean(Tc_fs_ne),"& sd=",sd(Tc_fs_ne) ,"\n")
  
  #Avalialble fishing Mortality (F)
  F1<-na.exclude((data$Author[data$F==F_fs_ne[1] & data$Specie=="F.subtilis" & data$Region!= ""]))
  F2<-na.exclude((data$Author[data$F==F_fs_ne[2] & data$Specie=="F.subtilis" & data$Region!= ""]))
  
  # Lc anos
  Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fs_ne[1] & data$Specie=="F.subtilis" &  data$Region!= ""]))
  Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fs_ne[2] & data$Specie=="F.subtilis" &  data$Region!= ""]))
  
  cat("\n","Mortality F=", F_fs_ne[1]," Author="); print(F1[1])
  cat("Mortality F=", F_fs_ne[2]," Author="); print(F2[1]); cat("\n")
  
  cat("Lc=", Lc_fs_ne[1]," Author="); print(Lc1[1])
  cat("Lc=", Lc_fs_ne[2]," Author="); print(Lc2[1]) ; cat("\n")
  #----------------------------------------------------------------------------------
  
  fs_ne_list<- list(Winf=mean(Winf_fs_ne),Linf = mean(Linf_fs_ne), K = mean(K_fs_ne), 
                                          t0 =0, M= mean(M_fs_ne), tr =mean(Tr_fs_ne))
  #----------------------------------------------------------------------------------
  #graphics isopleth
  fs_ne_yr<-predict_mod(fs_ne_list, FM_change = seq(0,4,0.1),
                         Lc_change=seq(1,15,0.1),type="ypr")
  
  plot(fs_ne_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1= "",xlab="")
  title("B", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)
  
  #title("F. subtilis_NE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)
  
  #F information
  lines(x=c(mean(F_fs_ne),mean(F_fs_ne)),y=c(min(fs_ne_yr$Lc),mean(Lc_fs_ne)),lty=3,lwd=1)
  lines(x=c(min(fs_ne_yr$FM_change),mean(F_fs_ne)),y=c(mean(Lc_fs_ne),mean(Lc_fs_ne)),lty=3,lwd=1)
  text(x=1*(mean(F_fs_ne)),y=0.35*min(fs_ne_yr$Lc),expression(F[avg]),cex=1)

  #Lc information
  par(xpd=T,srt=90)
  text(x=min(fs_ne_yr$FM_change)-0.25,y=mean(Lc_fs_ne),expression(Lc["avg"]),cex=1)
  par(xpd=T,srt=360)
  #average point
  points(mean(F_fs_ne), mean(Lc_fs_ne),col="black",pch=20)
  
  #-----------------------------------------------------------------------------------
  
# F.brasiliensis SE #
#-------------------------------------------------------------------------
#Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
Tr_fb_se <- 0 - (log(1 - Lr_fb_se/mean(Linf_fb_se))/mean(K_fb_se))
Tc_fb_se<- 0 - (log(1 - Lc_fb_se/mean(Linf_fb_se))/mean(K_fb_se))
#-------------------------------------------------------------------------

cat("\n","Input Parameters F.brasiliensis SE","\n","Linf =",mean(Linf_fb_se),"& sd=",sd(Linf_fb_se),"\n",
    "Winf=",mean(Winf_fb_se),"& sd=",sd(Winf_fb_se),"\n","K=",mean(K_fb_se),"& sd=",sd(K_fb_se),"\n", 
    "L50=",mean(L50_fb_se),"& sd=",sd(L50_fb_se),"\n","Lr=",mean(Lr_fb_se),"& sd=",sd(Lr_fb_se),"\n",
    "Lc=",mean(Lc_fb_se),"& sd=",sd(Lc_fb_se),"\n","M=",mean(M_fb_se),"& sd=",sd(M_fb_se),"\n","T0=",(0),"\n",
    "F=",mean(F_fb_se),"& sd=",sd(F_fb_se),"\n","Tmax=",mean(Tmax_fb_se),"& sd=",sd(Tmax_fb_se),"\n",
    "Tr=",mean(Tr_fb_se),"& sd=",sd(Tr_fb_se),"\n","Tc=",mean(Tc_fb_se),"& sd=",sd(Tc_fb_se) ,"\n")

#Avalialble fishing Mortality (F)
F1<-na.exclude((data$Author[data$F==F_fb_se[1] & data$Specie=="F.brasiliensis" & data$Region=="SE"]))
F2<-na.exclude((data$Author[data$F==F_fb_se[2] & data$Specie=="F.brasiliensis" &  data$Region=="SE"]))
# Lc 
Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fb_se[1] & data$Specie=="F.brasiliensis" & data$Region=="SE"]))
Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fb_se[2] & data$Specie=="F.brasiliensis" & data$Region== "SE"]))

cat("\n","Mortality=", F_fb_se[1]," Author="); print(F1[1])
cat("Mortality=", F_fb_se[2]," Author="); print(F2[1]); cat("\n")

cat("Lc=", Lc_fb_se[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_fb_se[2]," Author="); print(Lc2[1]) ; cat("\n")
#------------------------------------------------------------------------------

fb_se_list<- list(Winf=mean(Winf_fb_se),Linf = mean(Linf_fb_se),K=mean(K_fb_se),
                                    t0 =0, M= mean(M_fb_se), tr =mean(Tr_fb_se))

#------------------------------------------------------------------------------
#graphics isopleth
fb_se_yr<-predict_mod(fb_se_list, FM_change = seq(0,8.5,0.1),
                         Lc_change=seq(2,18,0.1),type="ypr")
plot(fb_se_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1 = "",xlab="")
title("C", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)

#title("F. brasiliensis_SE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(mean(F_fb_se),mean(F_fb_se)),y=c(min(fb_se_yr$Lc),mean(Lc_fb_se)),lty=3,lwd=1)
lines(x=c(min(fb_se_yr$FM_change),mean(F_fb_se)),y=c(mean(Lc_fb_se),mean(Lc_fb_se)),lty=3,lwd=1)
text(x=1*(mean(F_fb_se)),y=0.64*min(fb_se_yr$Lc),expression(F[avg]),cex=1)

#Lc information
par(xpd=T,srt=90)
text(x=min(fb_se_yr$FM_change)-0.47,y=mean(Lc_fb_se),expression(Lc["avg"]),cex=1)
par(xpd=T,srt=360)
#average point
points(mean(F_fb_se), mean(Lc_fb_se),col="black",pch=20)
#----------------------------------------------------------------------------------------

# F.paulensis SE #

#--------------------------------------------------------------------------
#Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
Tr_fp_se <- 0 - (log(1 - Lr_fp_se/mean(Linf_fp_se))/mean(K_fp_se))
Tc_fp_se<- 0 - (log(1 - Lc_fp_se/mean(Linf_fp_se))/mean(K_fp_se))
#--------------------------------------------------------------------------

cat("\n","Input Parameters F.paulensis SE","\n","Linf =",mean(Linf_fp_se),"& sd=",sd(Linf_fp_se),"\n",
    "Winf=",mean(Winf_fp_se),"& sd=",sd(Winf_fp_se),"\n","K=",mean(K_fp_se),"& sd=",sd(K_fp_se),"\n", 
    "L50=",mean(L50_fp_se),"& sd=",sd(L50_fp_se),"\n","Lr=",mean(Lr_fp_se),"& sd=",sd(Lr_fp_se),"\n",
    "Lc=",mean(Lc_fp_se),"& sd=",sd(Lc_fp_se),"\n","M=",mean(M_fp_se),"& sd=",sd(M_fp_se),"\n","T0=",(0),"\n",
    "F=",mean(F_fp_se),"& sd=",sd(F_fp_se),"\n","Tmax=",mean(Tmax_fp_se),"& sd=",sd(Tmax_fp_se),"\n",
    "Tr=",mean(Tr_fp_se),"& sd=",sd(Tr_fp_se),"\n","Tc=",mean(Tc_fp_se),"& sd=",sd(Tc_fp_se) ,"\n")


#Avalialble fishing Mortality (F)
F1<-na.exclude((data$Author[data$F==F_fp_se[1] & data$Specie=="F.paulensis" & data$Region== "SE"]))
F2<-na.exclude((data$Author[data$F==F_fp_se[2] & data$Specie=="F.paulensis" & data$Region== "SE"]))

# Lc 
Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fp_se[1] & data$Specie=="F.paulensis" & data$Region== "SE"]))
Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fp_se[2] & data$Specie=="F.paulensis" & data$Region== "SE"]))

cat("\n","Mortality=", F_fp_se[1]," Author="); print(F1[1])
cat("Mortality=", F_fp_se[2]," Author="); print(F2[1]); cat("\n")

cat("Lc=", Lc_fp_se[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_fp_se[2]," Author="); print(Lc2[1]) ; cat("\n")
#--------------------------------------------------------------------------------------------

fp_se_list<- list(Winf=mean(Winf_fp_se),Linf = mean(Linf_fp_se), K = mean(K_fp_se), t0 =0, 
                                                        M= mean(M_fp_se), tr =mean(Tr_fp_se))

#---------------------------------------------------------------------------------------------
#graphics isopleth
fp_se_yr<-predict_mod(fp_se_list, FM_change = seq(0,10,0.1),
                       Lc_change=seq(4,18,0.1),type="ypr")

plot(fp_se_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="Lc (cm)",xlab ="")
title("D", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)

#title("F. paulensis_SE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(mean(F_fp_se),mean(F_fp_se)),y=c(min(fp_se_yr$Lc),mean(Lc_fp_se)),lty=3,lwd=1)
lines(x=c(min(fp_se_yr$FM_change),mean(F_fp_se)),y=c(mean(Lc_fp_se),mean(Lc_fp_se)),lty=3,lwd=1)
text(x=1*(mean(F_fp_se)),y=0.84*min(fp_se_yr$Lc),expression(F[avg]),cex=1)

#Lc information
par(xpd=T,srt=90)
text(x=min(fp_se_yr$FM_change)-0.57,y=mean(Lc_fp_se),expression(Lc["avg"]),cex=1)
par(xpd=T,srt=360)
#average point
points(mean(F_fp_se), mean(Lc_fp_se),col="black",pch=20)
# 
#------------------------------------------------------------------------------

# F.paulensis S #

#-----------------------------------------------------------------------------
#Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
Tr_fp_s <- 0 - (log(1 - Lr_fp_s/mean(Linf_fp_s))/mean(K_fp_s))
Tc_fp_s<- 0 - (log(1 - Lc_fp_s/mean(Linf_fp_s))/mean(K_fp_s))
#-----------------------------------------------------------------------------
 
cat("\n","Input Parameters F.paulensis S","\n","Linf =",mean(Linf_fp_s),"& sd=",sd(Linf_fp_s),"\n",
    "Winf=",mean(Winf_fp_s),"& sd=",sd(Winf_fp_s),"\n","K=",mean(K_fp_s),"& sd=",sd(K_fp_s),"\n", 
    "L50=",mean(L50_fp_s),"& sd=",sd(L50_fp_s),"\n","Lr=",mean(Lr_fp_s),"& sd=",sd(Lr_fp_s),"\n",
    "Lc=",mean(Lc_fp_s),"& sd=",sd(Lc_fp_s),"\n","M=",mean(M_fp_s),"& sd=",sd(M_fp_s),"\n","T0=",(0),"\n",
    "F=",mean(F_fp_s),"& sd=",sd(F_fp_s),"\n","Tmax=",mean(Tmax_fp_s),"& sd=",sd(Tmax_fp_s),"\n",
    "Tr=",mean(Tr_fp_s),"& sd=",sd(Tr_fp_s),"\n","Tc=",mean(Tc_fp_s),"& sd=",sd(Tc_fp_s) ,"\n")

#Available fishing Mortality (F)
F1<-na.exclude((data$Author[data$F==F_fp_s[1] & data$Specie=="F.paulensis" & data$Region== "S"]))
F2<-na.exclude((data$Author[data$F==F_fp_s[2] & data$Specie=="F.paulensis" & data$Region== "S"]))

# Lc 
Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fp_s[1] & data$Specie=="F.paulensis" & data$Region== "S"]))
Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_fp_s[2] & data$Specie=="F.paulensis" & data$Region== "S"]))

cat("\n","Mortality=", F_fp_s[1]," Author="); print(F1[1])
cat("Mortality=", F_fp_s[2]," Author="); print(F2[1]); cat("\n")

cat("Lc=", Lc_fp_s[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_fp_s[2]," Author="); print(Lc2[1]) ; cat("\n")

#---------------------------------------------------------------------------------------

fp_s_list<- list(Winf=mean(Winf_fp_s),Linf = mean(Linf_fp_s), K = mean(K_fp_s), t0 =0, 
                 M= mean(M_fp_s), tr =mean(Tr_fp_s))

#---------------------------------------------------------------------------------------
#graphics isopleth
fp_s_yr<-predict_mod(fp_s_list, FM_change = seq(0,7.5,0.1),
                       Lc_change=seq(4,14,0.1),type="ypr")

plot(fp_s_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1 = "",xlab ="" )
title("E", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)

#title("F. paulensis_S", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(mean(F_fp_s),mean(F_fp_s)),y=c(min(fp_s_yr$Lc),mean(Lc_fp_s)),lty=3,lwd=1)
lines(x=c(min(fp_s_yr$FM_change),mean(F_fp_s)),y=c(mean(Lc_fp_s),mean(Lc_fp_s)),lty=3,lwd=1)
text(x=1*(mean(F_fp_s)),y=0.88*min(fp_s_yr$Lc),expression(F[avg]),cex=1)

#Lc information
par(xpd=T,srt=90)
text(x=min(fp_s_yr$FM_change)-0.43,y=mean(Lc_fp_s),expression(Lc["avg"]),cex=1)
par(xpd=T,srt=360)
#average point
points(mean(F_fp_s), mean(Lc_fp_s),col="black",pch=20)
# 
#----------------------------------------------------------------------------------


# litopenaeus schmitti N  #
#------------------------------------------------------------------------
#Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
Tr_ls_n <- 0 - (log(1 - Lr_ls_n/mean(Linf_ls_n))/mean(K_ls_n))
Tc_ls_n<- 0 - (log(1 - Lc_ls_n/mean(Linf_ls_n))/mean(K_ls_n))
#------------------------------------------------------------------------

cat("\n","Input Parameters L.schmitti N","\n","Linf =",mean(Linf_ls_n),"& sd=",sd(Linf_ls_n),"\n",
    "Winf=",mean(Winf_ls_n),"& sd=",sd(Winf_ls_n),"\n","K=",mean(K_ls_n),"& sd=",sd(K_ls_n),"\n", 
    "L50=",mean(L50_ls_n),"& sd=",sd(L50_ls_n),"\n","Lr=",mean(Lr_ls_n),"& sd=",sd(Lr_ls_n),"\n",
    "Lc=",mean(Lc_ls_n),"& sd=",sd(Lc_ls_n),"\n","M=",mean(M_ls_n),"& sd=",sd(M_ls_n),"\n","T0=",(0),"\n",
    "F=",mean(F_ls_n),"& sd=",sd(F_ls_n),"\n","Tmax=",mean(Tmax_ls_n),"& sd=",sd(Tmax_ls_n),"\n",
    "Tr=",mean(Tr_ls_n),"& sd=",sd(Tr_ls_n),"\n","Tc=",mean(Tc_ls_n),"& sd=",sd(Tc_ls_n) ,"\n")

#Avalialble Lc
Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_ls_n[1] & data$Specie=="L.schmitti" & data$Region== "N"]))
Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_ls_n[2] & data$Specie=="L.schmitti" & data$Region== "N"]))

cat("Lc=", Lc_ls_n[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_ls_n[2]," Author="); print(Lc2[1]) ; cat("\n")
#---------------------------------------------------------------------------------------

ls_n_list<- list(Winf=mean(Winf_ls_n),Linf = mean(Linf_ls_n), K = mean(K_ls_n), t0 =0,
                                                    M= mean(M_ls_n), tr =mean(Tr_ls_n))

#---------------------------------------------------------------------------------------
#graphics isopleth
ls_n_yr<-predict_mod(ls_n_list, FM_change = seq(0,4,0.1),
                       Lc_change=seq(1,10,0.1),type="ypr")

plot(ls_n_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="",xlab ="" )
title("F", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)

#title("L. schmitti_N", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#Lc information
lines(x=c(min(ls_n_yr$FM_change),max(ls_n_yr$FM_change)),y=c(mean(Lc_ls_n),mean(Lc_ls_n)),lty=3,lwd=1)
par(xpd=T,srt=90)
text(x=min(ls_n_yr$FM_change)-0.26,y=mean(Lc_ls_n),expression(Lc["avg"]),cex=1)
par(xpd=T,srt=360)
#--------------------------------------------------------------------------------------------


# litopenaeus schmitti NE #

  #------------------------------------------------------------------------
  #Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
  Tr_ls_ne <- 0 - (log(1 - Lr_ls_ne/mean(Linf_ls_ne))/mean(K_ls_ne))
  Tc_ls_ne<- 0 - (log(1 - Lc_ls_ne/mean(Linf_ls_ne))/mean(K_ls_ne))
  #------------------------------------------------------------------------
  
  cat("\n","Input Parameters L.schmitti NE","\n","Linf =",mean(Linf_ls_ne),"& sd=",sd(Linf_ls_ne),"\n",
      "Winf=",mean(Winf_ls_ne),"& sd=",sd(Winf_ls_ne),"\n","K=",mean(K_ls_ne),"& sd=",sd(K_ls_ne),"\n", 
      "L50=",mean(L50_ls_ne),"& sd=",sd(L50_ls_ne),"\n","Lr=",mean(Lr_ls_ne),"& sd=",sd(Lr_ls_ne),"\n",
      "Lc=",mean(Lc_ls_ne),"& sd=",sd(Lc_ls_ne),"\n","M=",mean(M_ls_ne),"& sd=",sd(M_ls_ne),"\n","T0=",(0),"\n",
      "F=",mean(F_ls_ne),"& sd=",sd(F_ls_ne),"\n","Tmax=",mean(Tmax_ls_ne),"& sd=",sd(Tmax_ls_ne),"\n",
      "Tr=",mean(Tr_ls_ne),"& sd=",sd(Tr_ls_ne),"\n","Tc=",mean(Tc_ls_ne),"& sd=",sd(Tc_ls_ne) ,"\n")
  
  #F information
  F1<-na.exclude((data$Author[data$F==F_ls_ne[1] & data$Specie=="L.schmitti" & data$Region== "NE"]))
  F2<-na.exclude((data$Author[data$F==F_ls_ne[2] & data$Specie=="L.schmitti" & data$Region== "NE"]))
  # Lc anos
  Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_ls_ne[1] & data$Specie=="L.schmitti" & data$Region== "NE"]))
  Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_ls_ne[2] & data$Specie=="L.schmitti" & data$Region== "NE"]))
  
  cat("\n","Mortality=", F_ls_ne[1]," Author="); print(F1[1])
  cat("Mortality=", F_ls_ne[2]," Author="); print(F2[1]); cat("\n")
  
  cat("Lc=", Lc_ls_ne[1]," Author="); print(Lc1[1])
  cat("Lc=", Lc_ls_ne[2]," Author="); print(Lc2[1]) ; cat("\n")
  #----------------------------------------------------------------------------------------
  
  ls_ne_list<- list(Winf=mean(Winf_ls_ne),Linf = mean(Linf_ls_ne), K = mean(K_ls_ne), t0 =0,
                                                      M= mean(M_ls_ne), tr =mean(Tr_ls_ne))
  
  #----------------------------------------------------------------------------------------
  #graphics isopleth
  ls_ne_yr<-predict_mod(ls_ne_list, FM_change = seq(0,3,0.1),
                         Lc_change=seq(1,14,0.1),type="ypr")
 
  plot(ls_ne_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="",xlab ="")
  title("G", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)
  
  #title("L. schmitti_NE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)
  
  #F information
  lines(x=c(mean(F_ls_ne),mean(F_ls_ne)),y=c(min(ls_ne_yr$Lc),mean(Lc_ls_ne)),lty=3,lwd=1)
  lines(x=c(min(ls_ne_yr$FM_change),mean(F_ls_ne)),y=c(mean(Lc_ls_ne),mean(Lc_ls_ne)),lty=3,lwd=1)
  text(x=1*(mean(F_ls_ne)),y=0.35*min(ls_ne_yr$Lc),expression(F[avg]),cex=1)

  #Lc information
  par(xpd=T,srt=90)
  text(x=min(ls_ne_yr$FM_change)-0.21,y=mean(Lc_ls_ne),expression(Lc["avg"]),cex=1)
  par(xpd=T,srt=360)
  #average point
  points(mean(F_ls_ne), mean(Lc_ls_ne),col="black",pch=20)
  # 
  #-------------------------------------------------------------------------------------

  # litopenaeus schmitti SE #
  #--------------------------------------------------------------------------
  #Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
  Tr_ls_se <- 0 - (log(1 - Lr_ls_se/mean(Linf_ls_se))/mean(K_ls_se))
  Tc_ls_se<- 0 - (log(1 - Lc_ls_se/mean(Linf_ls_se))/mean(K_ls_se))
  #--------------------------------------------------------------------------
   
  cat("\n","Input Parameters L.schmitti SE","\n","Linf =",mean(Linf_ls_se),"& sd=",sd(Linf_ls_se),"\n",
      "Winf=",mean(Winf_ls_se),"& sd=",sd(Winf_ls_se),"\n","K=",mean(K_ls_se),"& sd=",sd(K_ls_se),"\n", 
      "L50=",mean(L50_ls_se),"& sd=",sd(L50_ls_se),"\n","Lr=",mean(Lr_ls_se),"& sd=",sd(Lr_ls_se),"\n",
      "Lc=",mean(Lc_ls_se),"& sd=",sd(Lc_ls_se),"\n","M=",mean(M_ls_se),"& sd=",sd(M_ls_se),"\n","T0=",(0),"\n",
      "F=",mean(F_ls_se),"& sd=",sd(F_ls_se),"\n","Tmax=",mean(Tmax_ls_se),"& sd=",sd(Tmax_ls_se),"\n",
      "Tr=",mean(Tr_ls_se),"& sd=",sd(Tr_ls_se),"\n","Tc=",mean(Tc_ls_se),"& sd=",sd(Tc_ls_se) ,"\n")
  
  #F information
  F1<-na.exclude((data$Author[data$F==F_ls_se[1] & data$Specie=="L.schmitti" & data$Region== "SE"]))
  F2<-na.exclude((data$Author[data$F==F_ls_se[2] & data$Specie=="L.schmitti" & data$Region=="SE"]))
  # Lc 
  Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_ls_se[1] & data$Specie=="L.schmitti" & data$Region== "SE"]))
  Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_ls_se[2] & data$Specie=="L.schmitti" & data$Region== "SE"]))
  
  cat("\n","Mortality=", F_ls_se[1]," Author="); print(F1[1])
  cat("Mortality=", F_ls_se[2]," Author="); print(F2[1]); cat("\n")
  
  cat("Lc=", Lc_ls_se[1]," Author="); print(Lc1[1])
  cat("Lc=", Lc_ls_se[2]," Author="); print(Lc2[1]) ; cat("\n")
  #------------------------------------------------------------------------------------------
  
  ls_se_list<- list(Winf=mean(Winf_ls_se),Linf = mean(Linf_ls_se), K = mean(K_ls_se), t0 =0,
                                                        M= mean(M_ls_se), tr =mean(Tr_ls_se))
  
  #------------------------------------------------------------------------------------------
  #graphics isopleth
  ls_se_yr<-predict_mod(ls_se_list, FM_change = seq(0,5.5,0.1),
                         Lc_change=seq(4,16,0.1),type="ypr")
  
  plot(ls_se_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1 ="",xlab ="")
  title("H", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)
  
  #title("L. schmitti_SE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)
  
  #F information
   lines(x=c(mean(F_ls_se),mean(F_ls_se)),y=c(min(ls_se_yr$Lc),mean(Lc_ls_se)),lty=3,lwd=1)
   lines(x=c(min(ls_se_yr$FM_change),mean(F_ls_se)),y=c(mean(Lc_ls_se),mean(Lc_ls_se)),lty=3,lwd=1)
   text(x=1*(mean(F_ls_se)),y=0.85*min(ls_se_yr$Lc),expression(F[avg]),cex=1)

  #Lc information
  par(xpd=T,srt=90)
  text(x=min(ls_se_yr$FM_change)-0.32,y=mean(Lc_ls_se),expression(Lc["avg"]),cex=1)
  par(xpd=T,srt=360)
  #average point
  points(mean(F_ls_se), mean(Lc_ls_se),col="black",pch=20)
  # 
#-------------------------------------------------------------------------------------

# X.kroyeri N #
#-----------------------------------------------------------------------
#Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
Tr_xk_n <- 0 - (log(1 - Lr_xk_n/mean(Linf_xk_n))/mean(K_xk_n))
Tc_xk_n<- 0 - (log(1 - Lc_xk_n/mean(Linf_xk_n))/mean(K_xk_n))
#-----------------------------------------------------------------------

cat("\n","Input Parameters X.kroyeri N","\n","Linf =",mean(Linf_xk_n),"& sd=",sd(Linf_xk_n),"\n",
    "Winf=",mean(Winf_xk_n),"& sd=",sd(Winf_xk_n),"\n","K=",mean(K_xk_n),"& sd=",sd(K_xk_n),"\n", 
    "L50=",mean(L50_xk_n),"& sd=",sd(L50_xk_n),"\n","Lr=",mean(Lr_xk_n),"& sd=",sd(Lr_xk_n),"\n",
    "Lc=",mean(Lc_xk_n),"& sd=",sd(Lc_xk_n),"\n","M=",mean(M_xk_n),"& sd=",sd(M_xk_n),"\n","T0=",(0),"\n",
    "F=",mean(F_xk_n),"& sd=",sd(F_xk_n),"\n","Tmax=",mean(Tmax_xk_n),"& sd=",sd(Tmax_xk_n),"\n",
    "Tr=",mean(Tr_xk_n),"& sd=",sd(Tr_xk_n),"\n","Tc=",mean(Tc_xk_n),"& sd=",sd(Tc_xk_n) ,"\n")

#No F information
# Lc 
Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_xk_n[1] & data$Specie=="X.kroyeri" & data$Region=="N"]))
Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_xk_n[2] & data$Specie=="X.kroyeri" & data$Region=="N"]))

cat("Lc=", Lc_xk_n[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_xk_n[2]," Author="); print(Lc2[1]) ; cat("\n")
#--------------------------------------------------------------------------------------

xk_n_list<- list(Winf=mean(Winf_xk_n),Linf = mean(Linf_xk_n), K = mean(K_xk_n), t0 =0, 
                                                  M= mean(M_xk_n), tr =mean(Tr_xk_n))

#--------------------------------------------------------------------------------------
#graphics isopleth
xk_n_yr<-predict_mod(xk_n_list, FM_change = seq(0,5,0.1),
                       Lc_change=seq(1,10,0.1),type="ypr")

plot(xk_n_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="",xlab ="")
title("I", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)

#title("X. kroyeri_N", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(min(xk_n_yr$FM_change),max(xk_n_yr$FM_change)),y=c(mean(Lc_xk_n),mean(Lc_xk_n)),lty=3,lwd=1)
#Lc
par(xpd=T,srt=90)
text(x=min(xk_n_yr$FM_change)-0.32,y=mean(Lc_xk_n),expression(Lc["avg"]),cex=1)
par(xpd=T,srt=360)
#average point
points(mean(F_xk_n), mean(Lc_xk_n),col="black",pch=16)
# 
#---------------------------------------------------------------------------------------------

# X.kroyeri  NE #
#--------------------------------------------------------------------------------------------
#Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
Tr_xk_ne <- 0 - (log(1 - Lr_xk_ne/mean(Linf_xk_ne))/mean(K_xk_ne))
Tc_xk_ne<- 0 - (log(1 - Lc_xk_ne/mean(Linf_xk_ne))/mean(K_xk_ne))
#-------------------------------------------------------------------------------------------

cat("\n","Input Parameters X.kroyeri NE","\n","Linf =",mean(Linf_xk_ne),"& sd=",sd(Linf_xk_ne),"\n",
    "Winf=",mean(Winf_xk_ne),"& sd=",sd(Winf_xk_ne),"\n","K=",mean(K_xk_ne),"& sd=",sd(K_xk_ne),"\n", 
    "L50=",mean(L50_xk_ne),"& sd=",sd(L50_xk_ne),"\n","Lr=",mean(Lr_xk_ne),"& sd=",sd(Lr_xk_ne),"\n",
    "Lc=",mean(Lc_xk_ne),"& sd=",sd(Lc_xk_ne),"\n","M=",mean(M_xk_ne),"& sd=",sd(M_xk_ne),"\n","T0=",(0),"\n",
    "F=",mean(F_xk_ne),"& sd=",sd(F_xk_ne),"\n","Tmax=",mean(Tmax_xk_ne),"& sd=",sd(Tmax_xk_ne),"\n",
    "Tr=",mean(Tr_xk_ne),"& sd=",sd(Tr_xk_ne),"\n","Tc=",mean(Tc_xk_ne),"& sd=",sd(Tc_xk_ne) ,"\n")

#F information
F1<-na.exclude((data$Author[data$F==F_xk_ne[1] & data$Specie=="X.kroyeri" & data$Region== "NE"]))
F2<-na.exclude((data$Author[data$F==F_xk_ne[2] & data$Specie=="X.kroyeri" & data$Region== "NE"]))
#Lc 
Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_xk_ne[1] & data$Specie=="X.kroyeri" & data$Region== "NE"]))
Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_xk_ne[2] & data$Specie=="X.kroyeri" & data$Region== "NE"]))

cat("\n","Mortality=", F_xk_ne[1]," Author="); print(F1[1])
cat("Mortality=", F_xk_ne[2]," Author="); print(F2[1]); cat("\n")

cat("Lc=", Lc_xk_ne[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_xk_ne[2]," Author="); print(Lc2[1]) ; cat("\n")
#-----------------------------------------------------------------------------------------

xk_ne_list<- list(Winf=mean(Winf_xk_ne),Linf = mean(Linf_xk_ne), K = mean(K_xk_ne),t0 =0,
                                                    M= mean(M_xk_ne), tr =mean(Tr_xk_ne))

#-----------------------------------------------------------------------------------------
#graphics isopleth
xk_ne_yr<-predict_mod(xk_ne_list, FM_change = seq(0,7,0.1),
                       Lc_change=seq(2,12,0.1),type="ypr")

plot(xk_ne_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="",xlim=c(0,11),xlab="")
title("J", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)

#title("X. kroyeri_NE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(mean(F_xk_ne),mean(F_xk_ne)),y=c(min(xk_ne_yr$Lc),mean(Lc_xk_ne)),lty=3,lwd=1)
lines(x=c(min(xk_ne_yr$FM_change),mean(F_xk_ne)),y=c(mean(Lc_xk_ne),mean(Lc_xk_ne)),lty=3,lwd=1)
text(x=1*(mean(F_xk_ne)),y=0.77*min(xk_ne_yr$Lc),expression(F[avg]),cex=1)

#Lc information
par(xpd=T,srt=90)
text(x=min(xk_ne_yr$FM_change)-0.4,y=mean(Lc_xk_ne),expression(Lc["avg"]),cex=1)
par(xpd=T,srt=360)
#average point
points(mean(F_xk_ne), mean(Lc_xk_ne),col="black",pch=20)
# 
#----------------------------------------------------------------------------------

# X.kroyeri SE #
#----------------------------------------------------------------------------
#Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
Tr_xk_se <- 0 - (log(1 - Lr_xk_se/mean(Linf_xk_se))/mean(K_xk_se))
Tc_xk_se<- 0 - (log(1 - Lc_xk_se/mean(Linf_xk_se))/mean(K_xk_se))
#----------------------------------------------------------------------------

cat("\n","Input Parameters X.kroyeri SE","\n","Linf =",mean(Linf_xk_se),"& sd=",sd(Linf_xk_se),"\n",
    "Winf=",mean(Winf_xk_se),"& sd=",sd(Winf_xk_se),"\n","K=",mean(K_xk_se),"& sd=",sd(K_xk_se),"\n", 
    "L50=",mean(L50_xk_se),"& sd=",sd(L50_xk_se),"\n","Lr=",mean(Lr_xk_se),"& sd=",sd(Lr_xk_se),"\n",
    "Lc=",mean(Lc_xk_se),"& sd=",sd(Lc_xk_se),"\n","M=",mean(M_xk_se),"& sd=",sd(M_xk_se),"\n","T0=",(0),"\n",
    "F=",mean(F_xk_se),"& sd=",sd(F_xk_se),"\n","Tmax=",mean(Tmax_xk_se),"& sd=",sd(Tmax_xk_se),"\n",
    "Tr=",mean(Tr_xk_se),"& sd=",sd(Tr_xk_se),"\n","Tc=",mean(Tc_xk_se),"& sd=",sd(Tc_xk_se) ,"\n")

#F information
F1<-na.exclude((data$Author[data$F==F_xk_se[1] & data$Specie=="X.kroyeri" & data$Region== "SE"]))
F2<-na.exclude((data$Author[data$F==F_xk_se[2] & data$Specie=="X.kroyeri" & data$Region== "SE"]))
# Lc 
Lc1<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_xk_se[1] & data$Specie=="X.kroyeri" & data$Region== "SE"]))
Lc2<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_xk_se[2] & data$Specie=="X.kroyeri" & data$Region== "SE"]))
Lc3<-na.exclude((data$Author[data$Lc.mm.TL/10==Lc_xk_se[3] & data$Specie=="X.kroyeri" & data$Region== "SE"]))

cat("\n","Mortality=", F_xk_se[1]," Author="); print(F1[1])
cat("Mortality=", F_xk_se[2]," Author="); print(F2[1]); cat("\n")

cat("Lc=", Lc_xk_se[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_xk_se[2]," Author="); print(Lc2[1])
cat("Lc=", Lc_xk_se[3]," Author="); print(Lc3[1]) ; cat("\n")
#-----------------------------------------------------------------------------------------

xk_se_list<- list(Winf=mean(Winf_xk_se),Linf = mean(Linf_xk_se), K = mean(K_xk_se), t0 =0,
                                                    M= mean(M_xk_se), tr =mean(Tr_xk_se))

#-----------------------------------------------------------------------------------------
#graphics isopleth
xk_se_yr<-predict_mod(xk_se_list, FM_change = seq(0,4.7,0.1),
                       Lc_change=seq(4,12,0.1),type="ypr")

plot(xk_se_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="",xlim=c(0,11),xlab="")
title("K", line = 0.2, adj=0.05, font.main= 2,cex.main=0.9)

#title("X. kroyeri_SE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(mean(F_xk_se),mean(F_xk_se)),y=c(min(xk_se_yr$Lc),mean(Lc_xk_se)),lty=3,lwd=1)
lines(x=c(min(xk_se_yr$FM_change),mean(F_xk_se)),y=c(mean(Lc_xk_se),mean(Lc_xk_se)),lty=3,lwd=1)
text(x=1*(mean(F_xk_se)),y=0.9*min(xk_se_yr$Lc),expression(F[avg]),cex=1)

#Lc information
par(xpd=T,srt=90)
text(x=min(xk_se_yr$FM_change)-0.3,y=mean(Lc_xk_se),expression(Lc["avg"]),cex=1)
par(xpd=T,srt=360)
#average point
points(mean(F_xk_se), mean(Lc_xk_se),col="black",pch=20)
# 
#----------------------------------------------------------------------------------


par(las=0)
mtext(side=2, text="First catch length (Lc)(cm)", outer=T,cex=0.9,line=0.45)
mtext(side=1, text=expression("Fishing mortality (F)"*(year^-1)), outer=T,cex=0.9,line=0.6)
dev.off()
#}
##x#x#xx#x#x#x#x#x#x#x#x End of yield per recruit #x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x