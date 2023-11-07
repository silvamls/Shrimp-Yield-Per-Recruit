#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x##x#x#x
# R code for shrimp yield optimization
# Pairwise test for life history parameters
# Yield per recruit estimates (Beverton & Holt)
# Isopleth graphics using TropfishR package
# Author: Matheus Lourenco
#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x##x#x#x

#cleaning workspace..
rm(list=ls())
#dev.off()

#packages *just one required
#install.packages("TropFishR")
library(TropFishR)

#reading data set of life history parameters-- Put the path where the worksheet is
data<-read.csv("C:/Matheus/Universidade/Mestrado/Planilhas/Dados NE/Parâmetros_Populacionais.csv",
               sep=";", dec=",", stringsAsFactors = F,encoding = 'latin1', header = TRUE)
#----------------------------------------------------------------------------------------------

#Life history parameters comparison

#----------- Pink parameters --------------#
#  F.subtilis N #
Winf_fs_n<-c(na.omit(data$Winf.g.[data$Especie=="F.subtilis" & data$Regiao=="N" ]))
Linf_fs_n<-c(na.omit(data$Linf.mm.TL[data$Especie=="F.subtilis" & data$Regiao=="N" ]))/10
K_fs_n<-c(na.omit(data$K.ano.[data$Especie=="F.subtilis" & data$Regiao=="N" ]))
L50_fs_n<-c(na.omit(data$L50.mm.TL[data$Especie=="F.subtilis" & data$Regiao=="N" ]))/10
Lr_fs_n<-c(na.omit(data$Lr.mm.TL[data$Especie=="F.subtilis" & data$Regiao=="N" ]))/10
Lc_fs_n<-c(na.omit(data$Lc.mm.TL [data$Especie=="F.subtilis" & data$Regiao=="N" ]))/10
M_fs_n<-c(na.omit(data$M[data$Especie=="F.subtilis" & data$Regiao=="N" ]))
F_fs_n<-c(na.omit(data$F[data$Especie=="F.subtilis" & data$Regiao=="N" ]))
T0_fs_n<-c(na.omit(data$T0[data$Especie=="F.subtilis" & data$Regiao=="N" ]))
Tmax_fs_n<-c(na.omit(data$Tmax[data$Especie=="F.subtilis" & data$Regiao=="N"]))

#  F.subtilis NE #
Winf_fs_ne<-c(na.omit(data$Winf.g.[data$Especie=="F.subtilis" & data$Regiao=="NE" ]))
Linf_fs_ne<-c(na.omit(data$Linf.mm.TL[data$Especie=="F.subtilis" & data$Regiao=="NE" ]))/10
K_fs_ne<-c(na.omit(data$K.ano.[data$Especie=="F.subtilis" & data$Regiao=="NE" ]))
L50_fs_ne<-c(na.omit(data$L50.mm.TL[data$Especie=="F.subtilis" & data$Regiao=="NE" ]))/10
Lr_fs_ne<-c(na.omit(data$Lr.mm.TL[data$Especie=="F.subtilis" & data$Regiao=="NE" ]))/10
Lc_fs_ne<-c(na.omit(data$Lc.mm.TL [data$Especie=="F.subtilis" & data$Regiao=="NE" ]))/10
M_fs_ne<-c(na.omit(data$M[data$Especie=="F.subtilis" & data$Regiao=="NE" ]))
F_fs_ne<-c(na.omit(data$F[data$Especie=="F.subtilis" & data$Regiao=="NE" ]))
T0_fs_ne<-c(na.omit(data$T0[data$Especie=="F.subtilis" & data$Regiao=="NE" ]))
Tmax_fs_ne<-c(na.omit(data$Tmax[data$Especie=="F.subtilis" & data$Regiao=="NE"]))

#welch two sample t test (N vs NE)
t.test(Winf_fs_n,Winf_fs_ne)
t.test(Linf_fs_n,Linf_fs_ne)
t.test(K_fs_n,K_fs_ne)
t.test(L50_fs_n,L50_fs_ne)
#t.test(Lr_fs_n,Lr_fs_ne) *Not enough information
#t.test(Lc_fs_n,Lc_fs_ne)
#t.test(M_fs_n,M_fs_ne)
#t.test(F_fs_n,F_fs_ne)

#  F.paulensis SE #
Winf_fp_se<-c(na.omit(data$Winf.g.[data$Especie=="F.paulensis" & data$Regiao=="SE" ]))
Linf_fp_se<-c(na.omit(data$Linf.mm.TL[data$Especie=="F.paulensis" & data$Regiao=="SE" ]))/10
K_fp_se<-c(na.omit(data$K.ano.[data$Especie=="F.paulensis" & data$Regiao=="SE" ]))
L50_fp_se<-c(na.omit(data$L50.mm.TL[data$Especie=="F.paulensis" & data$Regiao=="SE" ]))/10
Lr_fp_se<-c(na.omit(data$Lr.mm.TL[data$Especie=="F.paulensis" & data$Regiao=="SE" ]))/10
Lc_fp_se<-c(na.omit(data$Lc.mm.TL [data$Especie=="F.paulensis" & data$Regiao=="SE" ]))/10
M_fp_se<-c(na.omit(data$M[data$Especie=="F.paulensis" & data$Regiao=="SE" ]))
F_fp_se<-c(na.omit(data$F[data$Especie=="F.paulensis" & data$Regiao=="SE" ]))
T0_fp_se<-c(na.omit(data$T0[data$Especie=="F.paulensis" & data$Regiao=="SE" ]))
Tmax_fp_se<-c(na.omit(data$Tmax[data$Especie=="F.paulensis" & data$Regiao=="SE"]))

#  F.paulensis S #
Winf_fp_s<-c(na.omit(data$Winf.g.[data$Especie=="F.paulensis" & data$Regiao=="S" ]))
Linf_fp_s<-c(na.omit(data$Linf.mm.TL[data$Especie=="F.paulensis" & data$Regiao=="S" ]))/10
K_fp_s<-c(na.omit(data$K.ano.[data$Especie=="F.paulensis" & data$Regiao=="S" ]))
L50_fp_s<-c(na.omit(data$L50.mm.TL[data$Especie=="F.paulensis" & data$Regiao=="S" ]))/10
Lr_fp_s<-c(na.omit(data$Lr.mm.TL[data$Especie=="F.paulensis" & data$Regiao=="S" ]))/10
Lc_fp_s<-c(na.omit(data$Lc.mm.TL [data$Especie=="F.paulensis" & data$Regiao=="S" ]))/10
M_fp_s<-c(na.omit(data$M[data$Especie=="F.paulensis" & data$Regiao=="S" ]))
F_fp_s<-c(na.omit(data$F[data$Especie=="F.paulensis" & data$Regiao=="S" ]))
T0_fp_s<-c(na.omit(data$T0[data$Especie=="F.paulensis" & data$Regiao=="S" ]))
Tmax_fp_s<-c(na.omit(data$Tmax[data$Especie=="F.paulensis" & data$Regiao=="S"]))

#welch two sample t test (SE vs S)
t.test(Winf_fp_se,Winf_fp_s)
t.test(Linf_fp_se,Linf_fp_s)
t.test(K_fp_se,K_fp_s)
#t.test(L50_fp_se,L50_fp_s)
t.test(Lr_fp_se,Lr_fp_s)
#t.test(Lc_fp_se,Lc_fp_s)
#t.test(M_fp_se,M_fp_s)
#t.test(F_fp_se,F_fp_s)

# F.brasiliensis SE #
Linf_fb_se<-c(na.omit(data$Linf.mm.TL[data$Especie=="F.brasiliensis" & data$Regiao=="SE" ]))/10
Winf_fb_se<-c(na.omit(data$Winf.g.[data$Especie=="F.brasiliensis" & data$Regiao=="SE" ]))
K_fb_se<-c(na.omit(data$K.ano.[data$Especie=="F.brasiliensis" & data$Regiao=="SE" ]))
L50_fb_se<-c(na.omit(data$L50.mm.TL[data$Especie=="F.brasiliensis" & data$Regiao=="SE" ]))/10
Lr_fb_se<-c(na.omit(data$Lr.mm.TL[data$Especie=="F.brasiliensis" & data$Regiao=="SE" ]))/10
Lc_fb_se<-c(na.omit(data$Lc.mm.TL [data$Especie=="F.brasiliensis" & data$Regiao=="SE" ]))/10
M_fb_se<-c(na.omit(data$M[data$Especie=="F.brasiliensis" & data$Regiao=="SE" ]))
F_fb_se<-c(na.omit(data$F[data$Especie=="F.brasiliensis" & data$Regiao=="SE" ]))
T0_fb_se<-c(na.omit(data$T0[data$Especie=="F.brasiliensis" & data$Regiao=="SE" ]))
Tmax_fb_se<-c(na.omit(data$Tmax[data$Especie=="F.brasiliensis" & data$Regiao=="SE"]))


#welch two sample t test F.brasiliensis vs F.paulensis (SE vs SE)
t.test(Winf_fp_se,Winf_fb_se)
t.test(Linf_fp_se,Linf_fb_se)
t.test(K_fp_se,K_fb_se)
t.test(L50_fp_se,L50_fb_se)
t.test(Lr_fp_se,Lr_fb_se)
t.test(Lc_fp_se,Lc_fb_se)
#t.test(M_fp_se,M_fb_se)
#t.test(F_fp_se,F_fb_se)
#--------------------------------------------------------------


#----------- White parameters --------------#

# litopenaeus schmitti N  #
Winf_ls_n<-c((na.omit(data$Winf.g.[data$Especie=="L.schmitti" & data$Regiao=="N"])))
Linf_ls_n<-c((na.omit(data$Linf.mm.TL[data$Especie=="L.schmitti" & data$Regiao=="N"])))/10
K_ls_n<-c((na.omit(data$K.ano.[data$Especie=="L.schmitti"& data$Regiao=="N"])))
L50_ls_n<-c((na.omit(data$L50.mm.TL[data$Especie=="L.schmitti"& data$Regiao=="N"])))/10
Lr_ls_n<-c((na.omit(data$Lr.mm.TL [data$Especie=="L.schmitti"& data$Regiao=="N"])))/10
Lc_ls_n<-c((na.omit(data$Lc.mm.TL [data$Especie=="L.schmitti"& data$Regiao=="N"])))/10
M_ls_n<-c((na.omit(data$M[data$Especie=="L.schmitti" & data$Regiao!=""])))
F_ls_n<-c((na.omit(data$F[data$Especie=="L.schmitti" & data$Regiao!=""])))
T0_ls_n<-c((na.omit(data$T0[data$Especie=="L.schmitti"& data$Regiao=="N"])))
Tmax_ls_n<-c((na.omit(data$Tmax[data$Especie=="L.schmitti" & data$Regiao!=""])))


# litopenaeus schmitti NE #
Winf_ls_ne<-c((na.omit(data$Winf.g.[data$Especie=="L.schmitti" & data$Regiao=="NE"])))
Linf_ls_ne<-c((na.omit(data$Linf.mm.TL[data$Especie=="L.schmitti" & data$Regiao=="NE"])))/10
K_ls_ne<-c((na.omit(data$K.ano.[data$Especie=="L.schmitti"& data$Regiao=="NE"])))
L50_ls_ne<-c((na.omit(data$L50.mm.TL[data$Especie=="L.schmitti"& data$Regiao=="NE"])))/10
Lr_ls_ne<-c((na.omit(data$Lr.mm.TL [data$Especie=="L.schmitti"& data$Regiao=="NE" & data$Regiao!="N"])))/10
Lc_ls_ne<-c((na.omit(data$Lc.mm.TL [data$Especie=="L.schmitti"& data$Regiao=="NE"])))/10
M_ls_ne<-c((na.omit(data$M[data$Especie=="L.schmitti" & data$Regiao=="NE"])))
F_ls_ne<-c((na.omit(data$F[data$Especie=="L.schmitti" & data$Regiao=="NE"])))
T0_ls_ne<-c((na.omit(data$T0[data$Especie=="L.schmitti"& data$Regiao=="NE"])))
Tmax_ls_ne<-c((na.omit(data$Tmax[data$Especie=="L.schmitti" & data$Regiao=="NE"])))


#welch two sample t test (N vs NE) *not enough information
#t.test(Winf_ls_n,Winf_ls_ne)
#t.test(Linf_ls_n,Linf_ls_ne)
#t.test(K_ls_n,K_ls_ne)
t.test(L50_ls_n,L50_ls_ne)
#t.test(Lr_ls_n,Lr_ls_ne)
#t.test(Lc_ls_n,Lc_ls_ne)
#t.test(M_ls_n,M_ls_ne)
#t.test(F_ls_n,F_ls_ne)


# litopenaeus schmitti SE #
Winf_ls_se<-c((na.omit(data$Winf.g.[data$Especie=="L.schmitti" & data$Regiao=="SE"])))
Linf_ls_se<-c((na.omit(data$Linf.mm.TL[data$Especie=="L.schmitti" & data$Regiao=="SE"])))/10
K_ls_se<-c((na.omit(data$K.ano.[data$Especie=="L.schmitti"& data$Regiao=="SE"])))
L50_ls_se<-c((na.omit(data$L50.mm.TL[data$Especie=="L.schmitti"& data$Regiao=="SE"])))/10
Lr_ls_se<-c((na.omit(data$Lr.mm.TL [data$Especie=="L.schmitti"& data$Regiao=="SE" & data$Regiao!="N"])))/10
Lc_ls_se<-c((na.omit(data$Lc.mm.TL [data$Especie=="L.schmitti"& data$Regiao=="SE"])))/10
M_ls_se<-c((na.omit(data$M[data$Especie=="L.schmitti" & data$Regiao=="SE"])))
F_ls_se<-c((na.omit(data$F[data$Especie=="L.schmitti" & data$Regiao=="SE"])))
T0_ls_se<-c((na.omit(data$T0[data$Especie=="L.schmitti"& data$Regiao=="SE"])))
Tmax_ls_se<-c((na.omit(data$Tmax[data$Especie=="L.schmitti" & data$Regiao=="SE"])))

#welch two sample t test (NE vs SE) *not enough information
t.test(Winf_ls_se,Winf_ls_ne)
t.test(Linf_ls_se,Linf_ls_ne)
t.test(K_ls_se,K_ls_ne)
t.test(L50_ls_se,L50_ls_ne)
#t.test(Lr_ls_se,Lr_ls_ne)
#t.test(Lc_ls_se,Lc_ls_ne)
#t.test(M_ls_se,M_ls_ne)
#t.test(F_ls_se,F_ls_ne)
#--------------------------------------------------------


#----------- Seabob parameters --------------#

# X.kroyeri N #
Winf_xk_n<-c((na.omit(data$Winf.g.[data$Especie=="X.kroyeri" & data$Regiao=="N" ])))
Linf_xk_n<-c((na.omit(data$Linf.mm.TL[data$Especie=="X.kroyeri" & data$Regiao=="N" ])))/10
K_xk_n<-c((na.omit(data$K.ano.[data$Especie=="X.kroyeri" & data$Regiao=="N" ])))
L50_xk_n<-c((na.omit(data$L50.mm.TL [data$Especie=="X.kroyeri" & data$Regiao=="N" ])))/10
Lr_xk_n<-c((na.omit(data$Lr.mm.TL [data$Especie=="X.kroyeri" & data$Regiao!="N"])))/10
Lc_xk_n<-c((na.omit(data$Lc.mm.TL [data$Especie=="X.kroyeri" & data$Regiao=="N" ])))/10
M_xk_n<-c((na.omit(data$M[data$Especie=="X.kroyeri"])))
F_xk_n<-c((na.omit(data$F[data$Especie=="X.kroyeri"])))
T0_xk_n<-c((na.omit(data$T0[data$Especie=="X.kroyeri" & data$Regiao=="N"])))
Tmax_xk_n<-c((na.omit(data$Tmax[data$Especie=="X.kroyeri" ])))


# X.kroyeri NE #
Winf_xk_ne<-c((na.omit(data$Winf.g.[data$Especie=="X.kroyeri" & data$Regiao=="NE" ])))
Linf_xk_ne<-c((na.omit(data$Linf.mm.TL[data$Especie=="X.kroyeri" & data$Regiao=="NE" ])))/10
K_xk_ne<-c((na.omit(data$K.ano.[data$Especie=="X.kroyeri" & data$Regiao=="NE"])))
L50_xk_ne<-c((na.omit(data$L50.mm.TL [data$Especie=="X.kroyeri" & data$Regiao=="NE" ])))/10
Lr_xk_ne<-c((na.omit(data$Lr.mm.TL[data$Especie=="X.kroyeri" & data$Regiao=="NE" ])))/10
Lc_xk_ne<-c((na.omit(data$Lc.mm.TL [data$Especie=="X.kroyeri" & data$Regiao=="NE" ])))/10
M_xk_ne<-c((na.omit(data$M[data$Especie=="X.kroyeri" & data$Regiao=="NE" ])))
F_xk_ne<-c((na.omit(data$F[data$Especie=="X.kroyeri" & data$Regiao=="NE"])))
T0_xk_ne<-c((na.omit(data$T0[data$Especie=="X.kroyeri" & data$Regiao=="NE" ])))
Tmax_xk_ne<-c((na.omit(data$Tmax[data$Especie=="X.kroyeri" & data$Regiao=="NE"])))


#welch two sample t test (N vs NE) *few information
#t.test(Winf_xk_n,Winf_xk_ne)
#t.test(Linf_xk_n,Linf_xk_ne)
#t.test(K_xk_n,K_xk_ne)
t.test(L50_xk_n,L50_xk_ne)
t.test(Lr_xk_n,Lr_xk_ne)
#t.test(Lc_xk_n,Lc_xk_ne)
#t.test(M_xk_n,M_xk_ne)
#t.test(F_xk_n,F_xk_ne)

# X.kroyeri  SE #
Winf_xk_se<-c((na.omit(data$Winf.g.[data$Especie=="X.kroyeri" & data$Regiao=="SE"])))
Linf_xk_se<-c((na.omit(data$Linf.mm.TL[data$Especie=="X.kroyeri" & data$Regiao=="SE" ])))/10
K_xk_se<-c((na.omit(data$K.ano.[data$Especie=="X.kroyeri" & data$Regiao=="SE" ])))
L50_xk_se<-c((na.omit(data$L50.mm.TL [data$Especie=="X.kroyeri" & data$Regiao=="SE" ])))/10
Lr_xk_se<-c((na.omit(data$Lr.mm.TL [data$Especie=="X.kroyeri" & data$Regiao=="SE" ])))/10
Lc_xk_se<-c((na.omit(data$Lc.mm.TL [data$Especie=="X.kroyeri" & data$Regiao=="SE" ])))/10
M_xk_se<-c((na.omit(data$M[data$Especie=="X.kroyeri" & data$Regiao=="SE" ])))
F_xk_se<-c((na.omit(data$F[data$Especie=="X.kroyeri" & data$Regiao=="SE" ])))
T0_xk_se<-c((na.omit(data$T0[data$Especie=="X.kroyeri" & data$Regiao=="SE" ])))
Tmax_xk_se<-c((na.omit(data$Tmax[data$Especie=="X.kroyeri" & data$Regiao=="SE" ])))

#welch two sample t test (NE vs SE) *few information
t.test(Winf_xk_ne,Winf_xk_se)
t.test(Linf_xk_ne,Linf_xk_se)
t.test(K_xk_ne,K_xk_se)
t.test(L50_xk_ne,L50_xk_se)
t.test(Lr_xk_ne,Lr_xk_se)
t.test(Lc_xk_ne,Lc_xk_se)
t.test(M_xk_ne,M_xk_se)
#t.test(F_xk_ne,F_xk_se)


#welch two sample t test (N vs SE) *few information
#t.test(Winf_xk_n,Winf_xk_se)
#t.test(Linf_xk_n,Linf_xk_ne)
#t.test(K_xk_n,K_xk_ne)
t.test(L50_xk_n,L50_xk_se)
t.test(Lr_xk_n,Lr_xk_se)
#t.test(Lc_xk_n,Lc_xk_ne)
#t.test(M_xk_n,M_xk_ne)
#t.test(F_xk_n,F_xk_ne)


# X.kroyeri S #
Linf_xk_s<-c((na.omit(data$Linf.mm.TL[data$Especie=="X.kroyeri" & data$Regiao=="S" ])))/10
Winf_xk_s<-c((na.omit(data$Winf.g.[data$Especie=="X.kroyeri" & data$Regiao=="S" ])))
K_xk_s<-c((na.omit(data$K.ano.[data$Especie=="X.kroyeri" & data$Regiao=="S" ])))
L50_xk_s<-c((na.omit(data$L50.mm.TL [data$Especie=="X.kroyeri" & data$Regiao=="S" ])))/10
Lr_xk_s<-c((na.omit(data$Lr.mm.TL [data$Especie=="X.kroyeri"  & data$Regiao!="N"])))/10
Lc_xk_s<-c((na.omit(data$Lc.mm.TL [data$Especie=="X.kroyeri"  & data$Regiao!=" " ])))/10
M_xk_s<-c((na.omit(data$M[data$Especie=="X.kroyeri" & data$Regiao!=" "])))
F_xk_s<-c((na.omit(data$F[data$Especie=="X.kroyeri" & data$Regiao!=" "])))
T0_xk_s<-c((na.omit(data$T0[data$Especie=="X.kroyeri" & data$Regiao=="S" ])))
Tmax_xk_s<-c((na.omit(data$Tmax[data$Especie=="X.kroyeri" & data$Regiao=="S" ])))

#welch two sample t test (SE vs S) *few information
t.test(Winf_xk_se,Winf_xk_s)
t.test(Linf_xk_se,Linf_xk_s)
t.test(K_xk_se,K_xk_s)
t.test(L50_xk_se,L50_xk_s)
t.test(Lr_xk_se,Lr_xk_s)
#t.test(Lc_xk_se,Lc_xk_s)
#t.test(M_xk_se,M_xk_s)
#t.test(F_xk_se,F_xk_s)
#------------------------------------------------------------



#----------- Yield per recruit estimates for each specie ----------------#
{# F.subtilis N #
  
  #-------------------------------------------------------------------------
  #Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
  Tr_fs_n <- 0 - (log(1 - Lr_fs_n/mean(Linf_fs_n))/mean(K_fs_n))
  Tc_fs_n<- 0 - (log(1 - Lc_fs_n/mean(Linf_fs_n))/mean(K_fs_n))
  #-------------------------------------------------------------------------
  
  cat("\n","Input Parameters F.subtilis N","\n","Linf =",mean(Linf_fs_n),"& sd=",sd(Linf_fs_n),"\n",
      "Winf=",mean(Winf_fs_n),"& sd=",sd(Winf_fs_n),"\n","K=",mean(K_fs_n),"& sd=",sd(K_fs_n),"\n", 
      "L50=",mean(L50_fs_n),"& sd=",sd(L50_fs_n),"\n","Lr=",mean(Lr_fs_n),"& sd=",sd(Lr_fs_n),"\n",
      "Lc=",mean(Lc_fs_n),"& sd=",sd(Lc_fs_n),"\n","M=",mean(M_fs_n),"& sd=",sd(M_fs_n),"\n","T0=",(0),"\n",
      "F=",mean(F_fs_n),"& sd=",sd(F_fs_n),"\n","Tmax=",mean(Tmax_fs_n),"& sd=",sd(Tmax_fs_n),"\n",
      "Tr=",mean(Tr_fs_n),"& sd=",sd(Tr_fs_n),"\n","Tc=",mean(Tc_fs_n),"& sd=",sd(Tc_fs_n) ,"\n")
  
  #Avalialble fishing Mortality (F)
  F1<-na.exclude((data$Autor[data$F==F_fs_n[1] & data$Regiao!= ""]))
  F2<-na.exclude((data$Autor[data$F==F_fs_n[2] & data$Regiao!= ""]))
  
  # Lc 
  Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fs_n[1] & data$Regiao!= ""]))
  Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fs_n[2] & data$Regiao!= ""]))
  
  #F and author
  cat("\n","Mortality F=", F_fs_n[1]," Autor="); print(F1[1])
  cat("Mortality F=", F_fs_n[2]," Autor="); print(F2[1]) ; cat("\n")
  #Lc and author
  cat("Lc=", Lc_fs_n[1]," Author="); print(Lc1[1])
  cat("Lc=", Lc_fs_n[2]," Author="); print(Lc2[1]) ; cat("\n")
  
  #----------------------------------------------------------------------------------
  
  fs_n_list<- list(Winf=mean(Winf_fs_n),Linf = mean(Linf_fs_n),K = mean(K_fs_n),t0 =0,
                                                    M=mean(M_fs_n),tr =mean(Tr_fs_n))
  #----------------------------------------------------------------------------------
  #graphic isopleth
  fs_n_yr<-predict_mod(fs_n_list, FM_change = seq(0,2.5,0.1),
                         Lc_change=seq(1,14,0.1),type="ypr")
 
  #if you want to save file (png)
  # jpeg("Shrimp_YPR.png",width=24,height=14,
  #       units="cm",
  #       res=800,
  #       quality=150,
  #       antialias="cleartype")
  
  par(mfrow=c(3,4),mar=c(2.2,1.8,0.9,0.6),oma=c(1.4,1.6,0.7,0.3),xpd=T,
      lty=1 ,lwd=1.2, cex.lab=1.1,cex.axis=1.1, bty="L",las=1)
  
  plot(fs_n_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1 = "",xlab = "")
  title("F. subtilis_N", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)
  
  #F information
  lines(x=c(mean(F_fs_n),mean(F_fs_n)),y=c(min(fs_n_yr$Lc),mean(Lc_fs_n)),lty=3,lwd=1)
  lines(x=c(min(fs_n_yr$FM_change),mean(F_fs_n)),y=c(mean(Lc_fs_n),mean(Lc_fs_n)),lty=3,lwd=1)
  text(x=1*(mean(F_fs_n)),y=0.25*min(fs_n_yr$Lc),expression(F[avg]),cex=1.2)
    
  #Lc information
  par(xpd=T,srt=90)
  text(x=min(fs_n_yr$FM_change)-0.16,y=mean(Lc_fs_n),expression(Lc["avg"]),cex=1.2)
  par(xpd=T,srt=360)
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
  F1<-na.exclude((data$Autor[data$F==F_fs_ne[1] & data$Regiao!= ""]))
  F2<-na.exclude((data$Autor[data$F==F_fs_ne[2] & data$Regiao!= ""]))
  
  # Lc anos
  Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fs_ne[1] & data$Regiao!= ""]))
  Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fs_ne[2] & data$Regiao!= ""]))
  
  cat("\n","Mortality F=", F_fs_ne[1]," Author="); print(F1[1])
  cat("Mortality F=", F_fs_ne[2]," Author="); print(F2[1]); cat("\n")
  
  cat("Lc=", Lc_fs_ne[1]," Author="); print(Lc1[1])
  cat("Lc=", Lc_fs_ne[2]," Author="); print(Lc2[1]) ; cat("\n")
  #----------------------------------------------------------------------------------
  
  fs_ne_list<- list(Winf=mean(Winf_fs_ne),Linf = mean(Linf_fs_ne), K = mean(K_fs_ne), 
                                          t0 =0, M= mean(M_fs_ne), tr =mean(Tr_fs_ne))
  #----------------------------------------------------------------------------------
  #graphics isopleth
  fs_ne_yr<-predict_mod(fs_ne_list, FM_change = seq(0,3.5,0.1),
                         Lc_change=seq(1,15,0.1),type="ypr")
  
  plot(fs_ne_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1= "",xlab="")
  title("F. subtilis_NE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)
  
  #F information
  lines(x=c(mean(F_fs_ne),mean(F_fs_ne)),y=c(min(fs_ne_yr$Lc),mean(Lc_fs_ne)),lty=3,lwd=1)
  lines(x=c(min(fs_ne_yr$FM_change),mean(F_fs_ne)),y=c(mean(Lc_fs_ne),mean(Lc_fs_ne)),lty=3,lwd=1)
  text(x=1*(mean(F_fs_ne)),y=0.25*min(fs_ne_yr$Lc),expression(F[2016]),cex=1.2)
  
  #Lc information
  par(xpd=T,srt=90)
  text(x=min(fs_ne_yr$FM_change)-0.19,y=mean(Lc_fs_ne),expression(Lc[2016]),cex=1.2)
  par(xpd=T,srt=360)
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
F1<-na.exclude((data$Autor[data$F==F_fb_se[1] & data$Regiao!= ""]))
F2<-na.exclude((data$Autor[data$F==F_fb_se[2] & data$Regiao!= ""]))
# Lc 
Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fb_se[1] & data$Regiao!= ""]))
Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fb_se[2] & data$Regiao!= ""]))

cat("\n","Mortality=", F_fb_se[1]," Author="); print(F1[1])
cat("Mortality=", F_fb_se[2]," Author="); print(F2[1]); cat("\n")

cat("Lc=", Lc_fb_se[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_fb_se[2]," Author="); print(Lc2[1]) ; cat("\n")
#------------------------------------------------------------------------------

fb_se_list<- list(Winf=mean(Winf_fb_se),Linf = mean(Linf_fb_se),K=mean(K_fb_se),
                                    t0 =0, M= mean(M_fb_se), tr =mean(Tr_fb_se))

#------------------------------------------------------------------------------
#graphics isopleth
fb_se_yr<-predict_mod(fb_se_list, FM_change = seq(0,7,0.1),
                         Lc_change=seq(2,18,0.1),type="ypr")
plot(fb_se_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1 = "",xlab="")
title("F. brasiliensis_SE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(mean(F_fb_se),mean(F_fb_se)),y=c(min(fb_se_yr$Lc),mean(Lc_fb_se)),lty=3,lwd=1)
lines(x=c(min(fb_se_yr$FM_change),mean(F_fb_se)),y=c(mean(Lc_fb_se),mean(Lc_fb_se)),lty=3,lwd=1)
text(x=mean(F_fb_se),y=0.62*min(fb_se_yr$Lc),expression(F[avg]),cex=1.2)

#Lc information
par(xpd=T,srt=90)
text(x=min(fb_se_yr$FM_change)-0.32,y=mean(Lc_fb_se),expression(Lc["avg"]),cex=1.2)
par(xpd=T,srt=360)
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
F1<-na.exclude((data$Autor[data$F==F_fp_se[1] & data$Regiao!= ""]))
F2<-na.exclude((data$Autor[data$F==F_fp_se[2] & data$Regiao!= ""]))

# Lc 
Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fp_se[1] & data$Regiao!= ""]))
Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fp_se[2] & data$Regiao!= ""]))

cat("\n","Mortality=", F_fp_se[1]," Author="); print(F1[1])
cat("Mortality=", F_fp_se[2]," Author="); print(F2[1]); cat("\n")

cat("Lc=", Lc_fp_se[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_fp_se[2]," Author="); print(Lc2[1]) ; cat("\n")
#--------------------------------------------------------------------------------------------

fp_se_list<- list(Winf=mean(Winf_fp_se),Linf = mean(Linf_fp_se), K = mean(K_fp_se), t0 =0, 
                                                        M= mean(M_fp_se), tr =mean(Tr_fp_se))

#---------------------------------------------------------------------------------------------
#graphics isopleth
fp_se_yr<-predict_mod(fp_se_list, FM_change = seq(0,8,0.1),
                       Lc_change=seq(4,18,0.1),type="ypr")

plot(fp_se_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="Lc (cm)",xlab ="")
title("F. paulensis_SE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(F_fp_se[1],F_fp_se[1]),y=c(min(fp_se_yr$Lc),mean(Lc_fp_se)),lty=3,lwd=1)
lines(x=c(min(fp_se_yr$FM_change),F_fp_se[1]),y=c(mean(Lc_fp_se),mean(Lc_fp_se)),lty=3,lwd=1)
text(x=(F_fp_se[1]),y=0.8*min(fp_se_yr$Lc),expression(F[2006]),cex=1.2)

#Lc information
par(xpd=T,srt=90)
text(x=min(fp_se_yr$FM_change)-0.35,y=mean(Lc_fp_se),expression(Lc["avg"]),cex=1.2)
par(xpd=T,srt=360)
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

#Avalialble fishing Mortality (F)
F1<-na.exclude((data$Autor[data$F==F_fp_s[1] & data$Regiao!= ""]))
F2<-na.exclude((data$Autor[data$F==F_fp_s[2] & data$Regiao!= ""]))

# Lc 
Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fp_s[1] & data$Regiao!= ""]))
Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_fp_s[2] & data$Regiao!= ""]))

cat("\n","Mortality=", F_fp_s[1]," Author="); print(F1[1])
cat("Mortality=", F_fp_s[2]," Author="); print(F2[1]); cat("\n")

cat("Lc=", Lc_fp_s[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_fp_s[2]," Author="); print(Lc2[1]) ; cat("\n")

#---------------------------------------------------------------------------------------

fp_s_list<- list(Winf=mean(Winf_fp_s),Linf = mean(Linf_fp_s), K = mean(K_fp_s), t0 =0, 
                 M= mean(M_fp_s), tr =mean(Tr_fp_s))

#---------------------------------------------------------------------------------------
#graphics isopleth
fp_s_yr<-predict_mod(fp_s_list, FM_change = seq(0,7,0.1),
                       Lc_change=seq(4,14,0.1),type="ypr")

plot(fp_s_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1 = "",xlab ="" )
title("F. paulensis_S", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(F_fp_s[1],F_fp_s[1]),y=c(min(fp_s_yr$Lc),Lc_fp_s[1]),lty=3,lwd=1)
lines(x=c(min(fp_s_yr$FM_change),F_fp_s[1]),y=c(Lc_fp_s[1],Lc_fp_s[1]),lty=3,lwd=1)
text(x=(F_fp_s[1]),y=0.86*min(fp_s_yr$Lc),expression(F[1983]),cex=1.2)

#Lc information
par(xpd=T,srt=90)
text(x=min(fp_s_yr$FM_change)-0.32,y=Lc_fp_s[1],expression(Lc[2015]),cex=1.2)
par(xpd=T,srt=360)
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
Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_ls_n[1] & data$Regiao!= ""]))
Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_ls_n[2] & data$Regiao!= ""]))

cat("Lc=", Lc_ls_n[1]," Author="); print(Lc1[1])
cat("Lc=", Lc_ls_n[2]," Author="); print(Lc2[1]) ; cat("\n")
#---------------------------------------------------------------------------------------

ls_n_list<- list(Winf=mean(Winf_ls_n),Linf = mean(Linf_ls_n), K = mean(K_ls_n), t0 =0,
                                                    M= mean(M_ls_n), tr =mean(Tr_ls_n))

#---------------------------------------------------------------------------------------
#graphics isopleth
fp_s_yr<-predict_mod(ls_n_list, FM_change = seq(0,4,0.1),
                       Lc_change=seq(1,10,0.1),type="ypr")

plot(fp_s_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="",xlab ="" )
title("L. schmitti_N", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)


#Lc information
lines(x=c(min(fp_s_yr$FM_change),max(fp_s_yr$FM_change)),y=c(Lc_ls_n[1],Lc_ls_n[1]),lty=3,lwd=1)
par(xpd=T,srt=90)
text(x=min(fp_s_yr$FM_change)-0.2,y=mean(Lc_ls_n),expression(Lc["2015"]),cex=1.2)
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
  F1<-na.exclude((data$Autor[data$F==F_ls_ne[1] & data$Regiao!= ""]))
  F2<-na.exclude((data$Autor[data$F==F_ls_ne[2] & data$Regiao!= ""]))
  # Lc anos
  Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_ls_ne[1] & data$Regiao!= ""]))
  Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_ls_ne[2] & data$Regiao!= ""]))
  
  cat("\n","Mortality=", F_ls_ne[1]," Author="); print(F1[1])
  cat("Mortality=", F_ls_ne[2]," Author="); print(F2[1]); cat("\n")
  
  cat("Lc=", Lc_ls_ne[1]," Author="); print(Lc1[1])
  cat("Lc=", Lc_ls_ne[2]," Author="); print(Lc2[1]) ; cat("\n")
  #----------------------------------------------------------------------------------------
  
  ls_n_list<- list(Winf=mean(Winf_ls_ne),Linf = mean(Linf_ls_ne), K = mean(K_ls_ne), t0 =0,
                                                      M= mean(M_ls_ne), tr =mean(Tr_ls_ne))
  
  #----------------------------------------------------------------------------------------
  #graphics isopleth
  fp_s_yr<-predict_mod(ls_n_list, FM_change = seq(0,3,0.1),
                         Lc_change=seq(1,14,0.1),type="ypr")
 
  plot(fp_s_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="",xlab ="")
  title("L. schmitti_NE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)
  
  #F information
  lines(x=c(F_ls_ne[1],F_ls_ne[1]),y=c(min(fp_s_yr$Lc),mean(Lc_ls_ne)),lty=3,lwd=1)
  lines(x=c(min(fp_s_yr$FM_change),F_ls_ne[1]),y=c(mean(Lc_ls_ne),mean(Lc_ls_ne)),lty=3,lwd=1)
  text(x=F_ls_ne[1],y=0.32*min(fp_s_yr$Lc),expression(F["2019"]),cex=1.2)
  
  #Lc information
  par(xpd=T,srt=90)
  text(x=min(fp_s_yr$FM_change)-0.17,y=mean(Lc_ls_ne),expression(Lc["avg"]),cex=1.2)
  par(xpd=T,srt=360)
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
  F1<-na.exclude((data$Autor[data$F==F_ls_se[1] & data$Regiao!= ""]))
  F2<-na.exclude((data$Autor[data$F==F_ls_se[2] & data$Regiao!= ""]))
  # Lc 
  Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_ls_se[1] & data$Regiao!= ""]))
  Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_ls_se[2] & data$Regiao!= ""]))
  
  cat("\n","Mortality=", F_ls_se[1]," Author="); print(F1[1])
  cat("Mortality=", F_ls_se[2]," Author="); print(F2[1]); cat("\n")
  
  cat("Lc=", Lc_ls_se[1]," Author="); print(Lc1[1])
  cat("Lc=", Lc_ls_se[2]," Author="); print(Lc2[1]) ; cat("\n")
  #------------------------------------------------------------------------------------------
  
  ls_se_list<- list(Winf=mean(Winf_ls_se),Linf = mean(Linf_ls_se), K = mean(K_ls_se), t0 =0,
                                                        M= mean(M_ls_se), tr =mean(Tr_ls_se))
  
  #------------------------------------------------------------------------------------------
  #graphics isopleth
  fp_s_yr<-predict_mod(ls_se_list, FM_change = seq(0,5,0.1),
                         Lc_change=seq(4,16,0.1),type="ypr")
  
  plot(fp_s_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1 ="",xlab ="")
  title("L. schmitti_SE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)
  
  #F information
  lines(x=c(F_ls_se[1],F_ls_se[1]),y=c(min(fp_s_yr$Lc),Lc_ls_se[1]),lty=3,lwd=1)
  lines(x=c(min(fp_s_yr$FM_change),F_ls_se[1]),y=c(Lc_ls_se[1],Lc_ls_se[1]),lty=3,lwd=1)
  text(x=F_ls_se[1],y=0.85*min(fp_s_yr$Lc),expression(F["2013"]),cex=1.2)
  #Lc
  par(xpd=T,srt=90)
  text(x=min(fp_s_yr$FM_change)-0.25,y=mean(Lc_ls_se),expression(Lc["2012"]),cex=1.2)
  par(xpd=T,srt=360)
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

# Lc 
Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_xk_n[1] & data$Regiao!= ""]))
Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_xk_n[2] & data$Regiao!= ""]))

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
title("X. kroyeri_N", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(min(xk_n_yr$FM_change),max(xk_n_yr$FM_change)),y=c(Lc_xk_n[1],Lc_xk_n[1]),lty=3,lwd=1)
#Lc
par(xpd=T,srt=90)
text(x=min(xk_n_yr$FM_change)-0.25,y=Lc_xk_n[1],expression(Lc["2015"]),cex=1.2)
par(xpd=T,srt=360)
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
F1<-na.exclude((data$Autor[data$F==F_xk_ne[1] & data$Regiao!= ""]))
F2<-na.exclude((data$Autor[data$F==F_xk_ne[2] & data$Regiao!= ""]))
#Lc 
Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_xk_ne[1] & data$Regiao!= ""]))
Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_xk_ne[2] & data$Regiao!= ""]))

cat("\n","Mortality=", F[1]," Author="); print(F1[1])
cat("Mortality=", F[2]," Author="); print(F2[1]) ; cat("\n")

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
title("X. kroyeri_NE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(mean(F_xk_ne),mean(F_xk_ne)),y=c(min(xk_ne_yr$Lc),mean(Lc_xk_ne)),lty=3,lwd=1)
lines(x=c(min(xk_ne_yr$FM_change),mean(F_xk_ne)),y=c(mean(Lc_xk_ne),mean(Lc_xk_ne)),lty=3,lwd=1)
text(x=mean(F_xk_ne),y=0.73*min(xk_ne_yr$Lc),expression(F["avg"]),cex=1.2)
#Lc 
par(xpd=T,srt=90)
text(x=min(xk_ne_yr$FM_change)-0.32,y=mean(Lc_xk_ne),expression(Lc["2014"]),cex=1.2)
par(xpd=T,srt=360)
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
F1<-na.exclude((data$Autor[data$F==F_xk_se[1] & data$Regiao!= ""]))
F2<-na.exclude((data$Autor[data$F==F_xk_se[2] & data$Regiao!= ""]))
# Lc 
Lc1<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_xk_se[1] & data$Regiao!= ""]))
Lc2<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_xk_se[2] & data$Regiao!= ""]))
Lc3<-na.exclude((data$Autor[data$Lc.mm.TL/10==Lc_xk_se[3] & data$Regiao!= ""]))

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
xk_se_yr<-predict_mod(xk_se_list, FM_change = seq(0,4,0.1),
                       Lc_change=seq(4,12,0.1),type="ypr")

plot(xk_se_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="",xlim=c(0,11),xlab="")
title("X. kroyeri_SE", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

#F information
lines(x=c(F_xk_se[1],F_xk_se[1]),y=c(min(xk_se_yr$Lc),mean(Lc_xk_se)),lty=3,lwd=1)
lines(x=c(min(xk_se_yr$FM_change),F_xk_se[1]),y=c(mean(Lc_xk_se),mean(Lc_xk_se)),lty=3,lwd=1)
text(x=F_xk_se[1],y=0.89*min(xk_se_yr$Lc),expression(F[2014]),cex=1.2)
#Lc
par(xpd=T,srt=90)
text(x=min(xk_se_yr$FM_change)-0.22,y=mean(Lc_xk_se),expression(Lc["avg"]),cex=1.2)
par(xpd=T,srt=360)
#----------------------------------------------------------------------------------

# X.kroyeri S #
#-------------------------------------------------------------------------------------------
#Ages for Lr and Lc  Vonbertalanffy t <- t0 - (log(1 - L/Linf[1])/K[1])
Tr_xk_s <- 0 - (log(1 - Lr_xk_s/mean(Linf_xk_s))/mean(K_xk_s))
Tc_xk_s<- 0 - (log(1 - Lc_xk_s/mean(Linf_xk_s))/mean(K_xk_s))
#-------------------------------------------------------------------------------------------

cat("\n","Input Parameters X.kroyeri S","\n","Linf =",mean(Linf_xk_s),"& sd=",sd(Linf_xk_s),"\n",
    "Winf=",mean(Winf_xk_s),"& sd=",sd(Winf_xk_s),"\n","K=",mean(K_xk_s),"& sd=",sd(K_xk_s),"\n", 
    "L50=",mean(L50_xk_s),"& sd=",sd(L50_xk_s),"\n","Lr=",mean(Lr_xk_s),"& sd=",sd(Lr_xk_s),"\n",
    "Lc=",mean(Lc_xk_s),"& sd=",sd(Lc_xk_s),"\n","M=",mean(M_xk_s),"& sd=",sd(M_xk_s),"\n","T0=",(0),"\n",
    "F=",mean(F_xk_s),"& sd=",sd(F_xk_s),"\n","Tmax=",mean(Tmax_xk_s),"& sd=",sd(Tmax_xk_s),"\n",
    "Tr=",mean(Tr_xk_s),"& sd=",sd(Tr_xk_s),"\n","Tc=",mean(Tc_xk_s),"& sd=",sd(Tc_xk_s) ,"\n")

#Mortality
cat("None mortality information")
#--------------------------------------------------------------------------------------

xk_s_list<- list(Winf=mean(Winf_xk_s),Linf = mean(Linf_xk_s), K = mean(K_xk_s), t0 =0,
                                              M= mean(M_xk_s), tr =mean(Tr_xk_s))

#--------------------------------------------------------------------------------------
#graphics isopleth
xk_se_yr<-predict_mod(xk_s_list, FM_change = seq(0,5,0.1),
                       Lc_change=seq(1,9,0.1),type="ypr")

plot(xk_se_yr, yaxis2 = "n", type = "Isopleth", yaxis1= "Y_R",ylab1="",xlim=c(0,11),xlab ="")
title("X. kroyeri_S", line = 0.2, adj=0.95, font.main= 3,cex.main=0.9)

par(las=0)
mtext(side=2, text="First catch length (Lc)(cm)", outer=T,cex=0.9,line=0.45)
mtext(side=1, text=expression("Fishing mortality (F)"*(year^-1)), outer=T,cex=0.9,line=0.6)
#dev.off()
}
##x#x#xx#x#x#x#x#x#x#x#x End of yield per recruit #x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x

