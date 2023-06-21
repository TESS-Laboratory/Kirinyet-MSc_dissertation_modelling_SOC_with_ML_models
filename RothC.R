#RothC model for a site:https://www.bgc-jena.mpg.de/TEE/basics/2015/11/19/RothC/

#Asuume this climate datasets
Temp=data.frame(Month=1:12, Temp=c(-0.4, 0.3, 4.2, 8.3, 13.0, 15.9,
18.0, 17.5, 13.4, 8.7, 3.9,  0.6))
Precip=data.frame(Month=1:12, Precip=c(49, 39, 44, 41, 61, 58, 71, 58, 51,
                                       48, 50, 58))
Evp=data.frame(Month=1:12, Evp=c(12, 18, 35, 58, 82, 90, 97, 84, 54, 31,
                                 14, 10))

#percent clay content and annual litter inputs
soil.thick=25  #Soil thickness (organic layer topsoil), in cm
SOC=69.7       #Soil organic carbon in Mg/ha 
clay=48        #Percent clay
Cinputs=2.7   #Annual C inputs to soil in Mg/ha/yr

#Run simulations for 50 years into the future using this information
years=seq(1/12,500,by=1/12) 

#effects of climate on decomposition calculation using fT.RothC and fW.RothC
fT=fT.RothC(Temp[,2]) #Temperature effects per month
fW=fW.RothC(P=(Precip[,2]), E=(Evp[,2]), 
            S.Thick = soil.thick, pClay = clay, 
            pE = 1.0, bare = FALSE)$b #Moisture effects per month

xi.frame=data.frame(years,rep(fT*fW,length.out=length(years)))
FallIOM=0.049*SOC^(1.139) #IOM using Falloon method
Model1=RothCModel(t=years,C0=c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=FallIOM),
                  In=Cinputs, clay=clay, xi=xi.frame) #Loads the model

Ct1=getC(Model1) #Calculates stocks for each pool per month
matplot(years, Ct1, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topleft", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize1=as.numeric(tail(Ct1,1))
names(poolSize1)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize1

DOC=1.9       #Dissolved organic carbon in Mg/ha
SA=4.7        #OM in sand plus aggregates in Mg/ha
POM=5.9       #Particulate organic matter in Mg/ha
sc=53.8        #OM in silt plus clay in Mg/ha
rSOC=3.4      #Recalcitrant soil organic carbon in Mg/ha

DPMplusRPM=DOC+POM
RPM=0.888*(DPMplusRPM)
DPM=0.112*(DPMplusRPM)

BIOplusHUM=SA+sc
HUM=0.974*(BIOplusHUM)
BIO=0.026*(BIOplusHUM)
ZimmIOM=rSOC #IOM following Zimmermann's method

Model2=RothCModel(t=years,C0=c(DPM, RPM, BIO, HUM, ZimmIOM),
                  In=Cinputs,DR=1.44,clay=clay,xi=xi.frame) #Loads the model
Ct2=getC(Model2) #Calculates stocks for each pool per month

matplot(years, Ct2, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize2=as.numeric(tail(Ct2,1))
names(poolSize2)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize2

RPMptf=(0.184*SOC + 0.1555)*(clay + 1.275)^(-0.1158)
HUMptf=(0.7148*SOC + 0.5069)*(clay + 0.3421)^(0.0184)
BIOptf=(0.014*SOC + 0.0075)*(clay + 8.8473)^(0.0567)

DPMptf=SOC-FallIOM-RPMptf-HUMptf-BIOptf
c(DPMptf, RPMptf, BIOptf, HUMptf, FallIOM)

Model3=RothCModel(t=years,C0=c(DPMptf, RPMptf, BIOptf, HUMptf, FallIOM),
                  In=Cinputs,clay=clay,xi=xi.frame) #Loads the model
Ct3=getC(Model3) #Calculates stocks for each pool per month

matplot(years, Ct3, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="C stocks (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize3=as.numeric(tail(Ct3,1))
names(poolSize3)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize3