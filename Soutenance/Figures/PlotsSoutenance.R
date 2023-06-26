rm(list=ls())
source("C://Users/mathieu.lucas/Desktop/GitMat/PropagMaxAn/Codes/dirs.R")

dir.plots.soutenance = "C://Users/mathieu.lucas/Desktop/GitMat/Redac/Soutenance/Figures/"

dir.res.stoods = paste0(dir.res,"/GeV_Amax")
nyears = c(50,100,150,205)
nspag = dim(SpagsHydro)[2] ; nsim = 5000
SpagsHydro = read.table(paste0(dir.res,"spags_uTot_Amax.txt"))
QuantHydro = read.table(paste0(dir.res,"Quantiles_Amax.txt"),header=T)


case = 4
  dir.case = paste0(dir.res.stoods,"/",nyears[case],"y")
  QuantAll  = read.table(paste0(dir.case,"/Quants",case,".txt"),header = F)
  names(QuantAll) = c("Pr","Mp","Qhyd_2","Qhyd_9","Qtot_2","Qtot_9")
  QuantGG = data.frame(QuantAll ,case = nyears[case])
  FreqAll  = read.table(paste0(dir.case,"/Freqs",case,".txt"),header = F)
  names(FreqAll ) = c(names(QuantHydro),"Fr","Pr")
  FormTot  = read.table(paste0(dir.case,"/FormTot",case,".txt"), header = T)
  FormHyd  = read.table(paste0(dir.case,"/FormHyd",case,".txt"), header = T)
  
  QuantBoth = ggplot()+
    # geom_ribbon(data=QuantAll ,aes(x=Pr, ymin = Qtot_2, ymax=Qtot_9,
    #                                       fill="2-Total uncertainty : \n Streamflow + sampling"),alpha=0.8)+
    # geom_ribbon(data=QuantAll ,aes(x=Pr, ymin = Qhyd_2, ymax=Qhyd_9, 
    #                                       fill="1-Streamflow uncertainty"),alpha=1)+
    
    # geom_point(data = FreqAll[which(FreqAll$an > 1900),] , aes(x=Pr, y = mp), pch = 3, lwd = 4)+
    
    geom_point(data = FreqAll[which(FreqAll$an > 1900),], aes(x=Pr, y = mp))+
    
    # geom_line(data=QuantAll ,aes(x=Pr,y=Mp,col="Distribution GEV"), lwd = 1)+
    geom_errorbar(data= FreqAll[which(FreqAll$an > 1900),], aes(x=Pr,ymin = tot2.5, ymax = tot97.5))+
    scale_x_continuous(trans="log10")+
    xlab("Période de retour [années]")+
    ylab(expression(paste("Débit [",m^3,".",s^-1,"]",sep="")))+
    scale_fill_manual(name = element_blank(),
                      values = c("#fec44f","#67a9cf"))+
    scale_color_manual(values = "royalblue")+
    # geom_segment(aes(x=100, xend=100, y=0, yend=QuantAll$Mp[981]), size = 1.5, color = "grey", lty = 2)+
    # geom_segment(aes(x=0, xend=100, y=QuantAll$Mp[981], yend=QuantAll$Mp[981]), size = 1.5, color = "grey", lty = 2)+
    theme_bw(base_size=15)+
    # labs(title = paste0(min(FreqAll$an)," - ",max(FreqAll$an)))+
    ggtitle("Débits maximum annuels classés - Beaucaire (1900-2020)")+
    coord_cartesian(xlim=c(1,1000), ylim = c(3000,15000))+
    theme(legend.title=element_blank(),
          plot.title = element_text(size = 25),
          legend.position = c(0.8,0.2),
          axis.text=element_text(size=15),
          axis.title=element_text(size=20),
          legend.text=element_text(size=20),
          legend.key.size=unit(1, "cm"))
  
  # ggsave(path = dir.plots.soutenance, filename = "Qamax.pdf",
  #        device = "pdf", width = 12,height = 8,units = "in")
  
  # ggsave(path = dir.plots.soutenance, filename = "Qamax_GEV.pdf",
  #        device = "pdf", width = 12,height = 8,units = "in")
  
  # ggsave(path = dir.plots.soutenance, filename = "Qamax_GEV_Q100.pdf",
  #        device = "pdf", width = 12,height = 8,units = "in")
  
  # ggsave(path = dir.plots.soutenance, filename = "Qamax_uQ.pdf",
  #        device = "pdf", width = 12,height = 8,units = "in")

### plot uEch 
  ## generate 1000 gev estimates
  set.seed(3)
  Qgen = data.frame(index = 1:1000, Q=rgev(1000,540,150,-1*-0.1))
  Q100 = qgev(0.99,540,150,-1*-0.1)
  Q1000 = qgev(0.999,540,150,-1*-0.1)
  # plot(Qgen)
  # abline(a=Q100,b=0, col = "red")  
  # abline(a=Q1000,b=0, col = "red")http://127.0.0.1:43795/graphics/plot_zoom_png?width=1536&height=846
  
  
  
  ggplot(data = Qgen, aes(x=index,y=Q))+
    annotate("rect", xmin=422, xmax=422+60, ymin=-Inf, ymax=Inf, alpha=0.2, fill="brown1")+
    annotate("rect", xmin=260, xmax=260+60, ymin=-Inf, ymax=Inf, alpha=0.2, fill="brown1")+
    annotate("rect", xmin=940, xmax=1000, ymin=-Inf, ymax=Inf, alpha=0.2, fill="brown1")+
    geom_segment(aes(x = 260,xend=320,y=1167,yend=1167),col="red", lwd = 2)+
    geom_segment(aes(x = 422,xend=482,y=1542,yend=1542),col="red", lwd = 2)+
    geom_segment(aes(x = 940,xend=1000,y=1353,yend=1353),col="red", lwd = 2)+
    geom_point(color='royalblue', size = 2.5,alpha = 0.9)+
    geom_hline(aes(yintercept = Q100), lty = 2, lwd = 1.2, col = "grey")+
    geom_text(aes(x = 80, y = Q100+50, label= "Q100"), size = 7, color = "grey")+
    ylab(expression(paste("Débit simulé [",m^3,".",s^-1,"]",sep="")))+
    theme_bw(base_size = 20)+
    theme(axis.title.x = element_blank())+
    ggtitle("Débits simulés - GEV(500,100,0.1)")+
    coord_cartesian(xlim = c(30,970),ylim = c(200,2100))
  
  # ggsave(path = dir.plots.soutenance, filename = "Qsample1.pdf",
  #        device = "pdf", width = 12,height = 8,units = "in")
  # 
  # ggsave(path = dir.plots.soutenance, filename = "Qsample2.pdf",
  #        device = "pdf", width = 12,height = 8,units = "in")
  # 
  # 260:320
  # 1167
  # 
  # 422:482
  # 1542
  # 
  # 940:1000
  # 1353
  # 
  # Pos = parameter(name='Pos',init = 6000)  
  # Ech =  parameter(name='Ech',init = 1000) 
  # Form = parameter(name='Form',init = 0.01,priorDist='Gaussian',priorPar=c(0,0.3))
  # dat <- dataset(Y = data.frame(Qgen$Q[940:1000]))
  # mod <- model(dataset=dat, parentDist ='GEV', par=list(Pos,Ech,Form))
  # STooDs(mod,workspace = "C://Users/mathieu.lucas/Desktop/St", mcmcOptions = mcmc(Nsim = 5000, Nslim = 2))
  # MCMCres = readMCMC(file = file.path("C://Users/mathieu.lucas/Desktop/St","mcmc.txt"), burnFactor = 0.5, slimFactor = 2)
  # Mp.GeV = MCMCres[which.max(MCMCres$post),(1:3)]
  # Mp.Quant = qgev(p = 0.99,loc = Mp.GeV$Pos, scale = Mp.GeV$Ech,shape = -1*Mp.GeV$Form)
  
CX = read.csv2("C://Users/mathieu.lucas/Desktop/GitMat/CruesHisto/Data/CX_All.csv",header = T)
CX = CX[which(CX$An < 1800 & CX$Cat == "C4"),]  

CX = data.frame(An = c(1702,1708,1745,1785,1801), 
                Qmin = c(9100,9600,9400,10000,9400)-1000,
                Qmax = c(15000,16000,13800,12900,12200)+500)

require(ggthemes)

  ggplot()+
    #recent
    geom_errorbar(data=QuantHydro[27:75,], aes(x = an, ymin = tot2.5, ymax = tot97.5), color = "blue",
                  width = 0, lwd = 1.2)+
    geom_point(data=QuantHydro[27:75,], aes(x = an, y = mp),size = 2.5, color = "blue")+
    #histsyst
    geom_errorbar(data=QuantHydro[1:25,], aes(x = an, ymin = tot2.5, ymax = tot97.5),
                  width = 0,color="deepskyblue", lwd = .9)+
    geom_point(data=QuantHydro[1:25,], aes(x = an, y = mp),color="deepskyblue", size = 2)+
    # #histo
    geom_polygon(aes(x = c(1500,1815,1815,1500), y = c(0,0,8000,8000)),fill="lightgrey",alpha=0.5)+
    geom_segment(aes(x = 0, xend = 1815, y = 8000, yend = 8000),color="darkorange1", lwd = 1.2)+
    geom_errorbar(data = CX, aes(x = An, ymin = Qmin, ymax = Qmax), width = 1.8, lwd = 1.5, color = "black")+

    ylab(expression(paste("Débit [",m^3,".",s^-1,"]",sep="")))+
    theme_bw(base_size = 20)+
    theme(axis.title.x = element_blank(),axis.text.x = element_blank(),
          axis.title.y = element_blank(),axis.text.y = element_blank())+
    coord_cartesian(ylim = c(500,17000), xlim = c(1705,1882))
  
  
    # ggsave(path = dir.plots.soutenance, filename = "Serie1.pdf",
    #        device = "pdf", width = 12,height = 8,units = "in")
  
  
  