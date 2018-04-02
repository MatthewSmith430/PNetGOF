#' @title deg_dist_plot_directed
#'
#' @description Creates degree distribution plots for directed networks
#' @param NetList Network list created using read_sim_networks function
#' @param observed_name Observed network .txt file (used in PNet)
#' @export
#' @return degree distribution plots
#' @examples\donttest{
#' degplots<-deg_dist_plot_directed(NetList,"Oberved Directed Network.txt")
#' }
deg_dist_plot_directed<-function(NetList,observed_name){

  OBS<-utils::read.table(observed_name)
  OBS<-as.matrix(OBS)
  OBSdd<-ergm::degreedist(network::as.network(OBS))
  OBSdd<-as.data.frame(OBSdd)

  OBSdd<-tibble::rownames_to_column(OBSdd,var="NAME")

  OBout<-dplyr::filter(OBSdd, grepl('outdegree', OBSdd$NAME))
  OBin<-dplyr::filter(OBSdd, grepl('indegree', OBSdd$NAME))
  OBout$NAME<-NULL
  OBin$NAME<-NULL

  OBSmeltOut<-reshape::melt(OBout)
  OBSmeltOut$variable<-gsub("idegree","",OBSmeltOut$variable)
  ORobsdegOut<-gtools::mixedsort(unique(OBSmeltOut$variable))
  OBSmeltOut$factor<-factor(OBSmeltOut$variable, levels=c(as.character(ORobsdegOut)))

  OBSmeltIn<-reshape::melt(OBin)
  OBSmeltIn$variable<-gsub("idegree","",OBSmeltIn$variable)
  ORobsdegIn<-gtools::mixedsort(unique(OBSmeltIn$variable))
  OBSmeltIn$factor<-factor(OBSmeltIn$variable, levels=c(as.character(ORobsdegIn)))

  DDlist<-list()

  for (j in 1:length(NetList)){
    netJ<-NetList[[j]]
    DD<-ergm::degreedist(netJ)
    DD<-as.data.frame(DD)
    DDlist[[j]]<-DD
    }

  DDdf<-plyr::ldply(DDlist,data.frame)

  ROWlist<-list()
  for (u in 1:length(DDlist)){
    R<-rownames(DDlist[[u]])
    R1<-paste0(R[1],"_",u)
    R2<-paste0(R[2],"_",u)
    RR<-c(R1,R2)
    ROWlist[[u]]<-RR
    }

  ROW1<-unlist(ROWlist)
  ROW1<-as.vector(ROW1)
  rownames(DDdf)<-ROW1

  DDdf2<-tibble::rownames_to_column(DDdf,var="NAME")
  DDin<-dplyr::filter(DDdf2, grepl('indegree', DDdf2$NAME))
  DDout<-dplyr::filter(DDdf2, grepl('outdegree', DDdf2$NAME))

  DDin$NAME<-NULL
  DDout$NAME<-NULL

  meltDataOut <- reshape::melt(DDout)
  mdOut<-dplyr::filter(meltDataOut,meltDataOut$value>0)
  mdOut$variable<-as.character(mdOut$variable)
  mdOut$variable<-gsub("idegree","",mdOut$variable)
  mdOut$variable<-as.character(mdOut$variable)
  ORmddegOut<-gtools::mixedsort(unique(mdOut$variable))
  mdOut$factor<-factor(mdOut$variable, levels=c(as.character(ORmddegOut)))


  meltDataIn <- reshape::melt(DDin)
  mdIn<-dplyr::filter(meltDataIn,meltDataIn$value>0)
  mdIn$variable<-as.character(mdIn$variable)
  mdIn$variable<-gsub("idegree","",mdIn$variable)
  mdIn$variable<-as.character(mdIn$variable)
  ORmddegIn<-gtools::mixedsort(unique(mdIn$variable))
  mdIn$factor<-factor(mdIn$variable, levels=c(as.character(ORmddegIn)))

  pOut <- ggplot2::ggplot(mdOut, ggplot2::aes(mdOut$factor, mdOut$value))
  pOut2<-pOut +
    ggplot2::stat_boxplot(geom ='errorbar') +
    ggplot2::geom_boxplot()+
    ggplot2::theme_bw()+
    ggplot2::ggtitle("Out-degree Distribution") +
    ggplot2::xlab("Out-degree") + ggplot2::ylab("Frequency")+
    ggplot2::geom_point(data=OBSmeltOut,ggplot2::aes(OBSmeltOut$factor, OBSmeltOut$value),colour="red")+
    ggplot2::geom_line(data=OBSmeltOut,ggplot2::aes(OBSmeltOut$factor, OBSmeltOut$value,group = 1),colour="red")

  pIn <- ggplot2::ggplot(mdIn, ggplot2::aes(mdIn$factor, mdIn$value))
  pIn2<-pIn +
    ggplot2::stat_boxplot(geom ='errorbar') +
    ggplot2::geom_boxplot()+
    ggplot2::theme_bw()+
    ggplot2::ggtitle("In-degree Distribution") +
    ggplot2::xlab("In-degree") + ggplot2::ylab("Frequency")+
    ggplot2::geom_point(data=OBSmeltIn,ggplot2::aes(OBSmeltIn$factor, OBSmeltIn$value),colour="red")+
    ggplot2::geom_line(data=OBSmeltIn,ggplot2::aes(OBSmeltIn$factor, OBSmeltIn$value,group = 1),colour="red")


  pOut2
  pIn2

  plotlist<-list(pOut2,pIn2)

  return(plotlist)





}
