#' @title read_sim_networks
#'
#' @description Reads in all simulated networks (.txt files) into R as network files from the working directory.
#' @param searchname Search term - term to extract all simulated networks.
#' @export
#' @return network list
#' @examples\donttest{
#' net_list<-read_sim_networks("PNET_Network_A_")
#' }
read_sim_networks<-function(searchname){
  files <- list.files(pattern = searchname)
  NetList<-list()
  for (z in 1:length(files)){
    H<-files[z]
    R<-readPNetStatistics(H)
    net<-network::as.network(R)
    NetList[[z]]<-net
  }
  return(NetList)
  }
