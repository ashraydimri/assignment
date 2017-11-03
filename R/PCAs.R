#' @export
PCAs<-function(a,b)
{
  #library(mice)
  inm<-data.frame( read.csv(a))
  ing<-data.frame(read.csv(b))
  
  tr<-ing[,c(-1,-2)]
  summary(tr)
  indx<- sapply(tr,is.factor)
  tr[indx] <- lapply(tr[indx], function(x) as.numeric(as.character(x)))
  #library(mice)
  imputed_Data <- mice::mice(tr, m=2, maxit = 5, method = 'pmm', seed = 500)
  cd <- mice::complete(imputed_Data,2)
  pc<-prcomp(cd, center=TRUE, scale.=TRUE)
  
  #pairs.panels(pc$x,
  #gap=0,
  #pch=21)
  
  #library(ggbiplot)
  #library(plotly)
  g<- ggbiplot::ggbiplot(pc,
               obs.scale = 1,
               var.scale = 1,
               elipse=TRUE,
               circle=TRUE)+ ggplot2::geom_point(shape=1) 
  
  g<-g+ scale_color_discrete(name = '')
  g<-g+ theme(legend.direction = 'horizontal',
              legend.position = 'top')
  g<-plotly::ggplotly(g)
  print(g)
  
}