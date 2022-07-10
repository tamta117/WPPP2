#function to process checked camera trap files with no cougars

coug_bin0<-function(fname){
  cam<-read.csv(fname)%>%
    mutate(cam=ClusterID,
           nobs=0)%>%
    select(cam, nobs)
  cam2<-cam[1,]
}

#test function
test<-coug_bin0("data/cam/MVC202F-2588-Reconyx_Checked.csv")
