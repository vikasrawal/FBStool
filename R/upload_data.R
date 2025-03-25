upload_data=function(input, output, session){
  

  get_data<-reactive({
    inFile=input$file
    
    if (input$uploadfile==0){return()}
    else {
      if(!is.null(inFile))
        
      {
        data=read.csv(file=inFile$datapath, header = input$header)
        data=setDT(data)
        
        return(data)
      }
      
      else {return(NULL)}
      
    }
    
  })
  

}