######### Standard Functions ##########


sample_dat = function(dat, rows = 10){
  return(dat[sample(1:nrow(dat), rows),])
}

install_and_load = function(library){
  if(!is.character(library)){
    message("Please type library name as a character value")
    return(NULL)
  }
  if(!require(library, character.only = T)){
    install.packages(library)
    require(library, character.only = T)
  }
  return(NULL)
}

####### Loading Libraries ###########



install_and_load('httr')

test = httr::GET('https://sanjose.custhelp.com/services/rest/connect/v1.3/analyticsReportResults', 
          config = list(authenticate(user = 'arti.tangri', password = 'MyS@nJose5')))





