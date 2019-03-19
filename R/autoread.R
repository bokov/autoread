library(readr); library(readxl); library(data.table);

autoread <- function(file,na=c('','.','(null)','NULL','NA'),...){
  args <- list(...);
  # check for text formats
  if(nrow(enc<-guess_encoding(file))>0){
    # try to read as a delimited file via fread
    txargs <- args[intersect(names(args),names(formals(fread)))];
    txargs$na.strings <- na;
    out <- try(as_tibble(do.call(fread,c(list(input=file),txargs)))
               ,silent = T);
    if(!is(out,'try-error')) return(out);
    cat('\nGuessed encoding:\n');print(enc);
    stop(attr(out,'condition')$message);
  }
  # try various binary formats
  xlargs <- args[intersect(names(args),names(formals(read_xls)))];
  xlargs$na <- na;
  # xlsx
  sheets <- try(.Call('readxl_xlsx_sheets',PACKAGE='readxl',file),silent=F);
  if(!is(sheets,'try-error')){
    if(length(sheets)>1 && !'sheet' %in% names(xlargs)) warning(
      "\nMultiple sheets found:\n",paste(sheets,collapse=', ')
      ,"\nReading in the first sheet. If you want a different one"
      ,"\nplease specify a 'sheet' argument")
    return(do.call(read_xlsx,c(list(path=file),xlargs)));}
  # xls
  sheets <- try(.Call('readxl_xls_sheets',PACKAGE='readxl',file),silent=F);
  if(!is(sheets,'try-error')){
    if(length(sheets)>1 && !'sheet' %in% names(xlargs)) warning(
      "Multiple sheets found: ",paste(sheets,collapse=', ')
      ,"\nReading in the first sheet. If you want a different one"
      ,"\nplease specify a 'sheet' argument")
    return(do.call(read_xls,c(list(path=file),xlargs)));}
  # need to unzip the file?
  out <- try(unzip(file,list=T));
  if(!is(out,'try-error')){
    if(length(zfiles <- out$Name[out$Length>0])==1){
      out <- unzip(file,files=zfiles,exdir=tempdir());
      return(autoread(out,na=na,...));
    } else {
      print(zfiles);
      stop('Please unzip the input file and run autoread() 
           on the individual files inside it.');
    }
  }
  message('\nUnknown file type?\n');
  stop(attr(out,'condition')$message);
}
