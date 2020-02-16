# dl and extract data
{
  # download April_Sales.zip if not there
  if(!file.exists("output/April_Sales.zip")) {
    download.file("https://public.dhe.ibm.com/software/data/sw-library/cognos/mobile/C11/data/April_Sales.zip", 
                  destfile = "output/April_Sales.zip")
  }
  
  # extract April_Sales.zip if not done
  if(!file.exists("output/April_Sales")) {
    unzip("output/April_Sales.zip", 
          exdir = "output/April_Sales")
  }
}
lll <- 2