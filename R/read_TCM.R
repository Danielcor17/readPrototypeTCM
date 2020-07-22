#' Read TCM data files
#'
#' This function reads TCM data files and exports .csv files
#' with sensor data and current profile data appended.
#'
#' @param TCM_data_dir Path to the input file
#' @param data_file_name full name of .csv file
#' @param burst_rate burst interval used during data collection
#' @return A list of data from binary files
#' @export

##### Read Binary data from TCM's #####
#
#
####

read_TCM = function(TCM_data_dir, data_file_name, burst_rate) {

   #TCM_data_dir = "~/Documents/vocational/Current Meter/myTCM/myTCM tests/Calibrations/TCM_1 cal/bench/xl cal data"
   #data_file_name = "TCM_1 data.csv"
   #burst_rate = 1

  #
  # Input path to root data directory and
  #  list all data files within sub directories
  #  Handle user data file name
  #

  my_dir = getwd()
  data_dir = TCM_data_dir
  bin_files = dir(path = data_dir, pattern = "\\.dat$", full.names = TRUE, recursive = TRUE)
  data_file = unlist(strsplit(data_file_name,".", fixed = T))

  #
  # Create lists for data objects per data file
  # size = file total byte size
  # steps =  file size / data packet size (i.e. number of data packets)
  # bin_matrix = binary packet matrix stacked row by row
  # data_matrix = interpreted data matrix
  #

  size= list()
  steps= list()
  bin_matrix= list()
  data_matrix= list()
  packet1 = rep(1,8)
  packet2 = rep(2,6)
  packet3 = rep(4,11)

  #
  # packet 1/2/3 = subpackets of data packets
  # packet_sum = subpacket total size
  # packet1/2/3_read = index for reading byte by byte
  #

  packet_sum = sum(sum(packet1) + sum(packet2) + sum(packet3))
  packet1_read = 1 : sum(packet1)
  packet2_read = (1 + sum(packet1)) : (sum(packet1) + sum(packet2))+0
  packet3_read = (1 + sum(packet1) + sum(packet2)):(sum(packet1) + sum(packet2) + sum(packet3))+0

  #
  # Loop through each data file and translate binary data into list
  #

  for(i in seq_len(length(bin_files))) {

    #i=1

    size[i] = file.size(bin_files[i])
    steps[i] = ceiling(size[[i]]/(packet_sum+0))
    bin_matrix[[i]] = matrix(readBin(bin_files[i], "raw", n=size[[i]]), nrow = steps[[i]], ncol = packet_sum+0, byrow = TRUE)
    data_matrix[[i]] = matrix( nrow = steps[[i]], ncol = 25)

    for(q in seq_len( steps[[i]] )) {

      data_matrix[[i]][q,] = c(readBin(bin_matrix[[i]][q,][packet1_read],
                                integer(),
                                n= length(packet1),
                                size= 1,
                                endian= "little"),
                        readBin(bin_matrix[[i]][q,][packet2_read],
                                integer(),
                                n = length(packet2),
                                size = 2,
                                endian= "little") ,
                        readBin(bin_matrix[[i]][q,][packet3_read],
                                double(),
                                n = length(packet3),
                                size = 4,
                                endian = "little"))
    }
  }

  #
  # Aggregate all list to master data file and apply variable names
  #

  raw_data = data.frame(do.call(rbind,data_matrix))
  colnames(raw_data) = c("SD_stat","Vbat_stat","Delay_stat",
                         "DS3231_stat","TSYS01_stat","MS5837_stat","LSM303_stat","TCM #",
                         "Year","Month","Day","Hour","Minute","Second",
                         "Volt","DS3231_temp","tsys01_temp","MS5837_temp","Pressure",
                         "Ax","Ay","Az","Mx","My","Mz")

  error_codes = data.frame( cbind(any(raw_data$SD_stat ==1),
                                  any(raw_data$Delay_stat ==1) &
                                  (sum(raw_data$Delay_stat == 1) != burst_rate),
                                  any(raw_data$Vbat_stat ==1), any(raw_data$DS3231_stat ==1),
                                  any(raw_data$TSYS01_stat ==1),any(raw_data$MS5837_stat ==1),
                                  any(raw_data$LSM303_stat ==1)) )
  colnames(error_codes) = c("SD_error", "Delay_error","Vbat_error","DS3231_error","TSYS01_error","MS5837_error","LSM303_error")

  if(any(raw_data$Delay_stat == 1)){
    avg_data= data.frame(sapply(raw_data[-c(which(raw_data$Delay_stat==1)) ,-c(1:7)], function(x) colMeans(matrix(x, nrow=burst_rate))))
  } else{
    avg_data= data.frame(sapply(raw_data[ ,-c(1:7)], function(x) colMeans(matrix(x, nrow=burst_rate))))
    }

  avg_data[,c(2:7)] = ceiling(avg_data[,c(2:7)])

  avg_data = cbind(avg_data,eular_angles_true(avg_data))
  avg_data = cbind(avg_data,current_profiles(avg_data))

  raw_data_file = paste( paste( data_file[1],"raw" ),".",data_file[2], sep="" )
  avg_data_file = paste( paste( data_file[1],"avg" ),".",data_file[2], sep="" )

  write.csv(raw_data[,-c(1:7)], file= paste(data_dir,raw_data_file, sep = "/") )
  write.csv(avg_data, file= paste(data_dir,avg_data_file, sep = "/") )

  remarks_file = paste(data_file[1],"remarks.txt")

  cat( paste(data_file[1], "remarks\n\n") ,file = paste(data_dir, remarks_file,sep="/") )

  if(any(error_codes==TRUE)) {

    errors = c(which(error_codes == TRUE))

    for (i in seq_along(length(errors))) {
      cat( paste(colnames(error_codes[errors[i]]),"detected\n"),file = paste(data_dir, remarks_file,sep="/"), append = T )
    }

    cat( "\nPlease review data before using\n",file = paste(data_dir, remarks_file,sep="/"), append = T )

  } else {
    cat( "No errors to report",file = paste(data_dir, remarks_file,sep="/"), append = T)
  }

  return(list(raw_data,avg_data))
}




