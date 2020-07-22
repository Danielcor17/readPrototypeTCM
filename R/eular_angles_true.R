#' Calculate eular angles
#'
#' This function calculates eular angles from IMU data and
#' returns a dataframe with associated eular angles.
#'
#' @param IMU A data frame with raw IMU data
#' @return A dataframe with associated eular angles.

eular_angles_true = function(IMU) {

  Phi = atan2( IMU$Ay, IMU$Az + IMU$Ax * 0.001 )
  Gz2 = IMU$Ay * sin( Phi ) + IMU$Az * cos( Phi )
  Theta = atan( -IMU$Ax / Gz2)
  By2 = IMU$Mz * sin( Phi ) - IMU$My * cos( Phi )
  Bz2 = IMU$My * sin( Phi ) + IMU$Mz * cos( Phi )
  Bx3 = IMU$Mx * cos( Theta ) + Bz2 * sin( Theta )
  Psi = atan2( By2 , Bx3)

  roll<- 180*Phi/pi

  pitch<- 180*Theta/pi

  yaw= 180*Psi/pi

  #yaw = ifelse(yaw < 0, yaw + 360, yaw)

  eular_data = data.frame(roll,pitch,yaw)
  colnames(eular_data) = c("Roll", "Pitch", "Yaw")

  return(eular_data)
}
