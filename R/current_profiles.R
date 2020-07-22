#' Calculate current profiles
#'
#' This function calculates current profiles based on IMU data.
#'
#' @param IMU A dataframe with raw IMU and eular angle data.
#' @return A dataframe with associated current profile data.

current_profiles = function(IMU){

  model = list("coefficients")

  model$coefficients [1] = 3.107205e-01
  model$coefficients[2] = 9.298555e-01
  model$coefficients[3] = -6.470836e-02
  model$coefficients[4] = 3.722014e-03
  model$coefficients[5] = -6.360694e-05
  model$coefficients[6] = -4.933006e-06
  model$coefficients[7] = 3.178334e-07
  model$coefficients[8] = -7.596240e-09
  model$coefficients[9] = 8.238378e-11
  model$coefficients[10] = -3.240198e-13

  acc_norm = sqrt(IMU$Ax^2 + IMU$Ay^2 + IMU$Az^2)
  v_estimate = sqrt( (IMU$Ax/acc_norm) ^ 2 +  (IMU$Ay/acc_norm) ^ 2 ) * (180 / pi)
  velocity = model_func(model,v_estimate)

  b_estimate<-atan2(IMU$Roll,- IMU$Pitch) * (180 / pi)
  bearing<-ifelse((b_estimate + IMU$Yaw) < 0, 360 + (b_estimate + IMU$Yaw), (b_estimate + IMU$Yaw))

  current_profile = data.frame(velocity,bearing)
  colnames(current_profile) = c("Velocity", "Bearing")

  return(current_profile)
  }
