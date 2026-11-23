! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
!

! BEGIN DART PREPROCESS TYPE DEFINITIONS
! METAR_U_10_METER_WIND,           QTY_U_WIND_COMPONENT,       COMMON_CODE
! METAR_V_10_METER_WIND,           QTY_V_WIND_COMPONENT,       COMMON_CODE
! METAR_TEMPERATURE_2_METER,       QTY_TEMPERATURE,            COMMON_CODE
! METAR_SPECIFIC_HUMIDITY_2_METER, QTY_SPECIFIC_HUMIDITY,      COMMON_CODE
! METAR_SURFACE_PRESSURE,          QTY_SURFACE_PRESSURE,       COMMON_CODE
! METAR_POT_TEMP_2_METER,          QTY_POTENTIAL_TEMPERATURE,  COMMON_CODE
! METAR_CLOUD_AMOUNT,              QTY_CLOUD_FRACTION
! END DART PREPROCESS TYPE DEFINITIONS

! BEGIN DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE
!  use obs_def_metar_mod, only : get_expected_cloud_okta
! END DART PREPROCESS USE OF SPECIAL OBS_DEF MODULE

! BEGIN DART PREPROCESS GET_EXPECTED_OBS_FROM_DEF
!  case(METAR_CLOUD_AMOUNT)
!     call get_expected_cloud_okta(state_handle, ens_size, location, expected_obs, istatus)
! END DART PREPROCESS GET_EXPECTED_OBS_FROM_DEF

! BEGIN DART PREPROCESS READ_OBS_DEF
!   case(METAR_CLOUD_AMOUNT)
!      continue
! END DART PREPROCESS READ_OBS_DEF

! BEGIN DART PREPROCESS WRITE_OBS_DEF
!   case(METAR_CLOUD_AMOUNT)
!      continue
! END DART PREPROCESS WRITE_OBS_DEF

! BEGIN DART PREPROCESS INTERACTIVE_OBS_DEF
!   case(METAR_CLOUD_AMOUNT)
!      continue
! END DART PREPROCESS INTERACTIVE_OBS_DEF

! BEGIN DART PREPROCESS MODULE CODE
module obs_def_metar_mod

use        types_mod, only : r8, missing_r8
use    utilities_mod, only : register_module
use     location_mod, only : location_type
use  assim_model_mod, only : interpolate
use     obs_kind_mod, only : QTY_CLOUD_FRACTION
use  ensemble_manager_mod, only : ensemble_type
use obs_def_utilities_mod, only : track_status

implicit none
private

public :: get_expected_cloud_okta

! version controlled file description for error handling, do not edit
character(len=256), parameter :: source   = &
   "$URL$"
character(len=32 ), parameter :: revision = "$Revision$"
character(len=128), parameter :: revdate  = "$Date$"

logical, save :: module_initialized = .false.

contains

! ---------------------------------------------------

subroutine initialize_module

! Handle any module initialization tasks

if (module_initialized) return

call register_module(source, revision, revdate)
module_initialized = .true.

end subroutine initialize_module

! ---------------------------------------------------

subroutine get_expected_cloud_okta(state_handle, ens_size, location, okta, istatus)

type(ensemble_type), intent(in)  :: state_handle
integer,             intent(in)  :: ens_size
type(location_type), intent(in)  :: location
real(r8),            intent(out) :: okta(ens_size)
integer,             intent(out) :: istatus(ens_size)

real(r8) :: cld_frac(ens_size)   ! raw cloud fraction
integer  :: this_istatus(ens_size)
logical  :: return_now

if ( .not. module_initialized ) call initialize_module

istatus = 0   ! to use track_status, it must start out 0

! Raw cloud fraction at this location - this calls the model_mod code.
call interpolate(state_handle, ens_size, location, QTY_CLOUD_FRACTION, cld_frac, this_istatus)
call track_status(ens_size, this_istatus, okta, istatus, return_now)
if (return_now) return

! The actual forward operator computation.  This is the value that
! will be returned.  istatus (the return code) of 0 is good,
! any value > 0 indicates an error.  (values < 0 reserved for
! system use.)

! Okta definitions: https://worldweather.wmo.int/oktas.htm

! Initial pass: Convert to nearest 1/8
where (istatus == 0)
   okta = anint(cld_frac * 8.0) / 8.0
end where

! Small, positive cloud fractions near 0 are assigned okta = 1
where (istatus == 0 .and. cld_frac > 0.0 .and. okta < 0.001)
  okta = 0.125
end where

! Large cloud fractions that are not quite 1 are assigned okta = 7
where (istatus == 0 .and. cld_frac < 1.0 .and. okta > 0.999)
  okta = 0.875
end where

! Catch instances where cld_frac may be slightly negative
where (istatus == 0 .and. cld_frac <= 0.0)
  okta = 0.0
end where

! Catch instances where cld_frac may be slightly above 1
where (istatus == 0 .and. cld_frac >= 1.0)
  okta = 1.0
end where

end subroutine get_expected_cloud_okta

! ---------------------------------------------------

end module obs_def_metar_mod
! END DART PREPROCESS MODULE CODE
