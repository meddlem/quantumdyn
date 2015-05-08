module constants
  implicit none
  ! module contains all constants used in the program
  
  ! dp: compiler specific kind for double precision float
  ! lng: compiler specific kind for long integer

  ! NOTE: IF YOU MAKE ANY CHANGES HERE RECOMPILE ALL MODULES: "make -B" 
  integer, parameter :: dp = selected_real_kind(15,307)
  integer, parameter :: lng = selected_int_kind(8)

  ! number constants
  real(dp), parameter :: pi = 4._dp*atan(1._dp)
  complex(dp), parameter :: zero = (0._dp, 0._dp)
  complex(dp), parameter :: one = (1._dp, 0._dp)
  complex(dp), parameter :: i_u = (0._dp, 1._dp)
end module
