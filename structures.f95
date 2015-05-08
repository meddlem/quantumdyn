module structures
  use constants
  implicit none

  type modl_par 
    ! data type that contains all model parameters 
    
    real(dp) :: k
    real(dp) :: L ! lattice length 
    real(dp) :: dx 
    real(dp) :: dt
    real(dp) :: tau  ! time constant potential

    integer      :: M ! lattice points in x-dir
    integer      :: N  ! number of time steps/iterations
    character(3) :: sim_type 
  end type
  
  type plt_par
    integer  :: plot_interval ! number of iterations between plots
    real(dp) :: rng(2) ! min and max y values for plot
    logical  :: plot_re
  end type
end module
