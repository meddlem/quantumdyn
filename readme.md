# 1d quantum dynamics simulation

Time integration of the 1d Schrodinger equation for a gaussian wavepacket, using the Crank-Nicolson method. 
The resulting systems are solved using the LAPACK routine zgtsv for complex tridiagonal matrices. 
The result of the simulation is an animation time evolution of the density (default), or the real part of the wavefunction (use flag: -r).
##3 simulations can be selected by the user: 

1. Default: Harmonic potential
2. Use -t flag for square barrier
3. Use -a flag for adiabatic potential variation: Harmonic potential to ISQW

Example: $./main -t
