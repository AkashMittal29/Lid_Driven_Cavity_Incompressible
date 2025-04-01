
PROGRAM main
    USE mod_parameters
    USE mod_solver

    IMPLICIT NONE
    TYPE(solver_incompressible) :: incompressible

    ! Solution
    CALL incompressible%read_input('input.ini')
    CALL incompressible%construct()
    CALL incompressible%initialize_grid()
    CALL incompressible%initialize_field()
    CALL incompressible%solve()
END PROGRAM main


