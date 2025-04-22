
MODULE mod_field
    USE mod_parameters
    USE mod_utility
    USE mod_grid
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : error_unit
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: field_type
    PUBLIC :: print_rms_diff_fields
    PUBLIC :: copy_field ! Not used in solver

    ! PUBLIC :: ASSIGNMENT(=) ! With ifort (intel) compiler, assignment operator can not be defined on allocatable or pointers. gfortran accepts it. 
    ! ! Interface block for assignment operator overloading
    ! INTERFACE ASSIGNMENT(=)
    !   MODULE PROCEDURE copy_field
    ! END INTERFACE

    TYPE :: field_type
        REAL(KIND=rkind), ALLOCATABLE, DIMENSION(:,:) :: u, v, p ! non-dim
        REAL(KIND=rkind) :: rho ! non-dim density (constant for incompressible flow)                                                       
        CLASS(grid_type), POINTER :: grid => NULL()
        
        CONTAINS
            PROCEDURE, PUBLIC,  PASS(self) :: construct
            PROCEDURE, PUBLIC,  PASS(self) :: printf
            PROCEDURE, PUBLIC,  PASS(self) :: set_bc
    END TYPE field_type


    CONTAINS
        ! Procedures of user defined type: field_type
        SUBROUTINE construct(self, grid)
            CLASS(field_type), INTENT(INOUT) :: self
            CLASS(grid_type), TARGET :: grid

            self%grid => grid ! Pointing to grid (associating to grid)

            ! Allocating field variables with staggered grid and ghost nodes
            ! u: staggered in x direction; top and bottom nodes not used; only for consistent indexing.
            IF(.NOT. ALLOCATED(self%u)) ALLOCATE( self%u(0:self%grid%nx,0:self%grid%ny+1) )
            ! v: staggered in y direction; left and right nodes not used; only for consistent indexing.
            IF(.NOT. ALLOCATED(self%v)) ALLOCATE( self%v(0:self%grid%nx+1,0:self%grid%ny) )
            ! p: defined on the nodes
            IF(.NOT. ALLOCATED(self%p)) ALLOCATE( self%p(0:self%grid%nx+1,0:self%grid%ny+1) )

            ! Initializing field variables with zeros
            self%u = 0.0 ! (u,v): divergence free field as initial condition
            self%v = 0.0
            self%p = 0.0
            self%rho = 1 ! Incompressible flow (non-dim fluid density)

            ! setting BC
            CALL self%set_bc()

            !CALL self%printf()
        END SUBROUTINE construct


        SUBROUTINE set_bc(self)
            CLASS(field_type), INTENT(INOUT) :: self
            ASSOCIATE(p => self%p, u => self%u, v => self%v, &
                    & nx => self%grid%nx, ny => self%grid%ny)
                ! BC on pressure using ghost nodes (zero normal gradient) 
                    p(0,:)    = p(2,:)    ! left boundary ghost nodes
                    p(nx+1,:) = p(nx-1,:) ! right boundary ghost nodes
                    p(:,0)    = p(:,2)    ! bottom boundary ghost nodes
                    p(:,ny+1) = p(:,ny-1) ! top boundary ghost nodes

                ! Ghost node values for u and v
                    u(0,:)  = -u(1,:)     ! left ghost nodes  -> u average on left boundary   = 0
                    u(nx,:) = -u(nx-1,:)  ! right ghost nodes -> u average on right boundary  = 0
                    v(:,0)  = -v(:,1)     ! bottom ghost nodes-> v average on bottom boundary = 0
                    v(:,ny) = -v(:,ny-1)  ! top ghost nodes   -> v average on top boundary    = 0

                
                ! No slip condition on u and v
                ! NOTE*: It is necessary to define top boundary (lid velocity) after the ghost node values
                ! because in pressure poisson equation, du/dx at top-left and top-right corner nodes shall be zero.
                ! If ghost value u(0,:)=-u(1,:) and u(nx,:) = -u(nx-1,:) are defined later, then du/dx at the top corner 
                ! nodes will be non-zero and the pressure distribution will be wrong, and so the velocity field.
                    u(:,1)  = 0.0         ! bottom boundary
                    u(:,ny) = 1.0         ! top boundary (lid velocity (non-dim)
                    v(1,:)  = 0.0         ! left boundary
                    v(nx,:) = 0.0         ! right boundary
            END ASSOCIATE
        END SUBROUTINE set_bc


        SUBROUTINE printf(self, format)
            ! Prints field variables value of a 2D grid in matrix form
            CLASS(field_type), INTENT(INOUT) :: self
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: format ! eg. format='F7.3'
            INTEGER :: ix_low, ix_up, iy_low, iy_up

            ASSOCIATE(u => self%u, v => self%v, p => self%p)
                ! u velocity
                ix_low = LBOUND(u,1)+1; ix_up  = UBOUND(u,1)-1;
                iy_low = LBOUND(u,2)+1; iy_up  = UBOUND(u,2)-1;
                PRINT*, 'field: u, shape: (dim1, dim2) = (',SHAPE(u(ix_low:ix_up,iy_low:iy_up)),')'
                IF(PRESENT(format)) THEN
                    CALL print_data(data=u(ix_low:ix_up,iy_low:iy_up), format=format, dim_along_row = 1)
                ELSE
                    CALL print_data(data=u(ix_low:ix_up,iy_low:iy_up), dim_along_row = 1)
                END IF

                ! v velocity
                ix_low = LBOUND(v,1)+1; ix_up  = UBOUND(v,1)-1;
                iy_low = LBOUND(v,2)+1; iy_up  = UBOUND(v,2)-1;
                PRINT*, 'field: v, shape: (dim1, dim2) = (',SHAPE(v(ix_low:ix_up,iy_low:iy_up)),')'
                IF(PRESENT(format)) THEN
                    CALL print_data(data=v(ix_low:ix_up,iy_low:iy_up), format=format, dim_along_row = 1)
                ELSE
                    CALL print_data(data=v(ix_low:ix_up,iy_low:iy_up), dim_along_row = 1)
                END IF

                ! p pressure
                ix_low = LBOUND(p,1)+1; ix_up  = UBOUND(p,1)-1;
                iy_low = LBOUND(p,2)+1; iy_up  = UBOUND(p,2)-1;
                PRINT*, 'field: p, shape: (dim1, dim2) = (',SHAPE(p(ix_low:ix_up,iy_low:iy_up)),')'
                IF(PRESENT(format)) THEN
                    CALL print_data(data=p(ix_low:ix_up,iy_low:iy_up), format=format, dim_along_row = 1)
                ELSE
                    CALL print_data(data=p(ix_low:ix_up,iy_low:iy_up), dim_along_row = 1)
                END IF
            END ASSOCIATE
        END SUBROUTINE printf


        ! Procedures of module mod_field
        SUBROUTINE copy_field(left, right)
            TYPE(field_type), INTENT(OUT), POINTER  :: left
            TYPE(field_type), INTENT(IN),  POINTER  :: right
    
            IF(ASSOCIATED(left)) DEALLOCATE(left) ! Necessary to avoid memory leak
            ALLOCATE(left) ! Allocating afresh
    
            ALLOCATE( left%u(LBOUND(right%u,1):UBOUND(right%u,1), LBOUND(right%u,2):UBOUND(right%u,2)) )
            ALLOCATE( left%v(LBOUND(right%v,1):UBOUND(right%v,1), LBOUND(right%v,2):UBOUND(right%v,2)) )
            ALLOCATE( left%p(LBOUND(right%p,1):UBOUND(right%p,1), LBOUND(right%p,2):UBOUND(right%p,2)) )
            left%u   = right%u
            left%v   = right%v
            left%p   = right%p
            left%rho = right%rho
            left%grid => right%grid ! Associating to the same grid memory
        END SUBROUTINE copy_field


        SUBROUTINE print_rms_diff_fields(field1,field2,format)
            TYPE(field_type), INTENT(OUT), POINTER  :: field1
            TYPE(field_type), INTENT(IN),  POINTER  :: field2
            CHARACTER(LEN=*), INTENT(IN) :: format ! eg. format='F7.3'
            INTEGER :: ix_low, ix_up, iy_low, iy_up
            REAL(KIND=rkind) :: u_rms , v_rms, p_rms

            ix_low = LBOUND(field1%u,1)+1; ix_up  = UBOUND(field1%u,1)-1;
            iy_low = LBOUND(field1%u,2)+1; iy_up  = UBOUND(field1%u,2)-1;
            u_rms = SQRT( SUM( ( field1%u(ix_low:ix_up, iy_low:iy_up) &
                               &-field2%u(ix_low:ix_up, iy_low:iy_up) &
                               )**2                                    &
                             )/SIZE(field1%u(ix_low:ix_up, iy_low:iy_up)) )

            ix_low = LBOUND(field1%v,1)+1; ix_up  = UBOUND(field1%v,1)-1;
            iy_low = LBOUND(field1%v,2)+1; iy_up  = UBOUND(field1%v,2)-1;
            v_rms = SQRT( SUM( ( field1%v(ix_low:ix_up, iy_low:iy_up) &
                               &-field2%v(ix_low:ix_up, iy_low:iy_up) &
                               )**2                                    &
                             )/SIZE(field1%v(ix_low:ix_up, iy_low:iy_up)) )

            ix_low = LBOUND(field1%p,1)+1; ix_up  = UBOUND(field1%p,1)-1;
            iy_low = LBOUND(field1%p,2)+1; iy_up  = UBOUND(field1%p,2)-1;
            p_rms = SQRT( SUM( ( field1%p(ix_low:ix_up, iy_low:iy_up) &
                               &-field2%p(ix_low:ix_up, iy_low:iy_up) &
                               )**2                                    &
                             )/SIZE(field1%p(ix_low:ix_up, iy_low:iy_up)) )

            WRITE(*, FMT='(*('//format//' '//'))', ADVANCE='NO') u_rms, v_rms, p_rms
        END SUBROUTINE print_rms_diff_fields

END MODULE mod_field
