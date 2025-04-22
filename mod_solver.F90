
MODULE mod_solver
    USE mod_read_file
    USE mod_grid
    USE mod_field
    USE mod_parameters
    USE mod_utility
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : error_unit 
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: solver_incompressible
    

    TYPE :: solver_incompressible
        TYPE(input_data_type) :: input_data
        TYPE(grid_type),  POINTER, PUBLIC :: grid => NULL()
        TYPE(field_type), POINTER, PUBLIC :: field => NULL()
        CHARACTER(LEN=:), ALLOCATABLE :: grid_file
        REAL(KIND=rkind)  :: Re_ref                    ! Reference Reynolds number
        REAL(KIND=rkind)  :: dt                        ! Non-dim time-step
        REAL(KIND=rkind)  :: relax_u, relax_v, relax_p ! relaxation factors
        REAL(KIND=rkind)  :: tolerance                 ! Tolerance for convergence check
        INTEGER           :: niter                     ! Number of time iterations
        INTEGER           :: niter_gauss               ! Number of Gauss-Seidel iterations


        CONTAINS
            PROCEDURE, PASS(self), PUBLIC  :: read_input
            PROCEDURE, PASS(self), PUBLIC  :: construct
            PROCEDURE, PASS(self), PUBLIC  :: initialize_grid
            PROCEDURE, PASS(self), PUBLIC  :: initialize_field
            PROCEDURE, PASS(self), PUBLIC  :: solve
            PROCEDURE, PASS(self), PRIVATE :: comp_convective_terms_x
            PROCEDURE, PASS(self), PRIVATE :: comp_diffusion_terms_x
            PROCEDURE, PASS(self), PRIVATE :: comp_convective_terms_y
            PROCEDURE, PASS(self), PRIVATE :: comp_diffusion_terms_y
    END TYPE solver_incompressible


    CONTAINS
        ! Procedures of user defined type: solver_incompressible
        SUBROUTINE read_input(self, file)
            CLASS(solver_incompressible), INTENT(INOUT) :: self
            CHARACTER(LEN=*), INTENT(IN) :: file

            ! Reading input data and storing into input_data object
            CALL self%input_data%read_input(file)
        END SUBROUTINE read_input

        
        SUBROUTINE construct(self)
            CLASS(solver_incompressible), INTENT(INOUT) :: self
            
            IF(.NOT. ASSOCIATED(self%grid))  ALLOCATE(self%grid)
            IF(.NOT. ASSOCIATED(self%field)) ALLOCATE(self%field)

            ! Assigning variables from the input file now stored in input_data object
            CALL self%input_data%assign_variable('Grid information','grid_file',self%grid_file)
            CALL self%input_data%assign_variable('Flow parameters','Re_ref',self%Re_ref)
            CALL self%input_data%assign_variable('Computational parameters','dt',self%dt)
            CALL self%input_data%assign_variable('Computational parameters','niter',self%niter)
            CALL self%input_data%assign_variable('Computational parameters','niter_gauss',self%niter_gauss)
            CALL self%input_data%assign_variable('Computational parameters','tolerance',self%tolerance)
            CALL self%input_data%assign_variable('Computational parameters','relax_u',self%relax_u)
            CALL self%input_data%assign_variable('Computational parameters','relax_v',self%relax_v)
            CALL self%input_data%assign_variable('Computational parameters','relax_p',self%relax_p)

            PRINT*, 'Input data:'
            CALL self%input_data%print_input_data()
            PRINT*
        END SUBROUTINE construct


        SUBROUTINE initialize_grid(self)
            CLASS(solver_incompressible), INTENT(INOUT) :: self
            
            IF(self%grid_file == '') THEN
                WRITE(error_unit,'("Error: Grid file is not defined in the input file. Aborted.")')
                STOP
            END IF
            CALL self%grid%initialize(self%grid_file)
        END SUBROUTINE initialize_grid


        SUBROUTINE initialize_field(self)
            CLASS(solver_incompressible), INTENT(INOUT) :: self
            
            CALL self%field%construct(self%grid)
        END SUBROUTINE initialize_field


        SUBROUTINE solve(self)
            CLASS(solver_incompressible), INTENT(INOUT) :: self
            REAL(KIND=rkind), PARAMETER  :: mu_non_dim=1.0 ! Non dim coeff. of dynamic viscosity (=1.0) (assumed constant)
            TYPE(field_type), POINTER :: field_new => NULL()
            REAL(KIND=rkind), POINTER, DIMENSION(:,:) :: p_dash    => NULL()
            REAL(KIND=rkind), POINTER :: coeff(:,:,:) => NULL(), rhs(:,:) => NULL() ! used in Gauss-Seidel method 
            INTEGER :: iter = 0
            
            ALLOCATE(field_new)
            field_new = self%field 
			! Allocation of field_new is necessary since here automatic assignment is called.
			! Automatic assignment assumes the pointers are already allocated.
			! In automatic assignments, allocatable attributes are first allocated and then values are copied.
			! Pointer attributes are referenced/associated without allocating new memory. i.e. pointer attributes point to the same memory.
			! It is always recommended to make user defined copy subroutine. Anyway, here, automatic copying serves our purpose.

            ! Allocating such that ghost nodes are also included and the indexing is preserved
            ALLOCATE( p_dash(LBOUND(self%field%p,1):UBOUND(self%field%p,1), LBOUND(self%field%p,2):UBOUND(self%field%p,2)) )
            ALLOCATE( coeff( 1:5, LBOUND(self%field%p,1):UBOUND(self%field%p,1), LBOUND(self%field%p,2):UBOUND(self%field%p,2)), &
                          &   rhs(LBOUND(self%field%p,1):UBOUND(self%field%p,1), LBOUND(self%field%p,2):UBOUND(self%field%p,2)) )

            ASSOCIATE(nx => self%grid%nx, ny => self%grid%ny, &
                    & dx => self%grid%dx, dy => self%grid%dy, &
                    & u => self%field%u, v => self%field%v, p => self%field%p, &
                    & rho => self%field%rho, &
                    & u_new => field_new%u, v_new => field_new%v, p_new => field_new%p, &
                    & Re_ref => self%Re_ref, dt => self%dt, &
                    & relax_u => self%relax_u, relax_v => self%relax_v, relax_p => self%relax_p)

                ! For pressure poisson equation: coefficients of p_dash's (ghost nodes in coeff are not used, but boundary nodes are used)
                coeff(1,1:nx,1:ny) = -dt*( SPREAD(1/( dx(1:nx)*dx(0:nx-1) ),dim=2,ncopies=ny) &
                                        & +SPREAD(1/( dy(1:ny)*dy(0:ny-1) ),dim=1,ncopies=nx) )
                coeff(2,1:nx,1:ny) = SPREAD( dt*( 1/( dx(1:nx)*  ( dx(0:nx-1)+dx(1:nx) ) ) ), dim=2,ncopies=ny )
                coeff(3,1:nx,1:ny) = SPREAD( dt*( 1/( dx(0:nx-1)*( dx(0:nx-1)+dx(1:nx) ) ) ), dim=2,ncopies=ny )
                coeff(4,1:nx,1:ny) = SPREAD( dt*( 1/( dy(1:ny)*  ( dy(0:ny-1)+dy(1:ny) ) ) ), dim=1,ncopies=nx )
                coeff(5,1:nx,1:ny) = SPREAD( dt*( 1/( dy(0:ny-1)*( dy(0:ny-1)+dy(1:ny) ) ) ), dim=1,ncopies=nx )
                
                CALL self%field%printf(format='F13.8')
                PRINT*; CALL write_date_time(); PRINT*;  
                WRITE(*,'("iter poisson_rhs_before poisson_rhs_after u_rms_diff v_rms_diff p_rms_diff")')   

                ! Loop
                iter_loop: DO WHILE(iter<self%niter)
                    iter = iter+1
                    WRITE(*,'(I6," ")',ADVANCE='NO') iter
                
                    u_new(1:nx-1,2:ny-1) = u(1:nx-1,2:ny-1) & 
                                    & +dt*( self%comp_convective_terms_x() &
                                    &      +self%comp_diffusion_terms_x(mu_non_dim,Re_ref) ) &
                                    & -dt*( p(2:nx,2:ny-1)-p(1:nx-1,2:ny-1) )/SPREAD(dx(1:nx-1),dim=2,ncopies=ny-2) ! (rho*u)_new
                    
                    v_new(2:nx-1,1:ny-1) = v(2:nx-1,1:ny-1) & 
                                    & +dt*( self%comp_convective_terms_y() &
                                    &      +self%comp_diffusion_terms_y(mu_non_dim,Re_ref) ) &
                                    & -dt*( p(2:nx-1,2:ny)-p(2:nx-1,1:ny-1) )/SPREAD(dy(1:ny-1),dim=1,ncopies=nx-2) ! (rho*u)_new

                    CALL field_new%set_bc()

                    
                    ! Solving pressure poisson equation for p_dash (ghost nodes and boundary nodes in rhs are used)
                    rhs(1:nx,1:ny) = (u_new(1:nx,1:ny)-u_new(0:nx-1,1:ny))/SPREAD(dx(0:nx-1)+dx(1:nx),dim=2,ncopies=ny) &
                                    +(v_new(1:nx,1:ny)-v_new(1:nx,0:ny-1))/SPREAD(dy(0:ny-1)+dy(1:ny),dim=1,ncopies=nx)
                    WRITE(*, '(E15.7," ")', ADVANCE='NO') SUM(ABS(rhs(1:nx,1:ny)))/SIZE(rhs(1:nx,1:ny))
                    CALL solve_Gauss_Seidel(p_dash, coeff, rhs, self%tolerance, self%niter_gauss)

                    ! correcting the new field
                    u_new(1:nx-1,2:ny-1) = u_new(1:nx-1,2:ny-1) &
                                        & -relax_u*dt*( p_dash(2:nx,2:ny-1)-p_dash(1:nx-1,2:ny-1) ) &
                                        &  /SPREAD(dx(1:nx-1),dim=2,ncopies=ny-2)
                    v_new(2:nx-1,1:ny-1) = v_new(2:nx-1,1:ny-1) &
                                        & -relax_v*dt*( p_dash(2:nx-1,2:ny)-p_dash(2:nx-1,1:ny-1) ) &
                                        &  /SPREAD(dy(1:ny-1),dim=1,ncopies=nx-2)
                    p_new(1:nx,1:ny) = p(1:nx,1:ny) + relax_p*p_dash(1:nx,1:ny)

                    rhs(1:nx,1:ny) = (u_new(1:nx,1:ny)-u_new(0:nx-1,1:ny))/SPREAD(dx(0:nx-1)+dx(1:nx),dim=2,ncopies=ny) &
                                    +(v_new(1:nx,1:ny)-v_new(1:nx,0:ny-1))/SPREAD(dy(0:ny-1)+dy(1:ny),dim=1,ncopies=nx)
                    
                    WRITE(*, '(E15.7," ")', ADVANCE='NO') SUM(ABS(rhs(1:nx,1:ny)))/SIZE(rhs(1:nx,1:ny))

                    ! Applying BC and updating ghost nodes
                    CALL field_new%set_bc()

                    ! Calulating and printing the RMS diffence b/w field_new and field
                    CALL print_rms_diff_fields(self%field,field_new,format='E17.8')

                    ! Updating the field
                    self%field = field_new

                    PRINT* ! Advance to the new line
                END DO iter_loop

                PRINT*, 'Solution complete.'
                PRINT*; CALL write_date_time(); PRINT*;
                PRINT*,'New field'
                CALL field_new%printf(format='F13.8')
            END ASSOCIATE

            DEALLOCATE(field_new, p_dash, coeff, rhs)
        END SUBROUTINE solve


        FUNCTION comp_convective_terms_x(self) RESULT(conv_term)
            CLASS(solver_incompressible), INTENT(INOUT) :: self
            REAL(KIND=rkind), DIMENSION(1:self%grid%nx-1, 2:self%grid%ny-1) :: conv_term 
            REAL(KIND=rkind), DIMENSION(1:self%grid%nx-1, 1:self%grid%ny)   :: v_mean 
            INTEGER :: i, j

            ASSOCIATE(nx => self%grid%nx, ny => self%grid%ny, &
                    & dx => self%grid%dx, dy => self%grid%dy, &
                    & u => self%field%u, v => self%field%v, &
                    & rho => self%field%rho)

                conv_term = 0
                v_mean = 0.5*( ( v(1:nx-1,1:ny)  +v(2:nx,1:ny)   )*SPREAD(dy(0:ny-1),dim=1,ncopies=nx-1) &
                        &     +( v(1:nx-1,0:ny-1)+v(2:nx,0:ny-1) )*SPREAD(dy(1:ny),  dim=1,ncopies=nx-1) &
                        &    )/SPREAD(dy(0:ny-1)+dy(1:ny),dim=1,ncopies=nx-1)
                DO j=2,ny-1,1 ! can be parallelized
                    DO i=1,nx-1,1
                        conv_term(i,j) = conv_term(i,j) &
                                      & -rho*( ( ( u(i+1,j)*dx(i)+u(i,j)*dx(i+1) )/( dx(i)+dx(i+1) ) )**2 &
                                      &       -( ( u(i,j)*dx(i-1)+u(i-1,j)*dx(i) )/( dx(i-1)+dx(i) ) )**2 &
                                      &      )/dx(i)

                        conv_term(i,j) = conv_term(i,j) &
                                      & -rho*( (u(i,j+1)*v_mean(i,j+1)) - (u(i,j-1)*v_mean(i,j-1)) ) &
                                      & /( dy(j)+dy(j-1) )
                    END DO
                END DO
            END ASSOCIATE 
            END FUNCTION comp_convective_terms_x


            FUNCTION comp_diffusion_terms_x(self, mu_non_dim, Re_ref) RESULT(diff_term)
                CLASS(solver_incompressible), INTENT(INOUT) :: self
                REAL(KIND=rkind), INTENT(IN) :: mu_non_dim, Re_ref
                REAL(KIND=rkind), DIMENSION(1:self%grid%nx-1, 2:self%grid%ny-1) :: diff_term 
                REAL(KIND=rkind), DIMENSION(1:self%grid%nx-1)   :: dxa, dxb
                INTEGER :: i, j
    
                ASSOCIATE(nx => self%grid%nx, ny => self%grid%ny, &
                        & dx => self%grid%dx, dy => self%grid%dy, &
                        & u => self%field%u, v => self%field%v, &
                        & rho => self%field%rho)
    
                    diff_term = 0
                    dxa = 0.5*( dx(1:nx-1)+dx(2:nx) )
                    dxb = 0.5*( dx(1:nx-1)+dx(0:nx-2) )
                    DO j=2,ny-1,1 ! can be parallelized
                        DO i=1,nx-1,1
                            diff_term(i,j) = diff_term(i,j) &
                                          & +( ( u(i+1,j)*dxb(i) - u(i,j)*( dxa(i)+dxb(i) ) + u(i-1,j)*dxa(i) ) &
                                          &   /( dxa(i)*dxb(i)*( dxa(i)+dxb(i) )/2 ) )
    
                            diff_term(i,j) = diff_term(i,j) &
                                          & +( ( u(i,j+1)*dy(j-1) - u(i,j)*( dy(j-1)+dy(j) ) + u(i,j-1)*dy(j) ) &
                                          &   /( dy(j)*dy(j-1)*( dy(j)+dy(j-1) )/2 ) )
                        END DO
                    END DO
                    diff_term = diff_term*mu_non_dim/Re_ref
                END ASSOCIATE 
            END FUNCTION comp_diffusion_terms_x


            FUNCTION comp_convective_terms_y(self) RESULT(conv_term)
                CLASS(solver_incompressible), INTENT(INOUT) :: self
                REAL(KIND=rkind), DIMENSION(2:self%grid%nx-1, 1:self%grid%ny-1) :: conv_term 
                REAL(KIND=rkind), DIMENSION(1:self%grid%nx,   1:self%grid%ny-1) :: u_mean 
                INTEGER :: i, j

                ASSOCIATE(nx => self%grid%nx, ny => self%grid%ny, &
                        & dx => self%grid%dx, dy => self%grid%dy, &
                        & u => self%field%u, v => self%field%v, &
                        & rho => self%field%rho)

                    conv_term = 0
                    u_mean = 0.5*( ( u(0:nx-1,1:ny-1)+u(0:nx-1,2:ny) )*SPREAD(dx(1:nx),  dim=2,ncopies=ny-1) &
                            &     +( u(1:nx,  1:ny-1)+u(1:nx,  2:ny) )*SPREAD(dx(0:nx-1),dim=2,ncopies=ny-1) &
                            &    )/SPREAD(dx(0:nx-1)+dx(1:nx),dim=2,ncopies=ny-1)
                    DO j=1,ny-1,1 ! can be parallelized
                        DO i=2,nx-1,1
                            conv_term(i,j) = conv_term(i,j) &
                                            & -rho*( ( ( v(i,j+1)*dy(j)+v(i,j)*dy(j+1) )/( dy(j)+dy(j+1) ) )**2 &
                                            &       -( ( v(i,j)*dy(j-1)+v(i,j-1)*dy(j) )/( dy(j-1)+dy(j) ) )**2 &
                                            &      )/dy(j)

                            conv_term(i,j) = conv_term(i,j) &
                                            & -rho*( (v(i+1,j)*u_mean(i+1,j)) - (v(i-1,j)*u_mean(i-1,j)) ) &
                                            & /( dx(i)+dx(i-1) )
                        END DO
                    END DO
                END ASSOCIATE 
            END FUNCTION comp_convective_terms_y


            FUNCTION comp_diffusion_terms_y(self, mu_non_dim, Re_ref) RESULT(diff_term)
                CLASS(solver_incompressible), INTENT(INOUT) :: self
                REAL(KIND=rkind), INTENT(IN) :: mu_non_dim, Re_ref
                REAL(KIND=rkind), DIMENSION(2:self%grid%nx-1, 1:self%grid%ny-1) :: diff_term 
                REAL(KIND=rkind), DIMENSION(1:self%grid%ny-1)   :: dya, dyb
                INTEGER :: i, j
    
                ASSOCIATE(nx => self%grid%nx, ny => self%grid%ny, &
                        & dx => self%grid%dx, dy => self%grid%dy, &
                        & u => self%field%u, v => self%field%v, &
                        & rho => self%field%rho)
    
                    diff_term = 0
                    dya = 0.5*( dy(1:ny-1)+dy(2:ny) )
                    dyb = 0.5*( dy(1:ny-1)+dy(0:ny-2) )
                    DO j=1,ny-1,1 ! can be parallelized
                        DO i=2,nx-1,1
                            diff_term(i,j) = diff_term(i,j) &
                                          & +( ( v(i+1,j)*dx(i-1) - v(i,j)*( dx(i-1)+dx(i) ) + v(i-1,j)*dx(i) ) &
                                          &   /( dx(i)*dx(i-1)*( dx(i)+dx(i-1) )/2 ) )
    
                            diff_term(i,j) = diff_term(i,j) &
                                          & +( ( v(i,j+1)*dyb(j) - v(i,j)*( dya(j)+dyb(j) ) + v(i,j-1)*dya(j) ) &
                                          &   /( dya(j)*dyb(j)*( dya(j)+dyb(j) )/2 ) )
                        END DO
                    END DO
                    diff_term = diff_term*mu_non_dim/Re_ref
                END ASSOCIATE 
            END FUNCTION comp_diffusion_terms_y

        
        ! Procedures of module mod_solver
        SUBROUTINE solve_Gauss_Seidel(p_dash, coeff, rhs, tolerance, max_iter)
            ! coeff(1,:,:)*p_dash(i,j)   + coeff(2,:,:)*p_dash(i+1,j) + coeff(3,:,:)*p_dash(i-1,j) +
            ! coeff(4,:,:)*p_dash(i,j+1) + coeff(5,:,:)*p_dash(i,j-1) = rhs
            REAL(KIND=rkind), DIMENSION(:,:),   POINTER, INTENT(INOUT) :: p_dash
            REAL(KIND=rkind), DIMENSION(:,:,:), POINTER, INTENT(IN)    :: coeff
            REAL(KIND=rkind), DIMENSION(:,:),   POINTER, INTENT(IN)    :: rhs
            REAL(KIND=rkind), INTENT(IN) :: tolerance
            INTEGER,          INTENT(IN) :: max_iter
            REAL(KIND=rkind) :: prev_val, rms_diff
            INTEGER :: i, j ! node indices
            INTEGER :: iter, lx, ly, ux, uy ! lower and upper bound index values

            iter = 0;

            lx = LBOUND(p_dash,1); ly = LBOUND(p_dash,2);
            ux = UBOUND(p_dash,1); uy = UBOUND(p_dash,2);
            p_dash = 0
            rms_diff = 1.0; ! Dummy value

            !WRITE(*,"('Beginning iterations')")
            !CALL write_date_time()
            !WRITE(*,"('  iter  msd')")
            it_loop:DO WHILE(rms_diff>=tolerance)
                iter = iter+1
                rms_diff = 0.0

                do_j:DO j = ly+1, uy-1 ! Serial loop execution is required for Gauss-Seidel method
                    do_i:DO i = lx+1, ux-1
                        prev_val = p_dash(i,j)
                        p_dash(i,j) = ( rhs(i,j) &
                                    &  -( coeff(2,i,j)*p_dash(i+1,j) &
                                    &    +coeff(3,i,j)*p_dash(i-1,j) &
                                    &    +coeff(4,i,j)*p_dash(i,j+1) &
                                    &    +coeff(5,i,j)*p_dash(i,j-1) &
                                    &   ) )/coeff(1,i,j) 
                        rms_diff = rms_diff + (p_dash(i,j)-prev_val)**2 ! To check convergence
                    END DO do_i
                END DO do_j
                
                ! BC on p_dash using ghost nodes (zero normal gradient) 
                p_dash(lx,:) = p_dash(lx+2,:) ! left boundary ghost nodes
                p_dash(ux,:) = p_dash(ux-2,:) ! right boundary ghost nodes
                p_dash(:,ly) = p_dash(:,ly+2) ! bottom boundary ghost nodes
                p_dash(:,uy) = p_dash(:,uy-2) ! top boundary ghost nodes
                
                ! calculating rms_diff
                rms_diff = SQRT(rms_diff/((SIZE(p_dash,1)-2)*(SIZE(p_dash,2)-2)))
                !WRITE(*,"('  ',I5,' ',E10.3)") iter, rms_diff

                IF(max_iter>0 .AND. iter==max_iter) EXIT it_loop
            END DO it_loop
            !CALL write_date_time()

            !WRITE(*,"('Solution completed.')")
        END SUBROUTINE solve_Gauss_Seidel

END MODULE mod_solver
