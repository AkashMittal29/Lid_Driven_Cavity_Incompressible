
MODULE mod_utility
    USE mod_parameters
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : error_unit
    IMPLICIT NONE

    INTERFACE print_data
        MODULE PROCEDURE print_2d_data_rkind ! , procedure_2, procedure_3
    END INTERFACE print_data

    INTERFACE rms
        MODULE PROCEDURE rms_2d
    END INTERFACE rms

    CONTAINS
        SUBROUTINE print_2d_data_rkind(data, format, dim_along_row)
            REAL(KIND=rkind), DIMENSION(:,:), INTENT(IN) :: data
            CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: format
            INTEGER, INTENT(IN) :: dim_along_row ! Dimension number along row of 2d array (= 1 or 2)
            INTEGER :: i
            CHARACTER(LEN=100) :: fmt

            IF(PRESENT(format)) THEN
                fmt = "(*("//TRIM(ADJUSTL(format))//",' ',))"
            ELSE
                fmt =  "(*("//"F8.3"//",' ',))"
            END IF
            
            IF(dim_along_row==1) THEN
                DO i=1,SIZE(data,dim=2)
                    WRITE(*,FMT=TRIM(ADJUSTL(fmt))) data(:,i)
                END DO
            ELSE
                DO i=1,SIZE(data,dim=1)
                    WRITE(*,FMT=TRIM(ADJUSTL(fmt))) data(i,:)
                END DO
            END IF
        END SUBROUTINE print_2d_data_rkind


        FUNCTION rms_2d(data) RESULT(rms_val)
            REAL(KIND=rkind), DIMENSION(:,:), INTENT(IN) :: data
            REAL(KIND=rkind) :: rms_val
            rms_val = (SUM(data**2.0)/SIZE(data))**0.5
        END FUNCTION rms_2d


        SUBROUTINE write_date_time()
            INTEGER :: clock_value(8)
            CALL DATE_AND_TIME(values=clock_value)
            WRITE(*,"('Date (d-m-y):',I2.2,'-',I2.2,'-',I4)",ADVANCE='NO') &
                  clock_value(3),clock_value(2),clock_value(1)
            WRITE(*,"(' Time: ',I2.2,':',I2.2,':',I2.2,'.',I3.3)") &
                  clock_value(5),clock_value(6),clock_value(7),clock_value(8)
        END SUBROUTINE write_date_time


        SUBROUTINE write_time()
            INTEGER :: clock_value(8)
            CALL DATE_AND_TIME(values=clock_value)
            WRITE(*,"('Time: ',I2.2,':',I2.2,':',I2.2,'.',I3.3)") &
                  clock_value(5),clock_value(6),clock_value(7),clock_value(8)
        END SUBROUTINE write_time

END MODULE mod_utility