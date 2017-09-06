
module escdff_error

    use, intrinsic :: iso_c_binding
    use escdff_common

    implicit none

    private

    public :: &
&       escdff_error_add, &
&       escdff_error_fetchall, &
&       escdff_error_flush, &
&       escdff_error_free, &
&       escdff_error_get_last, &
&       escdff_error_len, &
&       escdff_error_string

    type, public :: escdff_error_t
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type escdff_error_t

    interface

        ! Interface: error/add
        function escdf_error_add(error_id, filename, line, routine) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_error_add
            integer(kind=c_int), value :: error_id
            character(kind=c_char) :: filename(*)
            integer(kind=c_int), value :: line
            character(kind=c_char) :: routine(*)
        end function escdf_error_add

        ! Interface: error/fetchall
        function escdf_error_fetchall() &
        &           bind(c)
            import
            type(c_ptr) :: escdf_error_fetchall
        end function escdf_error_fetchall

        subroutine escdff_error_flush() &
        &           bind(c, name='escdf_error_flush')
        end subroutine escdff_error_flush

        subroutine escdff_error_free() &
        &           bind(c, name='escdf_error_free')
        end subroutine escdff_error_free

        ! Interface: error/get_last
        function escdf_error_get_last(routine) &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_error_get_last
            character(kind=c_char) :: routine(*)
        end function escdf_error_get_last

        ! Interface: error/len
        function escdf_error_len() &
        &           bind(c)
            import
            integer(kind=c_int) :: escdf_error_len
        end function escdf_error_len

        ! Interface: error/string
        function escdf_error_string(error_id) &
        &           bind(c)
            import
            type(c_ptr) :: escdf_error_string
            integer(kind=c_int), value :: error_id
        end function escdf_error_string

    end interface

contains

    ! API: error/add
    integer function escdff_error_add(error_id, filename, line, routine) result(ret)

        use escdff_common

        implicit none

        integer, intent(in) :: error_id
        character(len=*), intent(in) :: filename
        integer, intent(in) :: line
        character(len=*), intent(in) :: routine

        character(kind=c_char) :: c_filename(len_trim(filename)+1)
        character(kind=c_char) :: c_routine(len_trim(routine)+1)

        c_filename = f_to_c_string(trim(filename))
        c_routine = f_to_c_string(trim(routine))
        ret = escdf_error_add(error_id, c_filename, line, c_routine)

    end function escdff_error_add

    ! API: error/fetchall
    character(len=ESCDF_STRLEN_ERROR) function escdff_error_fetchall() result(ret)

        use escdff_common

        implicit none

        type(c_ptr) :: c_ret

        c_ret = escdf_error_fetchall()
        call c_to_f_string_ptr(c_ret, ret)

    end function escdff_error_fetchall

    ! API: error/get_last
    integer function escdff_error_get_last(routine) result(ret)

        use escdff_common

        implicit none

        character(len=*), intent(in) :: routine

        character(kind=c_char) :: c_routine(len_trim(routine)+1)

        c_routine = f_to_c_string(trim(routine))
        ret = escdf_error_get_last(c_routine)

    end function escdff_error_get_last

    ! API: error/len
    integer function escdff_error_len() result(ret)

        implicit none

        ret = escdf_error_len()

    end function escdff_error_len

    ! API: error/string
    character(len=ESCDF_STRLEN_ERROR) function escdff_error_string(error_id) result(ret)

        use escdff_common

        implicit none

        integer, intent(in) :: error_id

        type(c_ptr) :: c_ret

        c_ret = escdf_error_string(error_id)
        call c_to_f_string_ptr(c_ret, ret)

    end function escdff_error_string

end module escdff_error
