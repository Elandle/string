module strings
    use stduse
    implicit none
    private

    public :: string

    public :: operator(//)
    public :: operator(*)

    public :: chars

    public :: str_ltrim
    public :: str_rtrim
    public :: str_print
    public :: str_len
    public :: str_set
    public :: str_to_real
    public :: str_to_realsp
    public :: str_to_realdp
    public :: str_to_realep
    public :: str_index
    public :: str_scan
    !public :: str_len_trim
    public :: str_adjustl
    public :: str_adjustr

    public :: str_to_complexdp


    !> Type for holding a string: an array of characters.
    !!
    !! Functionally, this type only holds an allocatable character array.
    !! The point is to be able to more conveniently use procedures and
    !! defined types compared to ordinary character arrays (allocatable or not).
    type :: string
        sequence
        private
        character(len=:), allocatable :: str
    end type

    !> Default constructor for strings: generate a string from a basic intrinsic type input.
    !!
    !! Returns a string of any single input character(*), integer, real(sp), real(dp), real(ep), or type(string).
    interface string
        module procedure string_from_chars
        module procedure string_from_int
        module procedure string_from_realsp
        module procedure string_from_realdp
        module procedure string_from_realep
        module procedure string_from_string
    endinterface

    !> Default plain chars (character array) constructor.
    !!
    !! Returns a plain character array character(*) of any single input character(*), integer, real(sp), real(dp), real(ep), or type(string).
    interface chars
        module procedure chars_from_chars
        module procedure chars_from_int
        module procedure chars_from_realsp
        module procedure chars_from_realdp
        module procedure chars_from_realep
        module procedure chars_from_string
    endinterface

    !> String concatenation operator. 
    !!
    !! Concatenate a string with another string or intrinsic basic type, returning a string.
    interface operator(//)
        module procedure string_conc_string
        module procedure string_conc_chars
        module procedure chars_conc_string
        module procedure string_conc_int
        module procedure int_conc_string
        module procedure string_conc_realdp
    endinterface

    !> String integer multiplication operator.
    !!
    !! Duplicates a string an integer number of times, returning a single string.
    interface operator(*)
        module procedure string_times_int
        module procedure int_times_string
    endinterface

    !> String setter.
    !!
    !! Sets the value of a string from a string or other basic intrinsic type.
    interface str_set
        module procedure string_set_chars
        module procedure string_set_string
        module procedure string_set_int
        module procedure string_set_realsp
        module procedure string_set_realdp
        module procedure string_set_realep
    endinterface

    !> Trims a string on the left, returning a string with left-blanks removed.
    !!
    !! Removes the leading blanks of a string, returning a new string with those blanks removed.
    interface str_ltrim
        module procedure str_ltrim
    endinterface

    !> Trims a string on the right, returning a string with right-blanks removed.
    !!
    !! Removes the trailing blanks for a string, returning a new string with those blanks removed.
    interface str_rtrim
        module procedure str_rtrim
    endinterface

    interface str_print
        module procedure str_print
    endinterface

    interface str_to_real
        module procedure str_to_realsp_subr
        module procedure str_to_realdp_subr
        module procedure str_to_realep_subr
    endinterface

    interface str_to_realsp
        module procedure str_to_realsp
    endinterface str_to_realsp
    interface str_to_realdp
        module procedure str_to_realdp
    endinterface str_to_realdp
    interface str_to_realep
        module procedure str_to_realep
    endinterface str_to_realep

    interface str_index
        module procedure str_index_chars
        module procedure str_index_str
    endinterface str_index

    !interface str_len_trim
    !    module procedure str_len_trim
    !endinterface str_len_trim

    interface str_scan
        module procedure str_scan_chars
        module procedure str_scan_str
    endinterface str_scan


    contains

        ! --------------------------------------------------------------------------------
        ! chars interface functions 

        !> Converts a character array to a character array.
        !!
        !!@param[in] chrs1 Character array to convert into a character array.
        function chars_from_chars(chrs1) result(chrs)
            character(*), intent(in) :: chrs1
            character(len=:), allocatable :: chrs

            chrs = chrs1
        endfunction chars_from_chars

        function chars_from_string(str) result(chrs)
            type(string), intent(in) :: str
            character(len=:), allocatable :: chrs

            chrs = str_to_chars(str)
        endfunction chars_from_string

        function chars_from_int(i) result(chrs)
            integer, intent(in) :: i
            character(len=:), allocatable :: chrs

            ! buffer length 11 for int32
            !               20 for int64
            !               40 for int128
            ! Keeping it safe with length 40 to handle all cases.
            ! Maybe could be a compile-time parameter computed ahead of time.
            character(40) :: buffer
            write(buffer, "(i0)") i
            chrs = trim(buffer)
        endfunction chars_from_int

        function chars_from_realsp(r) result(chrs)
            real(sp), intent(in) :: r
            character(len=:), allocatable :: chrs

            ! I am not quite sure what good buffer lengths are for
            ! real numbers, so I am using lengths that I think are quite larger than necessary
            ! for each of the realsp, realdp, realep cases.
            character(32) :: buffer
            write(buffer, "(g0)") r
            chrs = trim(buffer)
        endfunction chars_from_realsp

        function chars_from_realdp(r) result(chrs)
            real(dp), intent(in) :: r
            character(len=:), allocatable :: chrs

            character(32) :: buffer
            write(buffer, "(g0)") r
            chrs = trim(buffer)
        endfunction chars_from_realdp

        function chars_from_realep(r) result(chrs)
            real(ep), intent(in) :: r
            character(len=:), allocatable :: chrs

            character(64) :: buffer
            write(buffer, "(g0)") r
            chrs = trim(buffer)
        endfunction chars_from_realep

        ! -------------------------------------------------------------------------------------------

        subroutine string_set_chars(str, chrs)
            type(string), intent(out) :: str
            character(*), intent(in)  :: chrs

            str%str = chars(chrs)
        endsubroutine string_set_chars

        subroutine string_set_string(str, str1)
            type(string), intent(out) :: str
            type(string), intent(in)  :: str1

            str%str = chars(str1)
        endsubroutine string_set_string

        subroutine string_set_int(str, i)
            type(string), intent(out) :: str
            integer     , intent(in)  :: i

            str%str = chars(i)
        endsubroutine string_set_int

        subroutine string_set_realsp(str, r)
            type(string), intent(out) :: str
            real(sp)    , intent(in)  :: r

            str%str = chars(r)
        endsubroutine string_set_realsp

        subroutine string_set_realdp(str, r)
            type(string), intent(out) :: str
            real(dp)    , intent(in)  :: r

            str%str = chars(r)
        endsubroutine string_set_realdp

        subroutine string_set_realep(str, r)
            type(string), intent(out) :: str
            real(ep)    , intent(in)  :: r

            str%str = chars(r)
        endsubroutine string_set_realep

        function str_to_chars(str) result(chrs)
            type(string), intent(in) :: str
            character(len=:), allocatable :: chrs

            chrs = str%str
        endfunction str_to_chars

        function str_to_realsp(str) result(r)
            type(string), intent(in) :: str
            real(sp) :: r

            call str_to_real(str, r)
        endfunction

        function str_to_realdp(str) result(r)
            type(string), intent(in) :: str
            real(dp) :: r

            call str_to_real(str, r)
        endfunction

        function str_to_realep(str) result(r)
            type(string), intent(in) :: str
            real(ep) :: r

            call str_to_real(str, r)
        endfunction

        subroutine str_to_realsp_subr(str, r)
            type(string), intent(in)  :: str
            real(sp)    , intent(out) :: r

            integer :: ios

            read(str%str, *, iostat=ios) r

            if (ios .ne. 0) then
                error stop "Error in str_to_realsp_subr, cannot convert the following string to real(sp):" // str_to_chars(str)
            endif
        endsubroutine str_to_realsp_subr

        subroutine str_to_realdp_subr(str, r)
            type(string), intent(in)  :: str
            real(dp)    , intent(out) :: r

            integer :: ios

            read(str%str, *, iostat=ios) r

            if (ios .ne. 0) then
                error stop "Error in str_to_realdp_subr, cannot convert the following string to real(dp):" // str_to_chars(str)
            endif
        endsubroutine str_to_realdp_subr

        subroutine str_to_realep_subr(str, r)
            type(string), intent(in)  :: str
            real(ep)    , intent(out) :: r

            integer :: ios

            read(str%str, *, iostat=ios) r

            if (ios .ne. 0) then
                error stop "Error in str_to_realep_subr, cannot convert the following string to real(ep):" // str_to_chars(str)
            endif
        endsubroutine str_to_realep_subr



        subroutine str_allocate(str, length)
            type(string), intent(inout) :: str
            integer     , intent(in)    :: length

            call str_deallocate(str)
            allocate(character(len=length) :: str%str)
        endsubroutine str_allocate

        subroutine str_deallocate(str)
            type(string), intent(inout) :: str

            if (allocated(str%str)) then
                deallocate(str%str)
            endif
        endsubroutine str_deallocate

        function str_blank(length) result(str)
            integer, intent(in) :: length
            type(string) :: str

            call str_allocate(str, length)
        endfunction str_blank

        subroutine str_print(str, iounit)
            type(string), intent(in) :: str
            integer     , intent(in) :: iounit

            write(iounit, "(a)") str%str
        endsubroutine str_print


        ! --------------------------------------------------------------------------------------
        ! Functions for string interface.
        ! "Default constructors".
        function string_from_chars(chrs) result(str)
            character(*), intent(in) :: chrs
            type(string) :: str

            call str_set(str, chrs)
        endfunction

        function string_from_int(i) result(str)
            integer, intent(in) :: i
            type(string) :: str

            call str_set(str, i)
        endfunction string_from_int

        function string_from_realsp(r) result(str)
            real(sp), intent(in) :: r
            type(string) :: str

            call str_set(str, r)
        endfunction string_from_realsp

        function string_from_realdp(r) result(str)
            real(dp), intent(in) :: r
            type(string) :: str

            call str_set(str, r)
        endfunction string_from_realdp

        function string_from_realep(r) result(str)
            real(ep), intent(in) :: r
            type(string) :: str

            call str_set(str, r)
        endfunction string_from_realep

        function string_from_string(str1) result(str)
            type(string), intent(in) :: str1
            type(string) :: str

            str = str1
        endfunction string_from_string

        ! ---------------------------------------------------------------------------------------------------
        ! Functions for // operator interface.
        ! string concatenation.

        function string_conc_string(str1, str2) result(str)
            type(string), intent(in) :: str1
            type(string), intent(in) :: str2
            type(string) :: str

            str%str = chars(str1) // chars(str2)
        endfunction string_conc_string

        function string_conc_chars(str1, chrs1) result(str)
            type(string), intent(in) :: str1
            character(*), intent(in) :: chrs1
            type(string) :: str

            str%str = chars(str1) // chrs1
        endfunction string_conc_chars

        function chars_conc_string(chrs1, str1) result(str)
            character(*), intent(in) :: chrs1
            type(string), intent(in) :: str1
            type(string) :: str

            str%str = chrs1 // chars(str1)
        endfunction chars_conc_string

        function string_conc_int(str1, int1) result(str)
            type(string), intent(in) :: str1
            integer     , intent(in) :: int1
            type(string) :: str

            str = str1 // string(int1)
        endfunction string_conc_int

        function int_conc_string(int1, str1) result(str)
            integer     , intent(in) :: int1
            type(string), intent(in) :: str1
            type(string) :: str

            str = string(int1) // str1
        endfunction int_conc_string

        function string_conc_realdp(str1, r1) result(str)
            type(string), intent(in) :: str1
            real(dp)    , intent(in) :: r1
            type(string) :: str
            
            ! does not look that nice.
            ! think about a better looking solution.
            str = str1 // chars(r1)
        endfunction string_conc_realdp



        ! Behaves just like the usual character array slicing, but intended
        ! to be used in cases you can't using slicing (eg, a character
        ! array result of a function).
        function chars_slice(chrs, i, j) result(chrs1)
            character(*),           intent(in) :: chrs
            integer     , optional, intent(in) :: i
            integer     , optional, intent(in) :: j
            integer                       :: ii, jj
            character(len=:), allocatable :: chrs1

            if (present(i)) then
                ii = i
            else
                ii = 1
            endif

            if (present(j)) then
                jj = j
            else
                jj = len(chrs)
            endif

            chrs1 = chrs(ii:jj)
        endfunction chars_slice

        function str_slice(str, i, j) result(str1)
            type(string),           intent(in) :: str
            integer     , optional, intent(in) :: i
            integer     , optional, intent(in) :: j
            type(string) :: str1

            call str_set(str1, chars_slice(chars(str), i, j))
        endfunction str_slice


        ! ----------------------------------------------------------------------------------------------------
        ! Functions for str_ltrim and str_rtrim and str_lrtrim interfaces.
        ! left           "  abc " --> "abc " ,
        ! right          "  abc " --> "  abc",
        ! and left-right "  abc " --> "abc"    string trimming.

        function str_ltrim(str1) result(str)
            type(string), intent(in) :: str1
            type(string) :: str

            integer :: indx

            ! Fortran's trim intrinsic behaves like rtrim:
            ! it removes trailing blanks. So I have to craft up
            ! an ltrim: remove leading blanks.

            ! indx = first non blank (" ") character index in str1.
            ! 0 means no non-blank characters (str1 is all blanks).
            indx = verify(str1%str, " ")

            if (indx .eq. 0) then
                call str_set(str, "")
            else
                call str_set(str, str_slice(str1, indx))
                ! call str_set(str, chars(str1)(indx:))
            endif
        endfunction str_ltrim

        function str_rtrim(str1) result(str)
            type(string), intent(in) :: str1
            type(string) :: str

            call str_set(str, trim(chars(str1)))
        endfunction str_rtrim

        function str_lrtrim(str1) result(str)
            type(string), intent(in) :: str1
            type(string) :: str

            call str_set(str, adjustl(chars(str1)))
        endfunction str_lrtrim

        ! -----------------------------------------------------------------------
        ! Mimics the adjustl intrinsic but for strings.
        ! Shifts leading blanks to the right.
        ! For example:
        !
        ! "  abc " --> "abc   "
        function str_adjustl(str1) result(str)
            type(string), intent(in) :: str1
            type(string) :: str

            call str_set(str, adjustl(chars(str1)))
        endfunction str_adjustl

        ! Mimics the adjustr intrinsic but for strings.
        ! Shifts trailing blanks to the left.
        ! For example:
        !
        ! "  abc " --> "   abc"
        function str_adjustr(str1) result(str)
            type(string), intent(in) :: str1
            type(string) :: str

            call str_set(str, adjustr(chars(str1)))
        endfunction str_adjustr

        ! Returns the length (number of characters) of a string.
        ! For example:
        !
        !       "a bc " --> 5
        function str_len(str) result(length)
            type(string), intent(in) :: str
            integer :: length

            length = len(chars(str))
        endfunction str_len


        ! ----------------------------------------------------------------------
        ! Functions for * operator.
        ! Returns a string repeated a specified number of times (by *).
        ! For example:
        !
        !       3 * "abc" = "abcabcabc"
        !       "abc" * 2 = "abcabc"
        !
        ! (both sides of multiplication work).

        function string_times_int(str1, int1) result(str)
            type(string), intent(in) :: str1
            integer     , intent(in) :: int1
            type(string) :: str
            integer      :: i

            call str_set(str, repeat(chars(str1), int1))
        endfunction string_times_int

        function int_times_string(int1, str1) result(str)
            integer     , intent(in) :: int1
            type(string), intent(in) :: str1
            type(string) :: str

            str = str1 * int1
        endfunction int_times_string

        ! ----------------------------------------------------------------------

        subroutine str_split_str(str, set, pos, back)
            type(string), intent(in)              :: str
            type(string), intent(in)              :: set
            integer     , intent(inout)           :: pos
            logical     , intent(in)   , optional :: back
            logical :: bback

            if (present(back)) then
                bback = back
            else
                bback = .false.
            endif

            if (bback) then
                if (.not. (1 .le. pos .and. pos .le. str_len(str)+1)) then
                    error stop
                endif
            else
                if (.not. (0 .le. pos .and. pos .le. str_len(str))) then
                    error stop
                endif
            endif



        endsubroutine str_split_str

        subroutine str_split_chars(str, set, pos, back)
            type(string), intent(in)              :: str
            character(*), intent(in)              :: set
            integer     , intent(inout)           :: pos
            logical     , intent(in)   , optional :: back
        endsubroutine str_split_chars


        function str_index_str(str, substr, back) result(indx)
            type(string), intent(in)           :: str
            type(string), intent(in)           :: substr
            logical     , intent(in), optional :: back
            integer :: indx

            indx = str_index(str, chars(substr), back)
        endfunction str_index_str

        function str_index_chars(str, substr, back) result(indx)
            type(string), intent(in)           :: str
            character(*), intent(in)           :: substr
            logical     , intent(in), optional :: back
            integer :: indx

            indx = index(chars(str), substr, back)
        endfunction str_index_chars


        function str_scan_str(str, set, back) result(pos)
            type(string), intent(in)           :: str
            type(string), intent(in)           :: set
            logical     , intent(in), optional :: back
            integer :: pos

            pos = str_scan(str, chars(set), back)
        endfunction str_scan_str

        function str_scan_chars(str, set, back) result(pos)
            type(string), intent(in)           :: str
            character(*), intent(in)           :: set
            logical     , intent(in), optional :: back
            integer :: pos

            pos = scan(chars(str), set, back)
        endfunction str_scan_chars

endmodule strings