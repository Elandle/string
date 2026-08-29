program main
    implicit none


    character(len=:), allocatable :: x


    x = "1234567"

    print *, slice(x, 3, 5)







    contains

        function f(a) result(b)
            character(*), intent(in) :: a
            character(len=:), allocatable :: b

            b = a
        endfunction f

        function slice(a, i, j) result(b)
            character(*), intent(in) :: a
            integer, intent(in) :: i, j
            character(len=:), allocatable :: b

            b = a(i:j)
        endfunction slice






endprogram main