program main
    use stduse
    use strings
    implicit none

    type(string) :: str
    complex(dp) :: z
    
    str = string("abcd")
    call str_print(str, stdout)
    call str_print(string(str_scan(str, "b")), stdout)

    str = string("1.2 - 3i")

    z = str_to_complexdp(str)
    print *, z%re
    print *, z%im

end program main