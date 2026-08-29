module stduse
    use, intrinsic :: iso_fortran_env, only: real32, real64, real128, input_unit, output_unit, error_unit
    implicit none
    public

    integer, parameter :: sp = real32
    integer, parameter :: dp = real64
    integer, parameter :: ep = real128

    integer, parameter :: stdin  = input_unit
    integer, parameter :: stdout = output_unit
    integer, parameter :: stderr = error_unit
end module stduse