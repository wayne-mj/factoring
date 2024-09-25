program factoring
    implicit none
    integer :: n,i 
    integer :: ierr
    character(len=255) :: arg
    integer, allocatable :: factors(:)


    print *, "Factoring program"

    call get_command_argument(1, arg, STATUS=ierr)
    if (ierr .ne. 0) then
        print *, "USAGE: factoring <number>"
        stop
    else
        read(arg, '(I10)', iostat=ierr) n
        if (ierr .ne. 0) then
            print *, "USAGE: factoring <number>"
            stop
        end if

        if (n .eq. 0) then
            print *, "0 has no factors"
            stop
        end if

        factors = factored(n)
        print *, "Factors of ", n, " are: "
        do i = 1, size(factors)
            !print *, factors(i)
            write(*, '(I10, A)', advance='no') factors(i), " "
        end do   
        print *     
    end if

    contains
    ! Factoring function
    function factored(a) result(factors)
        integer, intent(in) :: a
        integer, allocatable :: factors(:)
        integer :: i, count, local
        logical :: neg

        ! I might be dealing with negative numbers
        neg = .false.
        local = a

        count = 0

        ! If the number is 0, return - why are we even here?
        if (local == 0) then
            return
        end if

        ! If the number is negative, make it positive
        ! and remember that it was negative
        ! Negative numbers have the same factors as positive numbers
        ! Negative numbers cause the Do loop to SEGFAULT
        if (local < 0) then
            neg = .true.
            local = -a
        end if

        if (local == 1) then                    ! -+
            count = 1                           !  |
            allocate(factors(count))            !  |
            factors(count) = 1                  !  |
            if (neg) then                       !   Do I really need to assign and use count here?
                factors(count) = -1             !  |
                return                          !  |
            end if                              !  |
        end if                                  ! -+

        do i = 1, local
            if (mod(local, i) == 0) then
                count = count + 1
                if (.not.allocated(factors)) then
                    allocate(factors(count))
                else
                    call resizearray(factors, count)
                end if
                if (neg) then
                    factors(count) = -i
                else
                    factors(count) = i
                end if
                ! print *, "This is :", i
            end if
        end do
    end function factored

    ! Resize the array to the new size
    subroutine resizearray(array, newsize)
        integer, allocatable :: array(:)
        integer, intent(in) :: newsize
        integer, allocatable :: temp(:)

        allocate(temp(newsize))
        if (allocated(array)) then
            temp(1:size(array)) = array
            deallocate(array)
        end if
        array = temp

        deallocate(temp)
    end subroutine resizearray

end program factoring