program main
implicit none
character(len=5) :: input_string
character(len=max(1, len(input_string))) :: output_string

input_string = "abcde"
output_string = input_string

print *, len(output_string)
print *, trim(output_string)
end program main
