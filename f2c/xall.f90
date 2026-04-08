program main
implicit none
integer :: i
integer :: ivec(2) = [10, 20], jvec(3) = [20, 30, 40]
print*,[(all(jvec/=ivec(i)),i=1,size(ivec))]
print*,[(any(jvec/=ivec(i)),i=1,size(ivec))]
end program main
