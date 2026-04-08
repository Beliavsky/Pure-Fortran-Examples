module price_stats_mod
implicit none

integer, parameter :: dp = kind(1.0d0)

contains

function count_fields(line) result(nfields)
! count comma-separated fields
character(len=*), intent(in) :: line
integer :: nfields
integer :: i

nfields = 1
do i = 1, len_trim(line)
   if (line(i:i) == ',') nfields = nfields + 1
end do
end function count_fields

subroutine split_csv_line(line, fields)
! split a csv line on commas
character(len=*), intent(in) :: line
character(len=*), intent(out) :: fields(:)
integer :: i, i0, k, n, lt

n = size(fields)
fields = ''
lt = len_trim(line)
i0 = 1
k = 1

do i = 1, lt
   if (line(i:i) == ',') then
      if (k <= n) fields(k) = adjustl(line(i0:i-1))
      k = k + 1
      i0 = i + 1
   end if
end do

if (k <= n) fields(k) = adjustl(line(i0:lt))
end subroutine split_csv_line

end module price_stats_mod

program price_return_stats
use price_stats_mod, only: dp, count_fields, split_csv_line
implicit none

character(len=256) :: filename
character(len=4096) :: line
character(len=64), allocatable :: names(:)
character(len=256), allocatable :: fields(:)
real(kind=dp), allocatable :: prices(:,:)
integer :: iu, ios, n_assets, nobs, i, j
call get_command_argument(1, filename)
if (len_trim(filename) == 0) filename = "spy_efa_eem_tlt_lqd.csv"

open(newunit=iu, file=trim(filename), status='old', action='read', iostat=ios)
if (ios /= 0) error stop 'cannot open input file'

read(iu, '(a)', iostat=ios) line
if (ios /= 0) error stop 'cannot read header line'

n_assets = count_fields(line) - 1
if (n_assets <= 0) error stop 'no asset columns found'

nobs = 0
do
   read(iu, '(a)', iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) > 0) nobs = nobs + 1
end do
close(iu)

if (nobs < 2) error stop 'need at least two price rows'

allocate(names(n_assets))
allocate(fields(n_assets + 1))
allocate(prices(nobs, n_assets))

open(newunit=iu, file=trim(filename), status='old', action='read', iostat=ios)
if (ios /= 0) error stop 'cannot reopen input file'

read(iu, '(a)', iostat=ios) line
if (ios /= 0) error stop 'cannot reread header line'

call split_csv_line(trim(line), fields)
names = fields(2:)

do i = 1, nobs
   read(iu, '(a)', iostat=ios) line
   if (ios /= 0) error stop 'unexpected end of file while reading prices'
   call split_csv_line(trim(line), fields)
   do j = 1, n_assets
      read(fields(j + 1), *, iostat=ios) prices(i, j)
      if (ios /= 0) then
         write(*,'(a,i0,a,i0)') 'error reading price at row ', i, ', column ', j
         error stop
      end if
   end do
end do
close(iu)
print*,prices(1, :)
print*,prices(nobs, :)

end program price_return_stats