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

function mean(x) result(mu)
! return the mean of a real vector
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: mu

mu = sum(x) / real(size(x), kind=dp)
end function mean

function sd(x) result(sig)
! return the sample standard deviation of a real vector
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: sig
real(kind=dp) :: mu

if (size(x) <= 1) then
   sig = 0.0_dp
else
   mu = mean(x)
   sig = sqrt(sum((x - mu)**2) / real(size(x) - 1, kind=dp))
end if
end function sd

end module price_stats_mod

program price_return_stats
use price_stats_mod, only: dp, count_fields, split_csv_line, mean, sd
implicit none

character(len=256) :: filename
character(len=4096) :: line
character(len=64), allocatable :: names(:)
character(len=256), allocatable :: fields(:)
real(kind=dp), allocatable :: prices(:,:), rets(:,:), mu(:), sig(:)
integer :: iu, ios, n_assets, nobs, i, j, name_width
character(len=100) :: fmt

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
allocate(rets(nobs - 1, n_assets))
allocate(mu(n_assets), sig(n_assets))

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

rets = prices(2:nobs, :) / prices(1:nobs - 1, :) - 1.0_dp

do j = 1, n_assets
   mu(j) = mean(rets(:, j))
   sig(j) = sd(rets(:, j))
end do

name_width = len('asset')
do j = 1, n_assets
   name_width = max(name_width, len_trim(names(j)))
end do

write(fmt, '("(a",i0,",2x,a14,2x,a14)")') name_width
write(*, fmt) 'asset', 'mean_return', 'std_return'

write(fmt, '("(a",i0,",2x,f14.8,2x,f14.8)")') name_width
do j = 1, n_assets
   write(*, fmt) trim(names(j)), mu(j), sig(j)
end do

end program price_return_stats
