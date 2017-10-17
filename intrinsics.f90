module intrinsics_module

	interface Populate
		module procedure rand_int
		module procedure rand_int_vec
	end interface

	interface APrint
		module procedure print_matrix
		module procedure print_real_matrix
		module procedure print_vec
		module procedure print_real_vec	
	end interface
	
	contains
		
		subroutine rand_int(a)
			integer,dimension(:,:),allocatable,intent(inout) :: a
			real :: u
			integer :: n,m
			n = size(a,1)
			do i = 1,n
				do j=1,n
					call RANDOM_NUMBER(u)
					a(i,j) = FLOOR((100)*u)+1
				enddo
			enddo
		end subroutine rand_int

		subroutine rand_int_vec(veca)
			integer,dimension(:),allocatable,intent(inout) :: veca
 			real :: u
			integer :: n
			n = size(veca)
			do i = 1,n
				call RANDOM_NUMBER(u)
				veca(i) = FLOOR((100)*u)+1
			enddo
		end subroutine rand_int_vec

		subroutine print_matrix(a)
			integer,dimension(:,:),allocatable,intent(in) :: a
 			integer :: length
			length = size(a,1)
			do i=1,length
				print *,(a(i,j), j=1,length)
			enddo
			print *,""
		end subroutine print_matrix

		subroutine print_real_matrix(r_a)
			real,dimension(:,:),allocatable,intent(in) :: r_a
 			integer :: length
			length = size(r_a,1)
			do i=1,length
				print *,(r_a(i,j), j=1,length)
			enddo
			print *,""
		end subroutine print_real_matrix
		
		subroutine print_real_vec(r_veca)
			real,dimension(:),allocatable,intent(in) :: r_veca
 			integer :: length
			length = size(r_veca)
			do i=1,length
				print *,r_veca(i)
			enddo
			print *,""	
		end subroutine print_real_vec

		subroutine print_vec(veca)
			integer,dimension(:),allocatable,intent(in) :: veca
 			integer :: length
			length = size(veca)
			do i=1,length
				print *,veca(i)
			enddo
			print *,""
		end subroutine print_vec

		function length(veca)
			integer,dimension(:),allocatable,intent(in) :: veca
			real :: length
			length = dot_product(veca,veca)
			length  = sqrt(length)
		end function length

		function length_pow(veca)
			integer,dimension(:),allocatable,intent(in) :: veca
			real :: length_pow
			length_pow = sum(veca**2)
			length_pow  = sqrt(length_pow)
		end function length_pow
		
		function length_with_size(veca)
			integer,dimension(:),allocatable,intent(in) :: veca
			real :: length_with_size
			length_with_size  = size(veca)
		end function length_with_size

		function matr_mul(a,b)
			integer,dimension(:,:),allocatable,intent(in) :: a,b
			integer,dimension(:,:),allocatable :: matr_mul
			allocate(matr_mul(n,n))
			matr_mul = matmul(a,b)
		end function matr_mul

		function matr_mul_dot(a,b)
			integer,dimension(:,:),allocatable,intent(in) :: a,b
			integer,dimension(:,:),allocatable :: matr_mul_dot
			n  = size (a,1)
			allocate(matr_mul_dot(n,n))
			do i=1,n
				do j=1,n
					matr_mul_dot(i,j) = dot_product(a(i,:),b(:,j))
				enddo
			enddo
		end function matr_mul_dot

		function matr_sum_std(a,b)
			integer,dimension(:,:),allocatable,intent(in) :: a,b
			integer,dimension(:,:),allocatable :: matr_sum_std
			n  = size (a,1)
			allocate(matr_sum_std(n,n))
			matr_sum_std = a+b
		end function matr_sum_std

		function matr_prod_std(a,b)
			integer,dimension(:,:),allocatable,intent(in) :: a,b
			integer,dimension(:,:),allocatable :: matr_prod_std
			n  = size (a,1)
			allocate(matr_prod_std(n,n))
			matr_prod_std = a*b
		end function matr_prod_std

		function matr_div_std(a,b)
			integer,dimension(:,:),allocatable,intent(in) :: a,b
			real,dimension(:,:),allocatable :: matr_div_std
			n  = size (a,1)
			allocate(matr_div_std(n,n))
			matr_div_std = a/b
		end function matr_div_std




end module intrinsics_module

program intrinsics 
	use intrinsics_module
	integer :: lengths(9) = (/2,4,10,100,200,400,800,1000,2000/)
	integer :: vecLengths(9)=(/20000,40000,80000,100000,1000000,5000000,10000000,50000000,100000000/)
	integer,dimension(:),allocatable :: veca,vecb
	integer,dimension(:,:),allocatable :: a,b,mres
	
	do l=1,9
		allocate (veca(vecLengths(l)))
		call Populate(veca)
		call cpu_time(startTime)
 		res= length(veca)
		call cpu_time(endTime)
		print '("time for vector(",i9,") length is ",f9.4," seconds ")',vecLengths(l),endTime-startTime
		deallocate(veca)
	enddo

	do l=1,9
		allocate (veca(vecLengths(l)))
		call Populate(veca)
		call cpu_time(startTime)
 		res= length_pow(veca)
		call cpu_time(endTime)
		print '("time for vector(",i9,") length using ** is ",f9.4," seconds ")',vecLengths(l),endTime-startTime
		deallocate(veca)
	enddo

	do l=1,9
		allocate (veca(vecLengths(l)))
		call Populate(veca)
		call cpu_time(startTime)
 		res= length_with_size(veca)
		call cpu_time(endTime)
		print '("time for vector(",i9,") length using size() is ",f9.4," seconds ")',vecLengths(l),endTime-startTime
		deallocate(veca)
	enddo

	do l=1,9
		allocate (a(lengths(l),lengths(l)),b(lengths(l),lengths(l)),mres(lengths(l),lengths(l)))
		call Populate(a)
		call Populate(b)
		call cpu_time(startTime)
 		mres = matr_mul_dot(a,b)
		call cpu_time(endTime)
		print '("time for matrix(",i4,"x",i4,") multiplication using dot_product is ",f9.4," seconds ")',lengths(l),lengths(l),endTime-startTime
		deallocate(a,b,mres)
	enddo


	do l=1,9
		allocate (a(lengths(l),lengths(l)),b(lengths(l),lengths(l)),mres(lengths(l),lengths(l)))
		call Populate(a)
		call Populate(b)
		call cpu_time(startTime)
 		mres = matr_mul(a,b)
		call cpu_time(endTime)
		print '("time for matrix(",i4,"x",i4,") multiplication using matmul() is ",f9.4," seconds ")',lengths(l),lengths(l),endTime-startTime
		deallocate(a,b,mres)
	enddo


	do l=1,9
		allocate (a(lengths(l),lengths(l)),b(lengths(l),lengths(l)),mres(lengths(l),lengths(l)))
		call Populate(a)
		call Populate(b)
		call cpu_time(startTime)
 		mres = matr_sum_std(a,b)
		call cpu_time(endTime)
		print '("time for matrix(",i4,"x",i4,") sum using + is ",f9.4," seconds ")',lengths(l),lengths(l),endTime-startTime
		deallocate(a,b,mres)
	enddo

	do l=1,9
		allocate (a(lengths(l),lengths(l)),b(lengths(l),lengths(l)),mres(lengths(l),lengths(l)))
		call Populate(a)
		call Populate(b)
		call cpu_time(startTime)
 		mres = matr_prod_std(a,b)
		call cpu_time(endTime)
		print '("time for matrix(",i4,"x",i4,") product using * is ",f9.4," seconds ")',lengths(l),lengths(l),endTime-startTime
		deallocate(a,b,mres)
	enddo

	do l=1,9
		allocate (a(lengths(l),lengths(l)),b(lengths(l),lengths(l)),mres(lengths(l),lengths(l)))
		call Populate(a)
		call Populate(b)
		call cpu_time(startTime)
 		mres = matr_div_std(a,b)
		call cpu_time(endTime)
		print '("time for matrix(",i4,"x",i4,") dividing using / is ",f9.4," seconds ")',lengths(l),lengths(l),endTime-startTime
		deallocate(a,b,mres)
	enddo

		
end program intrinsics
