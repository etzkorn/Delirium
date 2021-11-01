! This file contains two functions

! funcpaMultivSplines (function): computes the log likelihood for a vector of
! parameters b given the data stored in the module "comonmultiv"

! distanceJ_splines (subroutine):


!======================== funcpaMultivSplines ====================
    double precision function funcpaMultivSplines(b,np,id,thi,jd,thj,k0)

    use taillesmultiv
    use comonmultiv
    use residusMmultiv

    IMPLICIT NONE

! Variable Definitions
    integer,intent(in)::id,jd,np
        ! id, jd = indexes at which to approximate first, second derivatives
        ! np = total number parameters, items in vectors b, bh
    double precision,dimension(np),intent(in)::b
        ! b = parameter vector
    double precision,dimension(4),intent(in)::k0
        ! k0 = smoothing parameters
    double precision,intent(in)::thi,thj
        ! thi, thj = small differences for approximation of first, second derivatives
    integer::n,i,j,k,l,vj,ig,choix
    integer,dimension(ngmax)::cpt!,cptmeta
        ! cpt, cptmax = number of recurrent events for each person
    double precision::pe1,pe2,pe3,pe4,inv,inv2,som1,som2,som3,som4,&
    res,vet,vet2,vet3,vet4,h1,pi,frail,frail2,weight,weight2,ss
        ! vet = temporary value for relative hazard for an individual
        ! h1 = distance between knot points
        ! frail = used in GH quadrature for integration
        ! weight = used in GH quadrature for integration
        ! ss =  temporary value to hold integral for each individual
    double precision,dimension(-2:(nzloco+2)):: the1
    double precision,dimension(-2:(nzloco+2)):: the2
    !double precision,dimension(-2:(nzmeta+2)):: the3
    double precision,dimension(-2:(nzdc+2)):: the4
        ! the1, the2, the3 , the4 = spline coefficients for hazards (r1, t1, r2, t2)
    double precision,dimension(np)::bh
        ! bh = local copy of parameter vector
    double precision,dimension(ngmax)::res2,res1dc,res2dc&
    ,res3dc,integrale1,integrale2,integrale3,res1dc2,res2dc2&
    ,res3dc2!,res2meta
      ! Hold person-specific values of hazard, cumulative hazard, integrals, etc.
    double precision,dimension(ndatemax)::dut1
    double precision,dimension(ndatemaxdc)::dut2
    !double precision,dimension(ndatemeta)::dut3
    double precision,dimension(ndatemaxdc)::dut4
        ! hazards (Derivative of cumulative hazards) at each event time on the grid of event times
    double precision,dimension(0:ndatemax)::ut1
    double precision,dimension(0:ndatemaxdc)::ut2
    !double precision,dimension(0:ndatemeta)::ut3
    double precision,dimension(0:ndatemaxdc)::ut4
        ! cumulative hazards at each event time on the grid of event times
    double precision::int
	
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: Setup."
    !close(1)

    choix=0
    ig=0
    vj=0
    n=0
    j=0
    k=0
    l = 0
    bh=b
    pi=3.141592653589793d0
    ut1=0.d0
    ut2=0.d0
    !ut3=0.d0
    ut4=0.d0
    dut1=0.d0
    dut2=0.d0
    !dut3=0.d0
    dut4=0.d0
	
!---------------- Unpack model parameters-------------------
! Add small differences when approximating derivatives.
    if(id.ne.0) bh(id)=bh(id)+thi
    if(jd.ne.0) bh(jd)=bh(jd)+thj

! Copy spline coefficients
    the1=0.d0
    the2=0.d0
    !the3=0.d0
    the4=0.d0
        the1(-2:(nzloco-1))=dexp(bh(1:(nzloco+2)))!**2.0d0
        the2(-2:(nzloco-1))=dexp(bh((nzloco+3):(2*nzloco+4)))!**2.0d0
!        the3(i-3)=dexp(bh(nzloco+nzdc+4+i))**2.0d0
        the4(-2:(nzloco-1))=dexp(bh((2*nzloco+5):(3*nzloco+6)))!**2.0d0

! One Random Effect Model
    if(typeJoint.eq.0)then
        ! b = c(splinesR1, splinesT1, splinesR2, splinesT2, theta, alpha1, alpha2, coef...)
        theta = dexp(bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)-2))
        ! random effect sd ---> varaince
        alpha1= bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)-1) 
        ! random effect link to terminal event process 1
        alpha2= bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)) 
        ! random effect link to terminal event process 2
        rho = 0.d0 !na
        theta2 = 1.d0 !na
    else
! Two Random Effect Model
        ! b = c(splinesR1, splinesT1, splinesR2, splinesT2, theta, theta2, rho, alpha1, alpha2, coef...)
        theta = dexp(bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)-4))
        ! random effect sd ---> varaince
        theta2 = dexp(bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)-3))
        ! random effect sd ---> varaince
        rho = (2.d0*dexp(bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)-2))/&
            (1.d0+dexp(bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)-2)))) - 1.d0 
            ! transform correlation from unbounded to (-1,1) using inverse logit
        alpha1=bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)-1) 
            ! random effect link to terminal event process 1
        alpha2=bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)) 
            ! random effect link to terminal event process 2
    end if	
    inv = 1.d0/theta
    inv2 = 1.d0/theta2

    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: Parameters: theta = " , theta,&
    !"alpha1=",alpha1, "alpha2=",alpha2, "rho=", rho
    !write(1,*) "funcpaMultivSplines.f90:: Parameters: the1=", the1
    !write(1,*) "funcpaMultivSplines.f90:: Parameters: the2=", the2
    !write(1,*) "funcpaMultivSplines.f90:: Parameters: the4=", the4 
    !close(1)

!---------------------------------------------------------------------
! Insert leading values into hazards and cumulative hazards
    dut1(1) = (the1(-2)*4.d0/(zi(2)-zi(1)))
    dut2(1) = (the2(-2)*4.d0/(zidc(2)-zidc(1)))
!    dut3(1) = (the3(-2)*4.d0/(zimeta(2)-zimeta(1)))
    dut4(1) = (the4(-2)*4.d0/(zidc(2)-zidc(1)))

    ut1(1) = the1(-2)*dut1(1)*0.25d0*(zi(1)-zi(-2))
    ut2(1) = the2(-2)*dut2(1)*0.25d0*(zidc(1)-zidc(-2))
!    ut3(1) = the3(-2)*dut3(1)*0.25d0*(zimeta(1)-zimeta(-2))
    ut4(1) = the4(-2)*dut4(1)*0.25d0*(zidc(1)-zidc(-2))

    ut1(0) = 0.d0
    ut2(0) = 0.d0
!    ut3(0) = 0.d0
    ut4(0) = 0.d0

!---------------------------------------------------------------------
! Calculate hazard and cumulative hazard at each event time in grid
!--- Recurrent Event 1...
    som1 = 0.d0
    vj = 0
    do i=2,ndate-1
        do k = 2,nzloco!n-2
            if (((date(i)).ge.(zi(k-1) - 0.00001d0)).and.(date(i).lt.(zi(k)-0.01d0)))then
                j = k-1
                if ((j.gt.1).and.(j.gt.vj))then
                som1 = som1 + the1(j-4)
                vj  = j
                endif
            endif
        end do

        ut1(i) = som1 +(the1(j-3)*im3(i))+(the1(j-2)*im2(i)) &
        +(the1(j-1)*im1(i))+(the1(j)*im(i))

        dut1(i) = (the1(j-3)*mm3(i))+(the1(j-2)*mm2(i)) &
        +(the1(j-1)*mm1(i))+(the1(j)*mm(i))
    end do

!--- Terminal Event 1...
    vj = 0
    som2 = 0.d0
    do i=2,ndatedc-1
        do k = 2,nzdc!n-2
            if (((datedc(i)).ge.(zidc(k-1)-0.00001d0)).and.(datedc(i).lt.(zidc(k)-0.01d0)))then
                j = k-1
                if ((j.gt.1).and.(j.gt.vj))then
                som2 = som2 + the2(j-4)
                vj  = j
                endif
            endif
        end do
        ut2(i) = som2 +(the2(j-3)*im3dc(i))+(the2(j-2)*im2dc(i)) &
        +(the2(j-1)*im1dc(i))+(the2(j)*imdc(i))

        dut2(i) = (the2(j-3)*mm3dc(i))+(the2(j-2)*mm2dc(i)) &
        +(the2(j-1)*mm1dc(i))+(the2(j)*mmdc(i))
    end do

!--- Something for Recurrent Event 2...
!    som3 = 0.d0
!    vj = 0
!    do i=2,ndatemeta-1
!        do k = 2,nzmeta
!            if (((datemeta(i)).ge.(zimeta(k-1))).and.(datemeta(i).lt.zimeta(k)))then
!                j = k-1
!                if ((j.gt.1).and.(j.gt.vj))then
!                som3 = som3 + the3(j-4)
!                vj  = j
!                endif
!            endif
!        end do

!        ut3(i) = som3 +(the3(j-3)*im3meta(i))+(the3(j-2)*im2meta(i)) &
!        +(the3(j-1)*im1meta(i))+(the3(j)*immeta(i))

!        dut3(i) = (the3(j-3)*mm3meta(i))+(the3(j-2)*mm2meta(i)) &
!        +(the3(j-1)*mm1meta(i))+(the3(j)*mmmeta(i))

!    end do
	
!--- Terminal Event 2...
    vj = 0
    som4 = 0.d0
    do i=2,ndatedc-1
        do k = 2,nzdc!n-2
            if (((datedc(i)).ge.(zidc(k-1))).and.(datedc(i).lt.zidc(k)))then
                j = k-1
                if ((j.gt.1).and.(j.gt.vj))then
                som4 = som4 + the4(j-4)
                vj  = j
                endif
            endif
        end do
        ut4(i) = som4 +(the4(j-3)*im3dc(i))+(the4(j-2)*im2dc(i)) &
        +(the4(j-1)*im1dc(i))+(the4(j)*imdc(i))
        dut4(i) = (the4(j-3)*mm3dc(i))+(the4(j-2)*mm2dc(i)) &
        +(the4(j-1)*mm1dc(i))+(the4(j)*mmdc(i))
    end do
! End: Do something for each type of event
!---------------------------------------------------------------------
! Insert tail values in hazards/cumulative hazards
    i = nzloco
    h1 = (zi(i)-zi(i-1))
    ut1(ndate)=som1+the1(i-4)+the1(i-3)+the1(i-2)+the1(i-1)
    dut1(ndate) = (4.d0*the1(i-1)/h1)

    i = nzdc
    h1 = (zidc(i)-zidc(i-1))
    ut2(ndatedc)=som2+the2(i-4)+the2(i-3)+the2(i-2)+the2(i-1)!am the1(i-4)
    dut2(ndatedc) = (4.d0*the2(i-1)/h1)

!    i = nzmeta
!    h1 = (zimeta(i)-zimeta(i-1))
!    ut3(ndatemeta)=som3+the3(i-4)+the3(i-3)+the3(i-2)+the3(i-1)
!    dut3(ndatemeta) = (4.d0*the3(i-1)/h1)
	
    i = nzdc
    h1 = (zidc(i)-zidc(i-1))
    ut4(ndatedc)=som4+the4(i-4)+the4(i-3)+the4(i-2)+the4(i-1)
    dut4(ndatedc) = (4.d0*the4(i-1)/h1)
! End: Do something else for each type of event
!----------------------------------------------------------------------
! Begin Likelihood Calculation 

!---- with or without explanatory variables ------

    do k=1,ng
        res1(k) = 0.d0
        res2(k) = 0.d0
        res3(k) = 0.d0
        res1dc(k) = 0.d0
        res2dc(k) = 0.d0
        res3dc(k) = 0.d0
        !res1meta(k) = 0.d0
        !res2meta(k) = 0.d0
        !res3meta(k) = 0.d0
        res1dc2(k) = 0.d0
        res2dc2(k) = 0.d0
        res3dc2(k) = 0.d0
        cpt(k) = 0
        !cptmeta(k) = 0
        integrale1(k) = 0.d0
        integrale2(k) = 0.d0
        integrale3(k) = 0.d0
        aux1(k)=0.d0
        aux2(k)=0.d0
    end do


!---------------------------------------------
! Likelihood Contribuiton of Recurrent Event 1
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: Likelihood R1."
    !close(1)

    do i=1,nsujet
        ! (1) count the number of events per individual
        cpt(g(i))=cpt(g(i))+1

        ! (2) compute the relative hazard for each event
        if(nva1.gt.0)then
            vet = 0.d0
            do j=1,nva1
                vet =vet + bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)+j)*dble(ve(i,j))
            end do
            vet = dexp(vet)
        else
            vet=1.d0
        endif

        ! (3a) Compute hazard for non-censored events
        if(c(i).eq.1)then
            res2(g(i)) = res2(g(i))+dlog(dut1(nt1(i)))+dlog(vet)
            !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            !write(1,*) "funcpaMultivSplines.f90:: dlog(dut1(nt1(i)))+dlog(vet) = ", dlog(dut1(nt1(i)))+dlog(vet)
            !close(1)
        endif

        if ((res2(g(i)).ne.res2(g(i))).or.(abs(res2(g(i))).ge.1.d30)) then
            funcpaMultivSplines=-1.d9
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivSplines.f90:: Error with Recurrent event 1 hazards."
            write(1,*) "funcpaMultivSplines.f90:: (res2(g(i)).ne.res2(g(i)) = ", res2(g(i)).ne.res2(g(i))
            write(1,*) "funcpaMultivSplines.f90:: abs(res2(g(i))).ge.1.d30) = ", abs(res2(g(i))).ge.1.d30
            write(1,*) "funcpaMultivSplines.f90:: i = ", i
            write(1,*) "funcpaMultivSplines.f90:: g(i) = ", g(i)
            write(1,*) "funcpaMultivSplines.f90:: ng = ", ng, "ngmax = ", ngmax
            write(1,*) "funcpaMultivSplines.f90:: res2(g(i)) =", res2(g(i))
            write(1,*) "funcpaMultivSplines.f90:: nt1(i) = ", nt1(i)
            write(1,*) "funcpaMultivSplines.f90:: t0(i)) = ", t0(i)
            write(1,*) "funcpaMultivSplines.f90:: t1(i)) = ", t1(i)
            write(1,*) "funcpaMultivSplines.f90:: date(nt1(i)) = ", date(nt1(i))
            write(1,*) "funcpaMultivSplines.f90:: ut1(nt1(i)) = ", ut1(nt1(i))
            write(1,*) "funcpaMultivSplines.f90:: dut1(nt1(i)) = ", dut1(nt1(i))
            write(1,*) "funcpaMultivSplines.f90:: vet = ", vet
            write(1,*) "funcpaMultivSplines.f90:: nzloco = ", nzloco
            write(1,*) "funcpaMultivSplines.f90:: zi = ", zi
            write(1,*) "funcpaMultivSplines.f90:: the1 = ", the1
            write(1,*) "funcpaMultivSplines.f90:: res2 = ", res2
            close(1)
            goto 123
        end if

        ! (3b) Compute cumulative hazard for all events (censored and non-censored)
        ! this part goes into the integral
        res1(g(i)) = res1(g(i)) + ut1(nt1(i))*vet
        !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
        !write(1,*) "funcpaMultivSplines.f90:: ut1(nt1(i))*vet = ", ut1(nt1(i))*vet
        !close(1)
        if ((res1(g(i)).ne.res1(g(i))).or.(abs(res1(g(i))).ge. 1.d30)) then
            funcpaMultivSplines=-1.d9
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivSplines.f90:: Error with Recurrent event 1 cumulative hazards."
            write(1,*) "funcpaMultivSplines.f90:: i = ", i
            write(1,*) "funcpaMultivSplines.f90:: g(i) = ", g(i)
            write(1,*) "funcpaMultivSplines.f90:: res2(g(i)) =", res2(g(i))
            write(1,*) "funcpaMultivSplines.f90:: nt1(i) = ", nt1(i)
            write(1,*) "funcpaMultivSplines.f90:: t0(i)) = ", t0(i)
            write(1,*) "funcpaMultivSplines.f90:: t1(i)) = ", t1(i)
            write(1,*) "funcpaMultivSplines.f90:: date(nt1(i)) = ", date(nt1(i))
            write(1,*) "funcpaMultivSplines.f90:: ut1(nt1(i)) = ", ut1(nt1(i))
            write(1,*) "funcpaMultivSplines.f90:: dut1(nt1(i)) = ", dut1(nt1(i))
            write(1,*) "funcpaMultivSplines.f90:: vet = ", vet
            write(1,*) "funcpaMultivSplines.f90:: nzloco = ", nzloco
            write(1,*) "funcpaMultivSplines.f90:: zi = ", zi
            write(1,*) "funcpaMultivSplines.f90:: the1 = ", the1
            close(1)
            goto 123
        end if

        ! (3c) Modification for cumulative hazard with left truncation (for calendar time model 
        ! as opposed to gap time model)
        res3(g(i)) = res3(g(i)) + ut1(nt0(i))*vet
        !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
        !write(1,*) "funcpaMultivSplines.f90:: ut1(nt0(i))*vet = ", ut1(nt0(i))*vet
        !close(1)
        if ((res3(g(i)).ne.res3(g(i))).or.(abs(res3(g(i))).ge. 1.d30)) then
            funcpaMultivSplines=-1.d9
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivSplines.f90:: (res3(g(i)).ne.res3(g(i) = ", res3(g(i)).ne.res3(g(i))
            write(1,*) "funcpaMultivSplines.f90:: abs(res3(g(i))).ge. 1.d30) = ", abs(res3(g(i))).ge.1.d30
            write(1,*) "funcpaMultivSplines.f90:: i = ", i
            write(1,*) "funcpaMultivSplines.f90:: g(i) = ", g(i)
            write(1,*) "funcpaMultivSplines.f90:: ng = ", ng, "ngmax = ", ngmax
            write(1,*) "funcpaMultivSplines.f90:: res2(g(i)) =", res2(g(i))
            write(1,*) "funcpaMultivSplines.f90:: nt1(i) = ", nt1(i)
            write(1,*) "funcpaMultivSplines.f90:: t0(i)) = ", t0(i)
            write(1,*) "funcpaMultivSplines.f90:: t1(i)) = ", t1(i)
            write(1,*) "funcpaMultivSplines.f90:: date(nt1(i)) = ", date(nt1(i))
            write(1,*) "funcpaMultivSplines.f90:: ut1(nt1(i)) = ", ut1(nt1(i))
            write(1,*) "funcpaMultivSplines.f90:: dut1(nt1(i)) = ", dut1(nt1(i))
            write(1,*) "funcpaMultivSplines.f90:: vet = ", vet
            write(1,*) "funcpaMultivSplines.f90:: nzloco = ", nzloco
            write(1,*) "funcpaMultivSplines.f90:: zi = ", zi
            write(1,*) "funcpaMultivSplines.f90:: the1 = ", the1
            write(1,*) "funcpaMultivSplines.f90:: res2 = ", res2
            close(1)
            goto 123
        end if
    end do

!---------------------------------------------
! Likelihood Contribuiton of Terminal Event 1
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: Likelihood T1."
    !close(1)
    do k=1,ng
        ! (2) compute the relative hazard for each person
        if(nva2.gt.0)then
            vet2 = 0.d0
            do j=1,nva2
                vet2 = vet2 + bh(np-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)+j)*dble(vedc(k,j))
            end do
            vet2 = dexp(vet2)
        else
            vet2=1.d0
        endif

        ! (3a) Compute hazard for non-censored terminal events
        if(cdc(k).eq.1)then
            res2dc(k) = dlog(dut2(nt1dc(k))*vet2)

            if ((res2dc(k).ne.res2dc(k)).or.(abs(res2dc(k)).ge. 1.d30)) then
                funcpaMultivSplines=-1.d9
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivSplines.f90:: Error with terminal event 1 hazards."
                write(1,*) "funcpaMultivSplines.f90:: k = ", k, "of ", ng
                write(1,*) "funcpaMultivSplines.f90:: res2dc(k)= ", res2dc(k)
                write(1,*) "funcpaMultivSplines.f90:: nt1dc(k) = ", nt1dc(k)
                write(1,*) "funcpaMultivSplines.f90:: t0dc(k)) = ", t0dc(k)
                write(1,*) "funcpaMultivSplines.f90:: t1dc(k)) = ", t1dc(k)
                write(1,*) "funcpaMultivSplines.f90:: datedc(nt1dc(k)) = ", datedc(nt1dc(k))
                write(1,*) "funcpaMultivSplines.f90:: ut2(nt1dc(k)) = ", ut2(nt1dc(k))
                write(1,*) "funcpaMultivSplines.f90:: dut2(nt1dc(k)) = ", dut2(nt1dc(k))
                write(1,*) "funcpaMultivSplines.f90:: vet(k) = ", vet2
                write(1,*) "funcpaMultivSplines.f90:: zidc = ", zidc
                write(1,*) "funcpaMultivSplines.f90:: nzdc = ", nzdc				
                write(1,*) "funcpaMultivSplines.f90:: the2 = ", the2
                close(1)
                goto 123
            end if
        endif

        ! (3b) Compute cumulative hazard for all terminal (censored and non-censored)
        ! this part goes into the integral
        aux1(k)=ut2(nt1dc(k))*vet2
        if ((aux1(k).ne.aux1(k)).or.(abs(aux1(k)).ge. 1.d30)) then
            funcpaMultivSplines=-1.d9
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivSplines.f90:: Error with terminal event 1 cumulative hazards."
            write(1,*) "funcpaMultivSplines.f90:: k = ", k
            write(1,*) "funcpaMultivSplines.f90:: res2dc(k)= ", res2dc(k)
            write(1,*) "funcpaMultivSplines.f90:: nt1dc(i) = ", nt1dc(k)
            write(1,*) "funcpaMultivSplines.f90:: t0dc(i)) = ", t0dc(k)
            write(1,*) "funcpaMultivSplines.f90:: t1dc(i)) = ", t1dc(k)
            write(1,*) "funcpaMultivSplines.f90:: ndatedc = ", ndatedc
            write(1,*) "funcpaMultivSplines.f90:: datedc(nt1dc(i)) = ", datedc(nt1dc(k))
            write(1,*) "funcpaMultivSplines.f90:: ut1(nt1dc(i)) = ", ut2(nt1dc(k))
            write(1,*) "funcpaMultivSplines.f90:: dut1(nt1dc(i)) = ", dut2(nt1dc(k))
            write(1,*) "funcpaMultivSplines.f90:: vet2(i) = ", vet2
            write(1,*) "funcpaMultivSplines.f90:: zidc = ", zidc
            write(1,*) "funcpaMultivSplines.f90:: the2 = ", the2
            write(1,*) "funcpaMultivSplines.f90:: ut1 Components:"
            write(1,*) "funcpaMultivSplines.f90:: im3dc", im3dc(1:ndatedc)
            write(1,*) "funcpaMultivSplines.f90:: im2dc", im2dc(1:ndatedc)
            write(1,*) "funcpaMultivSplines.f90:: im1dc", im1dc(1:ndatedc)
            write(1,*) "funcpaMultivSplines.f90:: imdc", imdc(1:ndatedc)
            write(1,*) "funcpaMultivSplines.f90:: mm3dc", mm3dc(1:ndatedc)
            write(1,*) "funcpaMultivSplines.f90:: mm2dc", mm2dc(1:ndatedc)
            write(1,*) "funcpaMultivSplines.f90:: mm1dc", mm1dc(1:ndatedc)
            write(1,*) "funcpaMultivSplines.f90:: mmdc", mmdc(1:ndatedc)
            close(1)
            goto 123
        end if
    end do

!---------------------------------------------
! Likelihood Contribuiton of Recurrent Event 2
!    do i=1,nsujetmeta
!        cptmeta(gmeta(i))=cptmeta(gmeta(i))+1
!        if(nva3.gt.0)then
!            vet3 = 0.d0
!            do j=1,nva3
!                vet3 =vet3 + bh(np-nva3-nva4+j)*dble(vemeta(i,j))
!            end do
!            vet3 = dexp(vet3)
!        else
!            vet3=1.d0
!        endif
!
!        if((cmeta(i).eq.1))then
!            res2meta(gmeta(i)) = res2meta(gmeta(i))+dlog(dut3(nt1meta(i))*vet3)
!        endif

!  Cumulative risk function of recurrence at tepms T_ij
!        res1meta(gmeta(i)) = res1meta(gmeta(i))+ut3(nt1meta(i))*vet3
! Risk function for recurrence at tepms T_i(j-1)
!        res3meta(gmeta(i)) = res3meta(gmeta(i))+ut3(nt0meta(i))*vet3
!    end do
	
!---------------------------------------------
! Likelihood Contribuiton of Terminal Event 2
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: Likelihood T2."
    !close(1)
    do k=1,ng
        ! (2) compute the relative hazard for each person
        if(nva4.gt.0)then
            vet4 = 0.d0
            do j=1,nva4
                vet4 = vet4 + bh(np-(nva4*terminal2_ind)+j)*dble(vedc2(k,j))
            end do
            vet4 = dexp(vet4)
        else
            vet4=1.d0
        endif
        ! (3a) Compute hazard for non-censored terminal events
        if(cdc2(k).eq.1)then
            res2dc2(k) = dlog(dut4(nt1dc(k))*vet4)
            if ((res2dc2(k).ne.res2dc2(k)).or.(abs(res2dc2(k)).ge. 1.d30)) then
                funcpaMultivSplines=-1.d9
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivSplines.f90:: Error with terminal event 2 hazards."
                close(1)
                goto 123
            end if
        endif
        ! (3b) Compute cumulative hazard for all terminal (censored and non-censored)
        ! this part goes into the integral
        aux2(k)=ut4(nt1dc(k))*vet4
        if ((aux2(k).ne.aux2(k)).or.(abs(aux2(k)).ge. 1.d30)) then
            funcpaMultivSplines=-1.d9
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivSplines.f90:: Error with terminal event 1 cumulative hazards."
            close(1)
            goto 123
        end if
    end do

!----------------------------------
! Integral Component of Likelihood

! compute integral for each person
! the integral component will depend on the parameterization of the random effects
! typeJoint = 0 for 1 random effect ~ N(0, theta)
! typeJoint = 1 for 2 correlated random effects ~ N(0, [theta, theta2, rho*sqrt(theta*theta2)])

!    do ig=1,ng
!        auxig=ig
!        call gausshermiteBIS2011(int,30)
!        integrale3(ig) = int !moins bon
!    end do

    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: Likelihood Integral."
    !close(1)

    if(typeJoint.eq.0)then
        do k=1, ng
            ss=0.d0
            do j=1, ghPoints
                weight = ghWeights(j)
                frail = ghNodes(j)
                ss = ss + &
                (weight* dexp( & ! GQ Weights
                frail * cpt(k) - dexp(frail)*(res1(k)-res3(k)) & ! recurrent event 1
                + alpha1 * frail * cdc(k) - dexp(frail*alpha1) * aux1(k) & ! terminal event 1
                + alpha2 * frail * cdc2(k) - dexp(frail*alpha2) * aux2(k) & ! terminal event 2
                - (frail**2.d0)/(2.d0*theta)))! frailty distribution (normal)
            end do
            if((ss.eq.0.0d0)) ss = 1.0d-10 ! integral should not be too small when theta is large
            integrale3(k) = ss
            if ((integrale3(k).ne.integrale3(k)).or.(abs(integrale3(k)).ge.1.d30)) then
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivWwib.f90:: Error with integral component of likelihood."
                write(1,*) "typeJoint = ", typeJoint
                write(1,*) "theta = ", theta
                write(1,*) "alpha1 = ", alpha1
                write(1,*) "alpha2 = ", alpha2
                write(1,*) "k = ", k
                write(1,*) "cpt(k) = ", cpt(k)
                write(1,*) "res1(k) = ", res1(k)
                write(1,*) "res3(k) = ", res3(k)
                write(1,*) "cdc(k) = ", cdc(k)
                write(1,*) "cdc2(k) = ", cdc2(k)
                write(1,*) "aux1(k) = ", aux1(k)
                write(1,*) "aux2(k) = ", aux2(k)
                write(1,*) "integrale3(k) = ", integrale3(k)
                close(1)
                funcpaMultivSplines=-1.d9
                goto 123
            end if
        end do
    else ! typeJoint = 1
        do k=1, ng
            ss=0.d0
            do j=1, ghPoints ! randdom effect for terminal 1
                do l = 1, ghPoints ! random effect for terminal 2
                    weight = ghWeights(j)
                    weight2 = ghWeights(l)
                    frail = ghNodes(j)
                    frail2 = ghNodes(l)
                    ss = ss + &
                    (weight * weight2 * dexp( & ! GQ Weights
                    (frail + frail2) * cpt(k) - dexp(frail + frail2)*(res1(k)-res3(k)) & ! recurrent event 1
                    + alpha1 * frail * cdc(k) - dexp(frail*alpha1) * aux1(k) & ! terminal event 1
                    + alpha2 * frail2 * cdc2(k) - dexp(frail2*alpha2) * aux2(k) & ! terminal event 2
                    - (((frail**2.d0)/theta) + ((frail2**2.d0)/theta2) - &
                    ((2.0d0*frail*frail2*rho)/dsqrt(theta)/dsqrt(theta2)))&
                    /(2.d0*(1.d0-(rho**2.d0)))))! frailty distribution (multivariate normal)
                end do
            end do
            if((ss.eq.0.0d0)) ss = 1.0d-10 ! integral should not be too small when theta is large
            integrale3(k) = ss
            if (integrale3(k).ne.integrale3(k).or.(abs(integrale3(k)).ge.1.d30)) then
                write(1,*) "funcpaMultivSplines.f90:: Error with integral component of likelihood."
                write(1,*) "integrale3(k) = ", integrale3(k)
                write(1,*) "typeJoint = ", typeJoint
                write(1,*) "theta = ", theta
                write(1,*) "theta2 = ", theta2
                write(1,*) "rho = ", rho
                write(1,*) "alpha1 = ", alpha1
                write(1,*) "alpha2 = ", alpha2
                write(1,*) "k = ", k
                write(1,*) "cpt(k) = ", cpt(k)
                write(1,*) "res1(k) = ", res1(k)
                write(1,*) "res3(k) = ", res3(k)
                write(1,*) "cdc(k) = ", cdc(k)
                write(1,*) "cdc2(k) = ", cdc2(k)
                write(1,*) "aux1(k) = ", aux1(k)
                write(1,*) "aux2(k) = ", aux2(k)
                close(1)
                funcpaMultivSplines=-1.d9
                goto 123
            end if
        end do
        !open(1, file = '../package_tests/multiv_model_integrals.dat',position="append")  
        !write(1,*) integrale3
        !close(1)
    end if

!--------------------------------------------------
! Combine Likelihood
!--------------------------------------------------
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: Combine Likelihood."
    !close(1)
	
    !open(1, file = '../package_tests/multiv_model_likelihood.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: res2 = ", res2
    !write(1,*) "funcpaMultivSplines.f90:: res2 = ", res2
    !close(1)

    res = 0.d0
	
    if(typeJoint.eq.0)then
        do k=1,ng
            res = res + res2(k)+res2dc(k)+res2dc2(k)+dlog(integrale3(k))- &
            dlog(dsqrt(theta))- dlog(2.d0*pi)/2.d0 
            if ((res.ne.res).or.(abs(res).ge.1.d30)) then
                funcpaMultivSplines=-1.d9
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivSplines.f90:: Error with sum of log likelihoods."
                write(1,*) "typeJoint = ", typeJoint
                write(1,*) "theta = ", theta
                write(1,*) "k = ", k
                write(1,*) "integrale3(k) = ", integrale3(k)
                write(1,*) "res2dc2(k) = ", res2dc2(k)
                write(1,*) "res2dc(k) = ", res2dc(k)
                write(1,*) "res2(k) = ", res2(k)
                close(1)
                Rrec(k)=0.d0
                Nrec(k)=0
                Rdc(k)=0.d0
                Ndc(k)=0
                goto 123
            else
                funcpaMultivSplines = res
                Rrec(k)=res1(k)
                Nrec(k)=nig(k)

                Rdc(k)=aux1(k)
                Ndc(k)=cdc(k)
            end if
        end do
    else !typeJoint = 1
        do k=1,ng
            res = res + res2(k)+res2dc(k)+res2dc2(k)+dlog(integrale3(k))- &
            dlog(dsqrt(theta)) - dlog(dsqrt(theta2)) - dlog(dsqrt(1.0d0-(rho**2.0d0))) - dlog(2.d0*pi)
            if ((res.ne.res).or.(abs(res).ge.1.d30)) then
                funcpaMultivSplines=-1.d9
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivSplines.f90:: Error with sum of log likelihoods."
                write(1,*) "typeJoint = ", typeJoint
                write(1,*) "theta = ", theta
                write(1,*) "k = ", k
                write(1,*) "integrale3(k) = ", integrale3(k)
                write(1,*) "res2dc2(k) = ", res2dc2(k)
                write(1,*) "res2dc(k) = ", res2dc(k)
                write(1,*) "res2(k) = ", res2(k)
                close(1)
                Rrec(k)=0.d0
                Nrec(k)=0
                Rdc(k)=0.d0
                Ndc(k)=0
                goto 123
            else
                funcpaMultivSplines = res
                Rrec(k)=res1(k)
                Nrec(k)=nig(k)

                Rdc(k)=aux1(k)
                Ndc(k)=cdc(k)
            end if
        end do
    end if

!-------------------------------------------------
! Calculate Penalization

    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: Calculate Penalties."
    !close(1)

    pe1 = 0.d0
    pe2 = 0.d0
    !pe3 = 0.d0
    pe4 = 0.d0

    do i=1,nzloco-1
        pe1 = pe1+(the1(i-3)*the1(i-3)*m3m3(i))+(the1(i-2) &
        *the1(i-2)*m2m2(i))+(the1(i-1)*the1(i-1)*m1m1(i))+( &
        the1(i)*the1(i)*mmm(i))+(2.d0*the1(i-3)*the1(i-2)* &
        m3m2(i))+(2.d0*the1(i-3)*the1(i-1)*m3m1(i))+(2.d0* &
        the1(i-3)*the1(i)*m3m(i))+(2.d0*the1(i-2)*the1(i-1)* &
        m2m1(i))+(2.d0*the1(i-2)*the1(i)*m2m(i))+(2.d0*the1(i-1) &
        *the1(i)*m1m(i))
    end do
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: R1 Penalty  Calculated."
    !close(1)
    do i=1,nzdc-1
        pe2 = pe2+(the2(i-3)*the2(i-3)*m3m3b(i))+(the2(i-2) &
        *the2(i-2)*m2m2b(i))+(the2(i-1)*the2(i-1)*m1m1b(i))+( &
        the2(i)*the2(i)*mmmb(i))+(2.d0*the2(i-3)*the2(i-2)* &
        m3m2b(i))+(2.d0*the2(i-3)*the2(i-1)*m3m1b(i))+(2.d0* &
        the2(i-3)*the2(i)*m3mb(i))+(2.d0*the2(i-2)*the2(i-1)* &
        m2m1b(i))+(2.d0*the2(i-2)*the2(i)*m2mb(i))+(2.d0*the2(i-1) &
        *the2(i)*m1mb(i))
    end do
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: T1 Penalty  Calculated."
    !close(1)
    !do i=1,nzmeta-1
    !    pe3 = pe3+(the3(i-3)*the3(i-3)*m3m3c(i))+(the3(i-2) &
    !    *the3(i-2)*m2m2c(i))+(the3(i-1)*the3(i-1)*m1m1c(i))+(  &
    !    the3(i)*the3(i)*mmmc(i))+(2.d0*the3(i-3)*the3(i-2)*  &
    !    m3m2c(i))+(2.d0*the3(i-3)*the3(i-1)*m3m1c(i))+(2.d0*  &
    !    the3(i-3)*the3(i)*m3mc(i))+(2.d0*the3(i-2)*the3(i-1)*  &
    !    m2m1c(i))+(2.d0*the3(i-2)*the3(i)*m2mc(i))+(2.d0*the3(i-1)  &
    !    *the3(i)*m1mc(i))
    !end do
!    open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
!    write(1,*) "funcpaMultivSplines.f90:: nzdc = ", nzdc
!    write(1,*) "funcpaMultivSplines.f90:: the4 = ", the4
!    write(1,*) "funcpaMultivSplines.f90:: m3m3d = ", m3m3d
!    write(1,*) "funcpaMultivSplines.f90:: m2m2d = ", m2m2d
!    write(1,*) "funcpaMultivSplines.f90:: m1m1d = ", m1m1d
!    write(1,*) "funcpaMultivSplines.f90:: mmmd = ", mmmd
!    write(1,*) "funcpaMultivSplines.f90:: m3m2d = ", m3m2d
!    write(1,*) "funcpaMultivSplines.f90:: m3m1d = ", m3m1d
!    write(1,*) "funcpaMultivSplines.f90:: m3md = ", m3md
!    write(1,*) "funcpaMultivSplines.f90:: m2m1d = ", m2m1d
!    write(1,*) "funcpaMultivSplines.f90:: m2md = ", m2md
!    write(1,*) "funcpaMultivSplines.f90:: m1md = ", m1md
!    close(1)
    do i=1,nzdc-1
        pe4 = pe4+(the4(i-3)*the4(i-3)*m3m3d(i))+(the4(i-2) &
        *the4(i-2)*m2m2d(i))+(the4(i-1)*the4(i-1)*m1m1d(i))+( &
        the4(i)*the4(i)*mmmd(i))+(2.d0*the4(i-3)*the4(i-2)* &
        m3m2d(i))+(2.d0*the4(i-3)*the4(i-1)*m3m1d(i))+(2.d0* &
        the4(i-3)*the4(i)*m3md(i))+(2.d0*the4(i-2)*the4(i-1)* &
        m2m1d(i))+(2.d0*the4(i-2)*the4(i)*m2md(i))+(2.d0*the4(i-1) &
        *the4(i)*m1md(i))
    end do
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: T2 Penalty  Calculated."
    !close(1)

    ! Overall penalty term
    pe = k0(1)*pe1 + k0(2)*pe2 + k0(4)*pe4 !+ko(3*pe3)

    ! Non-Penalized Likelihood
    resnonpen = res

    ! Penalized Likelihood
    res = res - pe
	
!    open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
!    write(1,*) "funcpaMultivSplines.f90:: pe = ", pe
!    write(1,*) "funcpaMultivSplines.f90:: k0 = ", k0
!    write(1,*) "funcpaMultivSplines.f90:: resnonpen = ", resnonpen
!    write(1,*) "funcpaMultivSplines.f90:: res = ", res
!    close(1)

!Ad:
123     continue
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivSplines.f90:: Done."
    !close(1)


    return

    end function funcpaMultivSplines



!==========================  DISTANCE   =================================

    subroutine distanceJ_splines(nzloco,nzdc,nzmeta,b,mt1,mt2,mt3,x1Out,lamOut,suOut,x2Out,lam2Out,su2Out,x3Out,lam3Out,su3Out)

    use taillesmultiv
    !use comonmultiv,only:c,cdc,date,datedc,hess,Hspl_hess,I_hess,ndate,ndatedc,&
    !nt0,nt1,nt0dc,nt1dc,nsujet,nva,nva1,nva2,t0,t1,t0dc,t1dc,PEN_deri
    use comonmultiv,only:zi,zidc,zimeta,H_hess !,nst &    

    Implicit none

    integer,intent(in):: nzloco,nzdc,nzmeta,mt1,mt2,mt3
    double precision ,dimension(npmax),intent(in):: b
    integer::i,j,n,k,l      
    double precision::x1,x2,h,hdc,hmeta,su,bsup,binf,lam,lbinf,lbsup
    double precision ,dimension(npmax,npmax)::hes1,hes2,hes3
    double precision ,dimension(-2:npmax):: the1,the2,the3     
    double precision,dimension(mt1)::x1Out
    double precision,dimension(mt2)::x2Out
    double precision,dimension(mt3)::x3Out
    double precision,dimension(mt1,3):: lamOut,suOut
    double precision,dimension(mt2,3):: lam2Out,su2Out
    double precision,dimension(mt3,3):: lam3Out,su3Out    

    n  = nzloco+2
    !loco
    do i=1,nzloco+2
        do j=1,nzloco+2
            hes1(i,j)=h_Hess(i,j)
        end do
    end do

    !dc
    k = 0
    do i=nzloco+3,nzloco+2+nzdc+2
        k = k + 1
        l = 0
        do j=nzloco+3,nzloco+2+nzdc+2
            l = l + 1
            hes2(k,l)=H_hess(i,j)
        end do
    end do

    !meta
    k = 0
    do i=nzloco+nzdc+5,nzloco+2+nzdc+2+nzmeta+2
        k = k + 1
        l = 0
        do j=nzloco+nzdc+5,nzloco+2+nzdc+2+nzmeta+2
            l = l + 1
            hes2(k,l)=H_hess(i,j)
        end do
    end do


    do i=1,nzloco+2
        the1(i-3)=(b(i))*(b(i))
    end do

    do i=1,nzdc+2
        j = nzloco+2+i
        the2(i-3)=(b(j))*(b(j))
    end do

    do i=1,nzmeta+2
        j = nzloco+2+nzdc+2+i
        the3(i-3)=(b(j))*(b(j))
    end do

    h = (zi(n)-zi(1))*0.01d0
    hdc = (zidc(n)-zidc(1))*0.01d0
    hmeta = (zimeta(n)-zimeta(1))*0.01d0

! Recurrent
    x1 = zi(1)
    x2 = zi(1)
    do i=1,mt1
        if(i .ne.1)then
            x1 = x1 + h
        end if
        call cosp(x1,the1,nzloco+2,hes1,zi,binf,su,bsup,lbinf,lam,lbsup)

        if(bsup.lt.0.d0)then
            bsup = 0.d0
        endif
        if(binf.gt.1.d0)then
            binf = 1.d0
        endif
        if(lbinf.lt.0.d0)then
            lbinf = 0.d0
        endif 
!!!   Replaced by next sentences and add new ones JRG January 05

        x1Out(i)=x1
        lamOut(i,1)=lam
        lamOut(i,2)=lbinf
        lamOut(i,3)=lbsup
        suOut(i,1)=su
        suOut(i,2)=bsup
        suOut(i,3)=binf
    end do

! Death
    x1 = zidc(1)
    x2 = zidc(1)
    do i=1,mt2
!!!   Replaced by next sentences and add new ones JRG January 05
        if(i.ne.1)then
            x2 = x2 + hdc
        endif
        call cosp(x2,the2,nzdc+2,hes2,zidc,binf,su,bsup,lbinf,lam,lbsup)
        if(bsup.lt.0.d0)then
            bsup = 0.d0
        endif
        if(binf.gt.1.d0)then
            binf = 1.d0 
        endif
        if(lbinf.lt.0.d0)then
            lbinf = 0.d0
        endif 

        x2Out(i)=x2
        lam2Out(i,1)=lam
        lam2Out(i,2)=lbinf
        lam2Out(i,3)=lbsup
        su2Out(i,1)=su
        su2Out(i,2)=binf
        su2Out(i,3)=bsup 
    end do

! Meta
    x1 = zimeta(1)
    x2 = zimeta(1)
    do i=1,mt3
        if(i .ne.1)then
            x1 = x1 + hmeta 
        end if
        call cosp(x1,the3,nzmeta+2,hes3,zimeta,binf,su,bsup,lbinf,lam,lbsup)

        if(bsup.lt.0.d0)then
            bsup = 0.d0
        endif
        if(binf.gt.1.d0)then
            binf = 1.d0
        endif
        if(lbinf.lt.0.d0)then
            lbinf = 0.d0
        endif
!!!   Replaced by next sentences and add new ones JRG January 05

        x3Out(i)=x1
        lam3Out(i,1)=lam
        lam3Out(i,2)=lbinf
        lam3Out(i,3)=lbsup
        su3Out(i,1)=su
        su3Out(i,2)=bsup
        su3Out(i,3)=binf
    end do
    return

    end subroutine distanceJ_splines

