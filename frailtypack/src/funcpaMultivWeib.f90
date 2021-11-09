! This file contains 

! funcpaMultivSplines (function): computes the log likelihood for a vector of
! parameters b given the data stored in the module "comonmultiv"


!========================  funcpaMultivWeib  ====================
! Log likelihood for one recurrent event and two terminal events.
    double precision function funcpaMultivWeib(b,np,id,thi,jd,thj,k0)

    use taillesmultiv
    use comonmultiv
    use residusMmultiv

    implicit none

! Variable Definitions
    integer,intent(in)::id,jd,np 
      ! id, jd = indexes at which to approximate first, second derivatives
      ! np = total number parameters, items in vectors b, bh
    double precision,dimension(np),intent(in)::b
      ! b = parameter vector
    double precision,dimension(4)::k0
        ! k0 = smoothing parameters
    double precision,intent(in)::thi,thj
        ! thi, thj = small differences for approximation of first, second derivatives
    integer::n,i,j,k,l,vj,ig,choix
    integer,dimension(ngmax)::cpt, deltaRes!, cptmeta
      ! cpt, cptmax = number of recurrent events for each person
    double precision::res,vet,vet2,vet3,vet4,frail,frail2,weight,weight2
        ! vet = temporary value for relative hazard for an individual
        ! frail = used in GH quadrature for integration
        ! weight = used in GH quadrature for integration
    double precision,dimension(np)::bh
        ! bh = local copy of parameter vector
    double precision,dimension(ng)::res2,res1dc,res2dc &
    ,res3dc,res2dc2,integrale1,integrale2,integrale3!,res2meta
    double precision::int
    double precision,parameter:: pi=3.141592653589793d0

    double precision::ss,ss1
    INTEGER::ii,jj,npg ! delete npg?
    !double precision,dimension(30)::x,w

    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivWeib.f90:: Setup."
    !close(1)

    !write(*,*) "Computing log likelihood..."
    kkapa=k0
    choix=0
    ig=0
    k=0
    vj=0
    n=0
    j=0
    l = 0

    do i=1,np
        bh(i)=b(i)
    end do

    if (id.ne.0) bh(id)=bh(id)+thi
    if (jd.ne.0) bh(jd)=bh(jd)+thj

    n = (np-nva-effet-indic_ALPHA)/nst ! delete this?

! One Random Effect Model
    if(typeJoint.eq.0)then
        ! b = c(betaR, etaR, betaD, etaD, betaD2, etaD2,theta,alpha1, alpha2, coef...)
        betaR= dexp(bh(1))**2.0d0 !shape
        etaR= dexp(bh(2))**2.0d0 !scale
        betaD= dexp(bh(3))**2.0d0
        etaD= dexp(bh(4))**2.0d0
        betaD2= dexp(bh(5))**2.0d0
        etaD2= dexp(bh(6))**2.0d0
        theta = dexp(bh(7))**2.0d0! log_e random effect sd ---> varaince
        alpha1=bh(8) ! random effect link to terminal event process 1
        alpha2=bh(9) ! random effect link to terminal event process 2
        rho = 0.d0
        theta2 = 1.d0
    endif
    if(typeJoint.eq.1)then
! Two Random Effect Model
        ! b = c(betaR, etaR, betaD, etaD, betaD2, etaD2, theta, theta2, rho, alpha1, alpha2, coef...)
        betaR= dexp(bh(1))**2.0d0 !shape
        etaR= dexp(bh(2))**2.0d0 !scale
        betaD= dexp(bh(3))**2.0d0
        etaD= dexp(bh(4))**2.0d0
        betaD2= dexp(bh(5))**2.0d0
        etaD2= dexp(bh(6))**2.0d0
        theta = dexp(bh(7))**2.0d0! log_e random effect sd ---> varaince
        theta2 = dexp(bh(8))**2.0d0! log_e random effect sd ---> varaince
        rho = (2.d0*dexp(bh(9))/(1.d0+dexp(bh(9)))) - 1.d0 ! transform correlation from unbounded to (-1,1) using inverse logit
        alpha1=bh(10) ! random effect link to terminal event process 1
        alpha2=bh(11) ! random effect link to terminal event process 2
    endif

    if (abs(rho).ge.1.d0) then
        open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
        write(1,*) "funcpaMultivWeib.f90:: Correlation out of bounds."
        close(1)
        funcpaMultivWeib =-1.d9
        do k=1,ng
            Rrec(k)=0.d0
            Nrec(k)=0
            Rdc(k)=0.d0
            Ndc(k)=0
            !Rrec2(k)=0.d0
            !Nrec2(k)=0
        end do
        goto 123
    endif

! Create empty vectors for likelihood components

    do k=1,ng
        res1(k) = 0.d0
        res2(k) = 0.d0
        res3(k) = 0.d0

        res1dc(k) = 0.d0
        res2dc(k) = 0.d0
        res3dc(k) = 0.d0

        res2dc2(k) = 0.d0

        integrale3(k) = 1.d0
        aux1(k)=0.d0
        aux2(k)=0.d0
    end do

! Unpack model parameters
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivWeib.f90:: Function Arguments..."
    !write(1,*) "funcpaMultivWeib.f90:: b=", b
    !write(1,*) "funcpaMultivWeib.f90:: bh=", bh
    !write(1,*) "funcpaMultivWeib.f90:: np=", np
    !write(1,*) "funcpaMultivWeib.f90:: id=", id
    !write(1,*) "funcpaMultivWeib.f90:: thi=", thi
    !write(1,*) "funcpaMultivWeib.f90:: jd=", jd
    !write(1,*) "funcpaMultivWeib.f90:: thj=", thj
    !write(1,*) "funcpaMultivWeib.f90:: k0=", k0
    !write(1,*) "funcpaMultivWeib.f90:: res=", res
    !write(1,*) "funcpaMultivWeib.f90:: typeJoint=", typeJoint
    !write(1,*) "funcpaMultivWeib.f90:: funcpaMultivWeib=", funcpaMultivWeib
    !write(1,*) "funcpaMultivWeib.f90:: betaR, etaR, betaD, etaD, betaD2, etaD2=", betaR, etaR, betaD, etaD, betaD2, etaD2
    !write(1,*) "funcpaMultivWeib.f90:: theta, theta2, alpha1, alpha2, rho =", theta, theta2, alpha1, alpha2, rho
    !write(1,*) "funcpaMultivWeib.f90:: res1(1:3)=", res1(1:3)
    !write(1,*) "funcpaMultivWeib.f90:: res2(1:3)=", res2(1:3)
    !write(1,*) "funcpaMultivWeib.f90:: res3(1:3)=", res3(1:3)
    !write(1,*) "funcpaMultivWeib.f90:: nig=", nig
    !write(1,*) "funcpaMultivWeib.f90:: res1dc(1:3)=", res1dc(1:3)
    !write(1,*) "funcpaMultivWeib.f90:: res2dc(1:3)=", res2dc(1:3)
    !write(1,*) "funcpaMultivWeib.f90:: res3dc(1:3)=", res3dc(1:3)
    !write(1,*) "funcpaMultivWeib.f90:: res2dc2(1:10)=", res2dc2(1:10)
    !write(1,*) "funcpaMultivWeib.f90:: integrale3(1:10)=", integrale3(1:10)
    !write(1,*) "funcpaMultivWeib.f90:: aux1(1:10)=", aux1(1:10)
    !write(1,*) "funcpaMultivWeib.f90:: aux2(1:10)=", aux2(1)
    !write(1,*) "funcpaMultivWeib.f90:: Rrec=", Rrec
    !write(1,*) "funcpaMultivWeib.f90:: Nrec=", Nrec
    !write(1,*) "funcpaMultivWeib.f90:: Rdc=", Rdc
    !write(1,*) "funcpaMultivWeib.f90:: Ndc=", Ndc
    !write(1,*) "funcpaMultivWeib.f90:: Rrec2=", Rrec2
    !write(1,*) "funcpaMultivWeib.f90:: Nrec2=", Nrec2
    !close(1)
!==========================================================================
! Likelihood Contribution for Recurrent Event 1
!==========================================================================
    cpt = 0

    do i=1,nsujet
        ! (1) count the number of events per individual
        cpt(g(i))=cpt(g(i))+c(i)

        ! (2) compute the relative hazard for each event
        if(nva1.gt.0)then
            vet = 0.d0
            do j=1,nva1
                vet = vet + bh(np-nva1-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)+j)*dble(ve(i,j))
            end do
            vet = dexp(vet)
        else
            vet=1.d0
        endif

        ! (3a) Compute hazard for non-censored events
        if((c(i).eq.1))then
            res2(g(i)) = res2(g(i))+(betaR-1.d0)*dlog(t1(i))+&
            dlog(betaR)-betaR*dlog(etaR)+dlog(vet)
        endif

        if ((res2(g(i)).ne.res2(g(i))).or.(abs(res2(g(i))).ge.1.d30)) then
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivWeib.f90:: Error with hazard for recurrent event 1."
            write(1,*) "funcpaMultivWeib.f90:: betaR, etaR=", betaR, etaR
            write(1,*) "funcpaMultivWeib.f90:: i=",i
            write(1,*) "funcpaMultivWeib.f90:: g(i)=",g(i)
            write(1,*) "funcpaMultivWeib.f90:: vet=", vet
            write(1,*) "funcpaMultivWeib.f90:: t1(i)=", t1(i)
            write(1,*) "funcpaMultivWeib.f90:: res2(g(i))=",res2(g(i))
            write(1,*) "res2(g(i)) = res2(g(i))+(betaR-1.d0)*dlog(t1(i))+"
            write(1,*) "dlog(betaR)-betaR*dlog(etaR)+dlog(vet)"
            close(1)
            funcpaMultivWeib=-1.d9
            goto 123
        end if

        ! (3b) Compute cumulative hazard for all events (censored and non-censored)
        ! this part goes into the integral
        res1(g(i)) = res1(g(i))+((t1(i)/etaR)**betaR)*vet
         if ((res1(g(i)).ne.res1(g(i))).or.(abs(res1(g(i))).ge. 1.d30)) then
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivWeib.f90:: Error with cumulative hazard for recurrent event 1."
            write(1,*) "funcpaMultivWeib.f90:: betaR, etaR=", betaR, etaR
            write(1,*) "funcpaMultivWeib.f90:: i=",i
            write(1,*) "funcpaMultivWeib.f90:: g(i)=",g(i)
            write(1,*) "funcpaMultivWeib.f90:: vet=", vet
            write(1,*) "funcpaMultivWeib.f90:: t1(i)=", t1(i)
            write(1,*) "funcpaMultivWeib.f90:: res1(g(i))=",res1(g(i))
            write(1,*) "res1(g(i)) = res1(g(i))+((t1(i)/etaR)**betaR)*vet"
            close(1)
            funcpaMultivWeib=-1.d9
            goto 123
        end if

        ! (3c) Modification for cumulative hazard with left truncation (for calendar time model 
        ! as opposed to gap time model)
        res3(g(i)) = res3(g(i))+((t0(i)/etaR)**betaR)*vet
        if ((res3(g(i)).ne.res3(g(i))).or.(abs(res3(g(i))).ge. 1.d30)) then
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivWeib.f90:: Error with truncation adjustment for recurrent event 1."
            write(1,*) "funcpaMultivWeib.f90:: betaR, etaR=", betaR, etaR
            write(1,*) "funcpaMultivWeib.f90:: i=",i
            write(1,*) "funcpaMultivWeib.f90:: g(i)=",g(i)
            write(1,*) "funcpaMultivWeib.f90:: vet=", vet
            write(1,*) "funcpaMultivWeib.f90:: t1(i)=", t0(i)
            write(1,*) "funcpaMultivWeib.f90:: res3(g(i))=",res3(g(i))
            close(1)
            funcpaMultivWeib=-1.d9
            goto 123
        end if
    end do

!==========================================================================
! Likelihood Contribution for Terminal Event 1
!==========================================================================

    do k=1,ng
        if(nva2.gt.0)then
            vet2 = 0.d0
            do j=1,nva2
                vet2 =vet2 + bh(np-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)+j)*dble(vedc(k,j))
            end do
            vet2 = dexp(vet2)
        else
            vet2=1.d0
        endif
        if(cdc(k).eq.1)then
            res2dc(k) = (betaD-1.d0)*dlog(t1dc(k))+dlog(betaD)-betaD*dlog(etaD)+dlog(vet2)
            if ((res2dc(k).ne.res2dc(k)).or.(abs(res2dc(k)).ge.1.d30)) then
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivWwib.f90:: Error with hazard for terminal event 1."
                write(1,*) "funcpaMultivWeib.f90:: betaD, etaD=", betaD, etaD
                write(1,*) "funcpaMultivWeib.f90:: t1dc(k)=", t1dc(k)
                write(1,*) "funcpaMultivWeib.f90:: res2dc(k)=", res2dc(k)
                write(1,*) "funcpaMultivWeib.f90:: vet2=", vet2
                close(1)
                funcpaMultivWeib=-1.d9
                goto 123
            end if
        endif

! cumulative hazard, goes into the integral
        aux1(k)=((t1dc(k)/etaD)**betaD)*vet2
        if ((aux1(k).ne.aux1(k)).or.(abs(aux1(k)).ge. 1.d30)) then
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivWwib.f90:: Error with cumulative hazard for terminal event 1."
            write(1,*) "funcpaMultivWeib.f90:: betaD, etaD=", betaD, etaD
            write(1,*) "funcpaMultivWeib.f90:: np , nva2, nva3, nva4 =", np , nva2, nva3, nva4
            write(1,*) "funcpaMultivWeib.f90:: bh(np-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)+1) =", &
            bh(np-nva2-(nva3*event2_ind)-(nva4*terminal2_ind)+1)
            write(1,*) "funcpaMultivWeib.f90:: k=", k
            write(1,*) "funcpaMultivWeib.f90:: vedc(k,1:nva2)=", vedc(k,1:nva2)
            write(1,*) "funcpaMultivWeib.f90:: vet2=", vet2
            write(1,*) "funcpaMultivWeib.f90:: aux1(k)=", aux1(k)
            close(1)
            funcpaMultivWeib=-1.d9
            goto 123
        end if
    end do

!==========================================================================
! Likelihood Contribution for Terminal Event 2
!==========================================================================

    do k=1,ng
        if(nva4.gt.0)then
            vet4 = 0.d0
            do j=1,nva4
                vet4 =vet4 + bh(np-(nva4*terminal2_ind)+j)*dble(vedc2(k,j))
            end do
            vet4 = dexp(vet4)
        else
            vet4=1.d0
        endif
        if(cdc2(k).eq.1)then
            res2dc2(k) = (betaD2-1.d0)*dlog(t1dc(k))+dlog(betaD2)-betaD2*dlog(etaD2)+dlog(vet4)
            if ((res2dc2(k).ne.res2dc2(k)).or.(abs(res2dc2(k)).ge. 1.d30)) then
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivWwib.f90:: Error with hazard for terminal event 2."
                close(1)
                funcpaMultivWeib=-1.d9
                goto 123
            end if
        endif

! cumulative hazard, goes into the integral
        aux2(k)=((t1dc(k)/etaD2)**betaD2)*vet4
        if ((aux2(k).ne.aux2(k)).or.(abs(aux2(k)).ge. 1.d30)) then
            open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
            write(1,*) "funcpaMultivWwib.f90:: Error with cumulative hazard for terminal event 2."
            close(1)
            funcpaMultivWeib=-1.d9
            goto 123
        end if
    end do


!==========================================================================
! Likelihood Contribution for Recurrent Event 2
!==========================================================================

!    do i=1,nsujetmeta
!        cptmeta(gmeta(i))=cptmeta(gmeta(i))+cmeta(i)
!        if(nva3.gt.0)then
!            vet3 = 0.d0
!            do j=1,nva3
!                vet3 =vet3 + bh(np-nva3+j)*dble(vemeta(i,j))
!            end do
!            vet3 = dexp(vet3)
!        else
!            vet3=1.d0
!        endif
!
!        if((cmeta(i).eq.1))then
!            res2meta(gmeta(i)) = res2meta(gmeta(i))+(betaM-1.d0)*dlog(t1meta(i))+dlog(betaM)-betaM*dlog(etaM)+dlog(vet3)
!            if ((res2meta(gmeta(i)).ne.res2meta(gmeta(i))).or.(abs(res2meta(gmeta(i))).ge. 1.d30)) then
!                funcpaMultivWeib=-1.d9
!                goto 123
!            end if
!        endif

!     nouvelle version
!        res1meta(gmeta(i)) = res1meta(gmeta(i))+((t1meta(i)/etaM)**betaM)*vet3
!         if ((res1meta(gmeta(i)).ne.res1meta(gmeta(i))).or.(abs(res1meta(gmeta(i))).ge. 1.d30)) then
!            funcpaMultivWeib=-1.d9
!            goto 123
!        end if

!     modification pour nouvelle vraisemblance / troncature:
!        res3meta(gmeta(i)) = res3meta(gmeta(i))+((t0meta(i)/etaM)**betaM)*vet3
!        if ((res3meta(gmeta(i)).ne.res3meta(gmeta(i))).or.(abs(res3meta(gmeta(i))).ge. 1.d30)) then
!            funcpaMultivWeib=-1.d9
!            goto 123
!        end if

!    end do


!==========================================================================
! Integrals
!==========================================================================
! compute integral for each person
! the integral component will depend on the parameterization of the random effects
! typeJoint = 0 for 1 random effect
! typeJoint = 1 for 2 correlated random effects

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
                funcpaMultivWeib=-1.d9
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
                    - (((frail**2.d0)/theta) + ((frail2**2.d0)/theta2) - ((2.0d0*frail*frail2*rho)/dsqrt(theta)/dsqrt(theta2)))&
                    /(2.d0*(1.d0-(rho**2.d0)))))! frailty distribution (multivariate normal)
                end do
            end do
            if((ss.eq.0.0d0)) ss = 1.0d-10 ! integral should not be too small when theta is large
            integrale3(k) = ss
            if ((integrale3(k).ne.integrale3(k)).or.(abs(integrale3(k)).ge.1.d30)) then
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivWwib.f90:: Error with integral component of likelihood."
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
                funcpaMultivWeib=-1.d9
                goto 123
            end if
        end do
        !open(1, file = '../package_tests/multiv_model_integrals.dat',position="append")  
        !write(1,*) integrale3
        !close(1)
    end if

!==========================================================================
! Combine likelihood
!==========================================================================
    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivWeib.f90:: Combine Likelihood."
    !write(1,*) "funcpaMultivWeib.f90:: (end of file) res = ",res
    !write(1,*) "funcpaMultivWeib.f90:: (end of file) res2(1:10) = ",res2(1:10)
    !write(1,*) "funcpaMultivWeib.f90:: (end of file) res2dc(1:10) = ",res2dc(1:10)
    !write(1,*) "funcpaMultivWeib.f90:: (end of file) res2dc2(1:10) = ",res2dc2(1:10)
    !write(1,*) "funcpaMultivWeib.f90:: (end of file) integrale3(1:10) = ",integrale3(1:10)
    !close(1)

    res = 0.d0

    if(typeJoint.eq.0)then
        do k=1,ng
            res = res + res2(k)+res2dc(k)+res2dc2(k)+dlog(integrale3(k))- &
            dlog(2.d0*pi)/2.d0 - dlog(dsqrt(theta))
            
            if ((res.ne.res).or.(abs(res).ge. 1.d30)) then
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivWeib.f90:: Error with sum of individual log likelihoods."
                write(1,*) "typeJoint = ", typeJoint
                write(1,*) "theta = ", theta
                write(1,*) "k = ", k
                write(1,*) "res2(k) = ", res2(k)
                write(1,*) "res2dc(k) = ", res2dc(k)
                write(1,*) "res2dc2(k) = ", res2dc2(k)
                write(1,*) "integrale3(k) = ", integrale3(k)
                write(1,*) "res(k)= ", &
                            res2(k)+res2dc(k)+res2dc2(k)+dlog(integrale3(k))- &
                                        dlog(2.d0*pi)/2.d0 - dlog(dsqrt(theta))
                write(1,*) "res = ", res
                write(1,*) "funcpaMultivWeib = ", funcpaMultivWeib
                close(1)
                funcpaMultivWeib =-1.d9
                Rrec(k)=0.d0
                Nrec(k)=0
                Rdc(k)=0.d0
                Ndc(k)=0
                !Rrec2(k)=0.d0
                !Nrec2(k)=0
                goto 123
            else
                funcpaMultivWeib = res
                Rrec(k)=res1(k)
                Nrec(k)=nig(k)
                Rdc(k)=aux1(k)
                Ndc(k)=cdc(k)
                !Rrec2(k)=res1meta(k)
                !Nrec2(k)=nigmeta(k)
            end if
            
        end do
    end if

    if(typeJoint.eq.1)then
        do k=1,ng
            res = res + res2(k)+res2dc(k)+res2dc2(k)+dlog(integrale3(k))- &
            dlog(dsqrt(theta)) - dlog(dsqrt(theta2)) - dlog(dsqrt(1.0d0-(rho**2.0d0))) - dlog(2.d0*pi)

            if ((res.ne.res).or.(abs(res).ge. 1.d30)) then
                open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
                write(1,*) "funcpaMultivWeib.f90:: Error with sum of individual log likelihoods."
                write(1,*) "typeJoint = ", typeJoint
                write(1,*) "theta = ", theta
                write(1,*) "theta = ", theta
                write(1,*) "theta2 = ", theta2
                write(1,*) "rho = ", rho
                write(1,*) "k = ", k
                write(1,*) "res2(k) = ", res2(k)
                write(1,*) "res2dc(k) = ", res2dc(k)
                write(1,*) "res2dc2(k) = ", res2dc2(k)
                write(1,*) "integrale3(k) = ", integrale3(k)
                write(1,*) "res(k)= ", &
                            res2(k)+res2dc(k)+res2dc2(k)+dlog(integrale3(k))- &
                                        dlog(2.d0*pi)/2.d0 - dlog(dsqrt(theta))
                write(1,*) "res = ", res
                write(1,*) "funcpaMultivWeib = ", funcpaMultivWeib
                close(1)
                funcpaMultivWeib =-1.d9
                Rrec(k)=0.d0
                Nrec(k)=0
                Rdc(k)=0.d0
                Ndc(k)=0
                !Rrec2(k)=0.d0
                !Nrec2(k)=0
                goto 123
            else
                funcpaMultivWeib = res
                Rrec(k)=res1(k)
                Nrec(k)=nig(k)
                Rdc(k)=aux1(k)
                Ndc(k)=cdc(k)
                !Rrec2(k)=res1meta(k)
                !Nrec2(k)=nigmeta(k)
            end if
        end do
    end if

123     continue

    !open(1, file = '../package_tests/multiv_model_progress.dat',position="append")  
    !write(1,*) "funcpaMultivWeib.f90:: End of funcpamultivWeib."
    !write(1,*) "funcpaMultivWeib.f90:: res = ", res
    !write(1,*) "funcpaMultivWeib.f90:: funcpaMultivWeib = ", funcpaMultivWeib
    !close(1)

    return

    end function funcpaMultivWeib

!=================================================================================================
