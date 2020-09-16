!
!   updateforecast.f90
!   
!
!   Created by Kurt Mitman on 24/01/15.
!   Copyright 2015 __MyCompanyName__. All rights reserved.
!
subroutine updateforecast

    use params
    use globals
    use funcs
    use procedures
    implicit none


    integer::counts(ngpAgg*ngpAgg)
    integer::kcounts(ngpAgg)
    integer::zindex,t,n_ols,k_ols,iAgg,iAggp
    double precision::R2
    double precision::meany
    double precision,allocatable::xt(:,:)
    double precision,allocatable::beta_ols(:),yyt(:),resid(:)

    double precision,dimension(Tsim-1)::Phvec,Phvecp

    n_ols=Tsim-101
    k_ols=ngpAgg*ngpAgg+ngpAgg
    allocate(Xt(n_ols,k_ols))
    allocate(yyt(n_ols))
    allocate(resid(n_ols))
    yyt=0.0d0
    Xt=0.0d0
    
    do t=100,Tsim-2
        iAgg=iAggpath(t)
        iAggp=iAggpath(t+1)
        zindex=(iAgg-1)*ngpAgg+iAggp
        Xt(t-99,zindex)=1.0d0
        Xt(t-99,ngpAgg*ngpAgg+iAgg)=log(Phpath(t))
        yyt(t-99)=log(Phpath(t+1))

    enddo

    aa0p=0.0d0
    aa1p=0.0d0

    allocate(beta_ols(k_ols))
    beta_ols=0.0d0
    call OLS(n_ols,k_ols,Xt,yyt,beta_ols)

    do iAgg=1,ngpAgg
       aa1p(iAgg,:)=beta_ols(ngpAgg*ngpAgg+iAgg)
       do iAggp=1,ngpAgg
            aa0p(iAgg,iAggp)=beta_ols((iAgg-1)*ngpAgg+iAggp)
        enddo
        
    enddo
    resid=yyt-matmul(Xt,beta_ols)
    meany=sum(yyt(1:n_ols))/real(n_ols)
    R2=1.0d0-sum( resid**2.0d0 )/sum( (yyt-meany)**2.0d0)
    print*,'R2= ',R2




end subroutine updateforecast
