PROGRAM CAIXA_SIMULACAO
    IMPLICIT NONE 

    INTEGER, PARAMETER :: N = 200000                             	   ! Número de passos
    INTEGER, PARAMETER :: NP = 1                                       ! Número de partículas
    INTEGER, PARAMETER :: N_PIN_ATRAT = 84 
    INTEGER, PARAMETER :: N_PIN_REP = 84 
    INTEGER :: J, P, K, IDUM, N_WRITE, I, U, L, S, D, STATUS
    INTEGER, PARAMETER :: WRITE_CUT = 5                                ! Corte para escrita
    INTEGER, PARAMETER :: N_BETA = 1
    INTEGER, PARAMETER :: N_F_0_ATRAT = 10
    INTEGER, PARAMETER :: N_F_0_REP = 10
    INTEGER, PARAMETER :: N_A_0_ATRAT = 1
    INTEGER, PARAMETER :: N_A_0_REP = 1
    INTEGER, PARAMETER :: N_VALUES = 4                                 ! Número de valores desejados de corrente para salvar
    
!-------------------------------------------------------------------------------  

    DOUBLE PRECISION, PARAMETER :: DT = 0.001D0                       ! Intervalo de integração
    DOUBLE PRECISION :: Y_OLD(NP), T, Y_NEW(NP), Y_HISTORIC(N/WRITE_CUT,NP)
    DOUBLE PRECISION :: T_HISTORIC(N/WRITE_CUT), LX, LY, X_HISTORIC(N/WRITE_CUT,NP)
    DOUBLE PRECISION :: X_OLD(NP), X_NEW(NP), FX_CURRENT, FY_CURRENT
    DOUBLE PRECISION :: RAN2, ALPHA_M, ALPHA_D
    DOUBLE PRECISION :: X_PIN_ATRAT(N_PIN_ATRAT), Y_PIN_ATRAT(N_PIN_ATRAT),X_PIN_REP(N_PIN_REP), Y_PIN_REP(N_PIN_REP)
    DOUBLE PRECISION :: VX_MEDIA(NP), VY_MEDIA(NP), VX_INST(NP), VY_INST(NP),N_STEPS_CURRENT, R(NP), W(NP)
	DOUBLE PRECISION :: F_ATRAT, F_REP, A_O_ATRAT, A_O_REP
    DOUBLE PRECISION :: VALUES_TO_SAVE(N_VALUES)
    DOUBLE PRECISION, PARAMETER :: PI = ACOS(-1.0D0)
    
!-------------------------------------------------------------------------------   

    DOUBLE PRECISION, DIMENSION(N_BETA) :: BETA    					 !1.D0/DSQRT(2.D0) 
	DOUBLE PRECISION, DIMENSION(N_F_0_ATRAT) :: F_O_ATRAT
	DOUBLE PRECISION, DIMENSION(N_F_0_REP) :: F_O_REP
	DOUBLE PRECISION, DIMENSION(N_A_0_ATRAT) :: A_ATRAT
	DOUBLE PRECISION, DIMENSION(N_A_0_REP) :: A_REP
	
!-------------------------------------------------------------------------------

	CHARACTER(LEN=200) :: FILENAME1, FILENAME2,FX_STR, B_STR, F_ATRAT_STR, F_REP_STR, A_O_ATRAT_STR, A_O_REP_STR
	
!-------------------------------------------------------------------------------

    18  FORMAT(2000D17.10)
     
    ! Parâmetros da força de pinning									!Força atrat (-), Força rep (+)
	F_O_ATRAT =   [( -I * 0.1D0, I = 1, N_F_0_ATRAT )]                                              ![( -I * 0.1D0, I = 1, N_F_0_ATRAT )]	 
	F_O_REP   =   [( I * 0.1D0 , I = 1, N_F_0_REP)]                                                 ![(  I * 0.1D0, I = 1, N_F_0_REP )]
	
	
	! Raio do pinning (ajustável)
	A_ATRAT = [( I * 0.65D0, I = 1, N_A_0_ATRAT)]      !0.65D0
    A_REP   = [( I * 0.65D0, I = 1, N_A_0_REP)]        !0.65D0

    ! Tamanho da Caixa
    LX = 32.7273D0  						!32.7273                    
    LY = 32.7273D0  					   ! Caixa hexagonal = (DSQRT(3.0D0)/2.0D0) * LX	

   PRINT *, "LX =", LX
   PRINT *, "LY =", LY
   PRINT *, "PI =", PI
   PRINT *, "----------------------------------"
  
  ! Defina os valores específicos de corrente em que deseja salvar
    DATA VALUES_TO_SAVE /0.25D0, 0.5D0, 0.70D0, 1.6D0/

   
   ! Definição dos valores de beta
	BETA = [SQRT(3.0D0)]				!BETA = [0.0D0, 0.176327D0, 0.36397D0, SQRT(3.0D0)/3.0D0, &
											!0.8391D0, 1.19175D0, SQRT(3.0D0), 2.74748D0, 5.67128D0]
            
!-------------------------------------------------------------------------------  
! Leitura da rede quadrada atrativos

    OPEN(20, FILE="rede_atrativa.dat", STATUS="OLD")

DO K=1,N_PIN_ATRAT 
        READ(20,*) X_PIN_ATRAT(K),Y_PIN_ATRAT(K)
END DO 

!-------------------------------------------------------------------------------  
 !Leitura da rede quadrada repulsivo
 
 OPEN(21, FILE="rede_repulsiva.dat", STATUS="OLD")

DO K = 1, N_PIN_REP
    READ(21,*) X_PIN_REP(K), Y_PIN_REP(K)
END DO
!-------------------------------------------------------------------------------
! Loop sobre raios atrat e reps
    DO S = 1, N_A_0_ATRAT  
        DO D =1, N_A_0_REP
            A_O_ATRAT = A_ATRAT(S)
            A_O_REP = A_REP(D)
            
        WRITE(*,'(A,F7.2)') "A_O_ATRAT = ", A_O_ATRAT
	 
	    WRITE(*,'(A,F7.2)') "A_O_REP   = ", A_O_REP

! Convertendo A_O_ATRAT para string
    WRITE(A_O_ATRAT_STR, '(F12.4)') A_O_ATRAT
    A_O_ATRAT_STR = TRIM(ADJUSTL(A_O_ATRAT_STR))

! Convertendo A_O_REP para string
    WRITE(A_O_REP_STR, '(F12.4)')  A_O_REP
    A_O_REP_STR = TRIM(ADJUSTL(A_O_REP_STR))
    
!-------------------------------------------------------------------------------
    ! Loop sobre forças atrat e reps
    DO U = 1, N_F_0_ATRAT  
        DO L =1, N_F_0_REP
            F_ATRAT = F_O_ATRAT(U)
            F_REP   = F_O_REP(L)

     WRITE(*,'(A,F7.2)') "F_ATRAT = ", F_ATRAT
	 
	 WRITE(*,'(A,F7.2)') "F_REP   = ", F_REP

! Convertendo F_ATRAT para string
    WRITE(F_ATRAT_STR, '(F12.4)') F_ATRAT
    F_ATRAT_STR = TRIM(ADJUSTL(F_ATRAT_STR))

    ! Convertendo F_REP para string
    WRITE(F_REP_STR, '(F12.4)') F_REP
    F_REP_STR = TRIM(ADJUSTL(F_REP_STR))

!-------------------------------------------------------------------------------

    ! Loop sobre os valores de BETA
    DO I = 1, N_BETA    
	
  ! Definição de ALPHA_M e ALPHA_D para o beta atual
  ! CONDICAO DE QUE: ALPHA_M **2 + ALPHA_D **2 = 1
  
        ALPHA_D = DSQRT(1.D0/(BETA(I)**2 + 1.D0))
        ALPHA_M = BETA(I) * ALPHA_D 

        PRINT *, "Rodando para BETA =", BETA(I)
        PRINT *, "ALPHA_D =", ALPHA_D
        PRINT *, "ALPHA_M =", ALPHA_M 
       PRINT *, "---------------------------------------"
       
       ! Criando string para nome do arquivo
        WRITE(B_STR, '(F12.4)') BETA(I)        
        B_STR = TRIM(ADJUSTL(B_STR))
    
       ! Nome do arquivo de saída para cada beta
        WRITE(FILENAME2, '("velocidade_global_beta_", A, "_F_ATRAT_", A, "_F_REP_", A, ".dat")') &
        TRIM(B_STR), TRIM(F_ATRAT_STR), TRIM(F_REP_STR)
        
        OPEN(30, FILE=TRIM(FILENAME2), STATUS="UNKNOWN")
             WRITE(30, *)  " FX_CURRENT      /          VX_MEDIA_GLOBAL /    VY_MEDIA_GLOBAL/"& 
             "  R     /       A_O_ATRAT   /  A_O_REP    "
       
!-------------------------------------------------------------------------------
    ! Inicializador
    
        FX_CURRENT = 0.0D0
        FY_CURRENT = 0.D0
		VX_MEDIA = 0.D0
		VY_MEDIA = 0.D0
		N_STEPS_CURRENT = 0.001D0
		
    T = 0.0D0                                             ! Tempo inicial
    IDUM = -8560 
    DO K = 1, NP
    
        !Y_OLD(K) = RAN2(IDUM) * LY                	 ! Gera uma coordenada y aleatória dentro da caixa de tamanho LY
        !X_OLD(K) = RAN2(IDUM) * LX                  ! Gera uma coordenada x aleatória dentro da caixa de tamanho LX
            
        Y_OLD(K) = 18.D0                           
        X_OLD(K) = 18.D0
        
    END DO
                                     
    N_WRITE = N / WRITE_CUT                               ! Calcula o número de passos de escrita
    
!-------------------------------------------------------------------------------
! loop corrente
        
        DO WHILE (FX_CURRENT <= 3.D0 )                !Loop corrente até a condição ser verdadeira, ou seja até força da corrente ser 3

    WRITE(FX_STR, '(F12.4)') FX_CURRENT                 !Criando string para criar arquivo
    FX_STR = ADJUSTL(FX_STR)
    
     !WRITE(FILENAME1, '("arquivo_velocidade_por_particula_corrente_", A, "_F_ATRAT_", A, "_F_REP_", A, ".dat")') &
     !TRIM(FX_STR), TRIM(F_ATRAT_STR), TRIM(F_REP_STR)
    !OPEN(31, FILE=FILENAME1, STATUS="UNKNOWN")
       !WRITE(31, *) "   T      /       VX      /             VY  "
        
!-------------------------------------------------------------------------------
    ! Loop principal de integração
    
    DO J = 1, N
    
        CALL RungeKutta2_Method(Y_OLD, Y_NEW, T, DT, X_OLD, X_NEW, NP, ALPHA_M, ALPHA_D, LX, LY,& ! Chama o método de Euler/ RK2
        FX_CURRENT,FY_CURRENT,N_PIN_ATRAT,N_PIN_REP, X_PIN_ATRAT, Y_PIN_ATRAT,X_PIN_REP,Y_PIN_REP,F_ATRAT,F_REP,A_O_ATRAT,A_O_REP)     
 
        !Calculo das velocidades
       DO K = 1, NP
            VX_INST(K) = (X_NEW(K) - X_OLD(K)) / DT
            VY_INST(K) = (Y_NEW(K) - Y_OLD(K)) / DT
        END DO

        ! Ajusta as coordenadas X e Y para que fiquem dentro dos limites da caixa
        DO K = 1, NP
            IF (X_NEW(K) .GT. LX) THEN
                X_NEW(K) = X_NEW(K) - LX
            ELSE IF (X_NEW(K) .LT. 0.D0) THEN
                X_NEW(K) = X_NEW(K) + LX
            END IF

            IF (Y_NEW(K) .GT. LY) THEN
                Y_NEW(K) = Y_NEW(K) - LY
            ELSE IF (Y_NEW(K) .LT. 0.D0) THEN
                Y_NEW(K) = Y_NEW(K) + LY
            END IF
        END DO

!-------------------------------------------------------------------------------

	! Acumula as velocidades para cada partícula
	
       VX_MEDIA = VX_MEDIA + VX_INST
       VY_MEDIA = VY_MEDIA + VY_INST
       
	!PRINT *, VX_MEDIA, VY_MEDIA
!-------------------------------------------------------------------------------
    !Atualiza os valores

        Y_OLD = Y_NEW                                               ! Atualiza Y_OLD para o próximo passo do loop
        X_OLD = X_NEW                                               ! Atualiza X_OLD para o próximo passo do loop
        T = T + DT                                                  ! Atualiza o tempo
        
!-------------------------------------------------------------------------------

        ! Armazena os resultados no histórico
        IF (MOD(J, WRITE_CUT) == 0) THEN
            Y_HISTORIC(J / WRITE_CUT, :) = Y_OLD(:)     
            X_HISTORIC(J / WRITE_CUT, :) = X_OLD(:)
           !T_HISTORIC(J / WRITE_CUT) = T          
        END IF
        
!-------------------------------------------------------------------------------

    ! Salvando velocidade de cada partícula em cada passo
        !DO K = 1, NP
            !WRITE(31,'(F12.4, 2F20.15)') T, VX_INST(K), VY_INST(K)
        !END DO
    
    END DO ! Fecha o loop de integração
    
        VX_MEDIA = VX_MEDIA / N
        VY_MEDIA = VY_MEDIA / N
    
		W = ATAN(VY_MEDIA/VX_MEDIA)		
		R = (180.0D0 * W) / PI							! Ângulo Theta(sk)
									
    ! Salvando velocidade média
            WRITE(30,'(6F25.15)') FX_CURRENT, VX_MEDIA, VY_MEDIA, R, A_O_ATRAT, A_O_REP
            
!-------------------------------------------------------------------------------

    ! Verifica se o valor de FX_CURRENT está no vetor de valores desejados
   ! DO K = 1, N_VALUES
       ! IF (DABS(FX_CURRENT - VALUES_TO_SAVE(K)) .LT. 1.0D-6) THEN  
             !Chama a subrotina de histórico e escrita dos arquivos out.dat
          !  CALL Save_Historic(N_WRITE, NP, DT, Y_HISTORIC, T_HISTORIC, X_HISTORIC, FX_CURRENT, FX_STR, A_O_ATRAT_STR, A_O_REP_STR,&
			!F_ATRAT_STR, F_REP_STR)
           ! EXIT  ! Sai do loop para evitar verificações desnecessárias
       ! END IF
    !END DO
        
            FX_CURRENT= FX_CURRENT +  N_STEPS_CURRENT      ! Inclemento manual
         
        END DO      !Loop corrente
        
    END DO          !Loop Beta
    
  END DO    !loop rep
 
END DO      !Loop atrat

END DO      !Loop raio atrat pinning

END DO      !Loop raio rep pinning

END PROGRAM CAIXA_SIMULACAO
!-------------------------------------------------------------------------------

! Subrotina para executar o método de Euler
!SUBROUTINE Euler_Method(Y_OLD, Y_NEW, T, DT, X_OLD, X_NEW, NP, ALPHA_M, ALPHA_D,&
!LX, LY, FX_CURRENT, FY_CURRENT, N_PIN_ATRAT,N_PIN_REP, X_PIN_ATRAT, Y_PIN_ATRAT,X_PIN_REP,Y_PIN_REP,VX,VY) 
    !IMPLICIT NONE
    !DOUBLE PRECISION, INTENT(IN) :: T, DT, LX, LY
    !INTEGER, INTENT(IN) :: NP, N_PIN_ATRAT,N_PIN_REP
    !DOUBLE PRECISION, INTENT(IN) :: X_OLD(NP), Y_OLD(NP), ALPHA_M, ALPHA_D
    !DOUBLE PRECISION, INTENT(IN) :: X_PIN_ATRAT(N_PIN_ATRAT), Y_PIN_ATRAT(N_PIN_ATRAT),X_PIN_REP(N_PIN_REP), Y_PIN_REP(N_PIN_REP)  
    !DOUBLE PRECISION, INTENT(OUT) :: X_NEW(NP), Y_NEW(NP)
    !DOUBLE PRECISION :: VX(NP), VY(NP), FX_CURRENT, FY_CURRENT
    

    !CALL Funcao(T, X_OLD, Y_OLD, VX, VY, NP, ALPHA_M, ALPHA_D, LX, LY,FX_CURRENT,FY_CURRENT,&     ! Calcula as velocidades
    !N_PIN_ATRAT,N_PIN_REP, X_PIN_ATRAT, Y_PIN_ATRAT,X_PIN_REP,Y_PIN_REP)                
    
    !X_NEW = X_OLD + DT * VX                                 ! Atualiza as posições
    !Y_NEW = Y_OLD + DT * VY
    
!END SUBROUTINE Euler_Method

!-------------------------------------------------------------------------------

! Subrotina para executar o método de Runge-Kutta de segunda ordem
SUBROUTINE RungeKutta2_Method(Y_OLD, Y_NEW, T, DT, X_OLD, X_NEW, NP, ALPHA_M, ALPHA_D, &
LX, LY, FX_CURRENT, FY_CURRENT, N_PIN_ATRAT, N_PIN_REP, X_PIN_ATRAT, Y_PIN_ATRAT, &
X_PIN_REP, Y_PIN_REP,F_ATRAT,F_REP,A_O_ATRAT,A_O_REP)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: T, DT, LX, LY
    INTEGER, INTENT(IN) :: NP, N_PIN_ATRAT, N_PIN_REP
    DOUBLE PRECISION, INTENT(IN) :: X_OLD(NP), Y_OLD(NP), ALPHA_M, ALPHA_D
    DOUBLE PRECISION, INTENT(IN) :: X_PIN_ATRAT(N_PIN_ATRAT), Y_PIN_ATRAT(N_PIN_ATRAT)
    DOUBLE PRECISION, INTENT(IN) :: X_PIN_REP(N_PIN_REP), Y_PIN_REP(N_PIN_REP)
    DOUBLE PRECISION, INTENT(OUT) :: X_NEW(NP), Y_NEW(NP)
    DOUBLE PRECISION :: VX(NP), VY(NP), VX_TEMP(NP), VY_TEMP(NP),FX_CURRENT, FY_CURRENT
    DOUBLE PRECISION :: X_TEMP(NP), Y_TEMP(NP), F_ATRAT,F_REP,A_O_ATRAT,A_O_REP

    ! Primeiro passo (k1): Avalia no ponto inicial
    CALL Funcao(T, X_OLD, Y_OLD, VX, VY, NP, ALPHA_M, ALPHA_D, LX, LY, FX_CURRENT, FY_CURRENT, &
    N_PIN_ATRAT, N_PIN_REP, X_PIN_ATRAT, Y_PIN_ATRAT, X_PIN_REP, Y_PIN_REP,F_ATRAT,F_REP,A_O_ATRAT,A_O_REP)

    ! Calcula as posições intermediárias
    X_TEMP = X_OLD + 0.5D0 * DT * VX
    Y_TEMP = Y_OLD + 0.5D0 * DT * VY

    ! Segundo passo (k2): Avalia no ponto intermediário
    CALL Funcao(T + 0.5D0 * DT, X_TEMP, Y_TEMP, VX_TEMP, VY_TEMP, NP, ALPHA_M, ALPHA_D, &
    LX, LY, FX_CURRENT, FY_CURRENT, N_PIN_ATRAT, N_PIN_REP, X_PIN_ATRAT, Y_PIN_ATRAT, &
    X_PIN_REP, Y_PIN_REP,F_ATRAT,F_REP,A_O_ATRAT,A_O_REP)

    ! Atualiza as posições finais usando media ponderada 
    !X_NEW = X_OLD + DT * 0.5D0 * (VX + VX_TEMP)
    !Y_NEW = Y_OLD + DT * 0.5D0 * (VY + VY_TEMP)
	
	! Atualiza as posições finais usando apenas k2
    X_NEW = X_OLD + DT * VX_TEMP
    Y_NEW = Y_OLD + DT * VY_TEMP

END SUBROUTINE RungeKutta2_Method

!-------------------------------------------------------------------------------

SUBROUTINE Funcao(T, X, Y, VX, VY, NP, ALPHA_M, ALPHA_D, LX, LY, FX_CURRENT, FY_CURRENT,&
N_PIN_ATRAT,N_PIN_REP, X_PIN_ATRAT, Y_PIN_ATRAT,X_PIN_REP,Y_PIN_REP,F_ATRAT,F_REP,A_O_ATRAT,A_O_REP)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: T, LX, LY, ALPHA_M, ALPHA_D
    INTEGER, INTENT(IN) :: NP, N_PIN_ATRAT, N_PIN_REP
    DOUBLE PRECISION, INTENT(IN) :: X(NP), Y(NP), X_PIN_ATRAT(N_PIN_ATRAT),&
    Y_PIN_ATRAT(N_PIN_ATRAT),X_PIN_REP(N_PIN_REP), Y_PIN_REP(N_PIN_REP)
    DOUBLE PRECISION, INTENT(OUT) :: VX(NP), VY(NP)
    DOUBLE PRECISION :: R, FX(NP), FY(NP), B
    INTEGER :: K, MX, MY, I
    DOUBLE PRECISION ::  DX, DY, BESSK, FDX, FDY
    DOUBLE PRECISION :: X1, Y1
    DOUBLE PRECISION :: FX_PIN, FY_PIN,F_ATRAT,F_REP,A_O_ATRAT,A_O_REP
    DOUBLE PRECISION :: FX_CURRENT, FY_CURRENT !forças corrente 
      
    ! Loop para calcular as interações entre partículas           
    DO I = 1, NP
    
        FX(I) = 0.D0                    
        FY(I) = 0.D0
        
        DO K = 1, NP ! Laço das particulas
        
            X1 = X(I) - X(K)                    ! Cálculo das diferenças de posição entre partículas
            Y1 = Y(I) - Y(K)
        
            IF (I .NE. K) THEN ! Verifica I com K
            
                DO MX = -1, 1 ! Caixas imagens em x
                
                    DO MY = -1, 1 ! Caixas imagens em y

                        DX = X1 + MX * LX
                        DY = Y1 + MY * LY
                        R = DSQRT(DX**2 + DY**2)                                  ! Vai ser usado no B
                        B = BESSK(1,R)                                           ! Function BESSEL(1,R)
                        FX(I) =  FX(I) + (B * (DX)/R)                        
                        FY(I) =  FY(I) + (B * (DY)/R)
                    END DO
                END DO    
            END IF            
        END DO 
        
        CALL CALC_PINNING_FORCE(X(I), Y(I), N_PIN_ATRAT,N_PIN_REP, X_PIN_ATRAT, &     ! Chama a subrotina para calcular a força de pinning
        Y_PIN_ATRAT,X_PIN_REP,Y_PIN_REP, FX_PIN, FY_PIN,F_ATRAT,F_REP,A_O_ATRAT,A_O_REP)
              
                    ! Soma a força de pinning às forças totais
                    FX(I) = FX(I) + FX_PIN + FX_CURRENT
                    FY(I) = FY(I) + FY_PIN + FY_CURRENT

        VX(I) = ( ALPHA_D * FX(I) + ALPHA_M * FY(I)) / (ALPHA_D**2 + ALPHA_M**2)
        VY(I) = (-ALPHA_M * FX(I) + ALPHA_D * FY(I)) / (ALPHA_D**2 + ALPHA_M**2)
        
    END DO

END SUBROUTINE Funcao

!-------------------------------------------------------------------------------

! Subrotina para calcular a força de pinning
SUBROUTINE CALC_PINNING_FORCE(X_SKYRMION, Y_SKYRMION,N_PIN_ATRAT,N_PIN_REP, X_PIN_ATRAT, Y_PIN_ATRAT,&
X_PIN_REP,Y_PIN_REP, FX_PIN, FY_PIN,F_ATRAT,F_REP,A_O_ATRAT,A_O_REP)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: X_SKYRMION, Y_SKYRMION
    INTEGER, INTENT(IN) :: N_PIN_ATRAT, N_PIN_REP
    DOUBLE PRECISION, INTENT(IN) :: X_PIN_ATRAT(N_PIN_ATRAT), Y_PIN_ATRAT(N_PIN_ATRAT),X_PIN_REP(N_PIN_REP), Y_PIN_REP(N_PIN_REP)
    DOUBLE PRECISION, INTENT(OUT) :: FX_PIN, FY_PIN
    DOUBLE PRECISION :: DX, DY, R_IO, F_O_ATRAT, F_O_REP, A_O_ATRAT, A_O_REP ,PINNING_FORCE_X_ATRAT
    DOUBLE PRECISION :: PINNING_FORCE_Y_ATRAT,PINNING_FORCE_X_REP,PINNING_FORCE_Y_REP, F_ATRAT,F_REP
    INTEGER :: K
    
    
    ! *, "Entrando em CALC_PINNING_FORCE"                   !Debug
    
    !PRINT *, "F_ATRAT =", F_ATRAT, "F_REP =", F_REP

    
    ! Inicializa as forças de pinning
    FX_PIN = 0.0D0
    FY_PIN = 0.0D0
    
    ! Loop para calcular a força de pinning atrativa
    DO K = 1, N_PIN_ATRAT
        DX = X_SKYRMION - X_PIN_ATRAT(K)
        DY = Y_SKYRMION - Y_PIN_ATRAT(K)
        R_IO = (DX**2 + DY**2) 
        
		
    FX_PIN = FX_PIN + ((2*F_ATRAT*DX/A_O_ATRAT**2) * DEXP(-((R_IO)/(A_O_ATRAT)**2))) 
	FY_PIN = FY_PIN + ((2*F_ATRAT*DY/A_O_ATRAT**2) * DEXP(-((R_IO)/(A_O_ATRAT)**2)))
	

    END DO

    DO K = 1, N_PIN_REP
        DX = X_SKYRMION - X_PIN_REP(K)
        DY = Y_SKYRMION - Y_PIN_REP(K)
       	R_IO = (DX**2 + DY**2)
			 
    
	FX_PIN = FX_PIN + ((2*F_REP*DX/A_O_REP**2) * DEXP(-((R_IO)/(A_O_REP)**2))) 
	FY_PIN = FY_PIN + ((2*F_REP*DY/A_O_REP**2) * DEXP(-((R_IO)/(A_O_REP)**2)))

            
    END DO
    
END SUBROUTINE CALC_PINNING_FORCE
!-------------------------------------------------------------------------------

! Subrotina para salvar os resultados no arquivo "out.dat"
SUBROUTINE Save_Historic(N_WRITE, NP, DT, Y_HISTORIC, T_HISTORIC, X_HISTORIC, FX_CURRENT, FX_STR, A_O_ATRAT_STR, A_O_REP_STR,&
 F_ATRAT_STR, F_REP_STR)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: N_WRITE, NP
    DOUBLE PRECISION, INTENT(IN) :: DT
    DOUBLE PRECISION, INTENT(IN) :: Y_HISTORIC(N_WRITE, NP), T_HISTORIC(N_WRITE), X_HISTORIC(N_WRITE, NP), FX_CURRENT
    INTEGER :: J, P
    CHARACTER(LEN=200) :: FILENAME,FX_STR, A_O_ATRAT_STR, A_O_REP_STR, F_ATRAT_STR, F_REP_STR                                      ! Variável que armazena o nome do arquivo

    FILENAME = 'out.dat'                                                 ! Nome do arquivo
    
    WRITE(FILENAME, '("out_",A,"_A_O_ATRAT_",A,"_A_O_REP_",A,"_F_ATRAT_",A,"_F_REP_",A,".dat")') TRIM(FX_STR), TRIM(A_O_ATRAT_STR),&
	TRIM(A_O_REP_STR),TRIM(F_ATRAT_STR), TRIM(F_REP_STR)  
    OPEN(10, FILE=FILENAME, ACTION="WRITE", STATUS="REPLACE") ! Abre um arquivo de escrita, e substitui se já tiver algum com o mesmo nome 

    ! Loop para escrever o passo e o tempo 
    DO J = 1, N_WRITE
        WRITE(10, *)  (X_HISTORIC(J, P), Y_HISTORIC(J, P), P = 1, NP)
    END DO

    CLOSE(10)
END SUBROUTINE Save_Historic
!-------------------------------------------------------------------------------

! Função de geração de números aleatórios
FUNCTION RAN2(IDUM)
    INTEGER IDUM, IM1, IM2, IMM1, IA1, IA2, IQ1, IQ2, IR1, IR2, NTAB, NDIV
    DOUBLE PRECISION RAN2, AM, RNMX, EPS
    PARAMETER (IM1=2147483563, IM2=2147483399, AM=1./IM1, IMM1=IM1-1, IA1=40014, &
              IA2=40692, IQ1=IM1/IA1, IQ2=IM2/IA2, IR1=52774, IR2=12211, &
              NTAB=32, NDIV=1+IMM1/NTAB, EPS=1.E-14, RNMX=1.-EPS)
    INTEGER J, K, IDUM2, IV(NTAB)
    DOUBLE PRECISION TEMP
    SAVE IDUM2, IV, IY
    DATA IDUM2 /123456789/, IV /NTAB*0/, IY /0/

    IF (IDUM.LE.0) THEN
        IDUM=MAX(-IDUM,1)
        IDUM2=IDUM
        DO 1 J=NTAB+7,1,-1
            K=IDUM/IQ1
            IDUM=IA1*(IDUM-K*IQ1)-K*IR1
            IF (IDUM.LT.0) IDUM=IDUM+IM1
            IF (J.LE.NTAB) IV(J)=IDUM
    1   CONTINUE
        IY=IV(1)
    END IF
    K=IDUM/IQ1
    IDUM=IA1*(IDUM-K*IQ1)-K*IR1
    IF (IDUM.LT.0) IDUM=IDUM+IM1
    K=IDUM2/IQ2
    IDUM2=IA2*(IDUM2-K*IQ2)-K*IR2
    IF (IDUM2.LT.0) IDUM2=IDUM2+IM2
    J=1+IY/NDIV
    IY=IV(J)-IDUM2
    IV(J)=IDUM
    IF (IY.LT.1) IY=IY+IMM1
    TEMP=AM*IY
    RAN2=MIN(TEMP,RNMX)
END FUNCTION RAN2

!-------------------------------------------------------------------------------

! Função BESSEL

      FUNCTION BESSK(N, X)
      IMPLICIT NONE
      INTEGER :: N, J
      DOUBLE PRECISION :: X, BESSK, BESSK0, BESSK1, TOX, BK, BKM, BKP

!     ------------------------------------------------------------------------
!     ESTE SUBPROGRAMA CALCULA A FUNÇÃO DE BESSEL MODIFICADA DE TERCEIRA
!     ESPÉCIE DE ORDEM INTEIRA N PARA QUALQUER X REAL POSITIVO > 0. AQUI
!     UTILIZA-SE A FÓRMULA DE RECORRÊNCIA CLÁSSICA, COMEÇANDO DE BESSK0 E BESSK1.
!
!     THIS ROUTINE CALCULATES THE MODIFIED BESSEL FUNCTION OF THE THIRD
!     KIND OF INTEGER ORDER, N FOR ANY POSITIVE REAL ARGUMENT, X. THE
!     CLASSICAL RECURSION FORMULA IS USED, STARTING FROM BESSK0 AND BESSK1.
!     ------------------------------------------------------------------------
!     REFERÊNCIA:
!     C.W.CLENSHAW, CHEBYSHEV SERIES FOR MATHEMATICAL FUNCTIONS,
!     MATHEMATICAL TABLES, VOL.5, 1962.
!     ------------------------------------------------------------------------

      IF (N .EQ. 0) THEN
         BESSK = BESSK0(X)
         RETURN
      ENDIF
      IF (N .EQ. 1) THEN
         BESSK = BESSK1(X)
         RETURN
      ENDIF
      IF (X .EQ. 0.D0) THEN
         BESSK = 1.D30
         RETURN
      ENDIF
      TOX = 2.D0 / X
      BK  = BESSK1(X)
      BKM = BESSK0(X)
      DO J = 1, N-1
         BKP = BKM + REAL(J) * TOX * BK
         BKM = BK
         BK  = BKP
      END DO
      BESSK = BK
      RETURN
      END FUNCTION BESSK

!     ----------------------------------------------------------------------
      FUNCTION BESSK0(X)
      IMPLICIT NONE
      DOUBLE PRECISION :: X, BESSK0, Y, AX, BESSI0
      DOUBLE PRECISION :: P1, P2, P3, P4, P5, P6, P7
      DOUBLE PRECISION :: Q1, Q2, Q3, Q4, Q5, Q6, Q7
      DATA P1, P2, P3, P4, P5, P6, P7 / -0.57721566D0, 0.42278420D0, 0.23069756D0,&
	   0.3488590D-1, 0.262698D-2, 0.10750D-3, 0.74D-5 /
      DATA Q1, Q2, Q3, Q4, Q5, Q6, Q7 / 1.25331414D0, -0.7832358D-1, 0.2189568D-1, -0.1062446D-1,&
	  0.587872D-2, -0.251540D-2, 0.53208D-3 /

      IF (X .EQ. 0.D0) THEN
         BESSK0 = 1.D30
         RETURN
      ENDIF
      IF (X .LE. 2.D0) THEN
         Y = X * X / 4.D0
         AX = -DLOG(X / 2.D0) * BESSI0(X)
         BESSK0 = AX + (P1 + Y * (P2 + Y * (P3 + Y * (P4 + Y * (P5 + Y * (P6 + Y * P7))))))
      ELSE
         Y = 2.D0 / X
         AX = DEXP(-X) / DSQRT(X)
         BESSK0 = AX * (Q1 + Y * (Q2 + Y * (Q3 + Y * (Q4 + Y * (Q5 + Y * (Q6 + Y * Q7))))))
      ENDIF
      RETURN
      END FUNCTION BESSK0

!     ----------------------------------------------------------------------
      FUNCTION BESSK1(X)
      IMPLICIT NONE
      DOUBLE PRECISION :: X, BESSK1, Y, AX,BESSI1
      DOUBLE PRECISION :: P1, P2, P3, P4, P5, P6, P7
      DOUBLE PRECISION :: Q1, Q2, Q3, Q4, Q5, Q6, Q7
      DATA P1, P2, P3, P4, P5, P6, P7 / 1.D0, 0.15443144D0, -0.67278579D0,&
	  -0.18156897D0, -0.1919402D-1, -0.110404D-2, -0.4686D-4 /
      DATA Q1, Q2, Q3, Q4, Q5, Q6, Q7 / 1.25331414D0, 0.23498619D0, -0.3655620D-1,&
	  0.1504268D-1, -0.780353D-2, 0.325614D-2, -0.68245D-3 /

      IF (X .EQ. 0.D0) THEN
         BESSK1 = 1.D32
         RETURN
      ENDIF
      IF (X .LE. 2.D0) THEN
         Y = X * X / 4.D0
         AX = DLOG(X / 2.D0) * BESSI1(X)
         BESSK1 = AX + (1.D0 / X) * (P1 + Y * (P2 + Y * (P3 + Y * (P4 + Y * (P5 + Y * (P6 + Y * P7))))))
      ELSE
         Y = 2.D0 / X
         AX = DEXP(-X) / DSQRT(X)
         BESSK1 = AX * (Q1 + Y * (Q2 + Y * (Q3 + Y * (Q4 + Y * (Q5 + Y * (Q6 + Y * Q7))))))
      ENDIF
      RETURN
      END FUNCTION BESSK1

!     ----------------------------------------------------------------------
!     FUNÇÃO DE BESSEL DA PRIMEIRA ESPÉCIE DE ORDEM ZERO.
!     Bessel Function of the 1st kind of order zero.
      FUNCTION BESSI0(X)
      IMPLICIT NONE
      DOUBLE PRECISION :: X, BESSI0, Y, AX, BX
      DOUBLE PRECISION :: P1, P2, P3, P4, P5, P6, P7
      DOUBLE PRECISION :: Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9
      DATA P1, P2, P3, P4, P5, P6, P7 / 1.D0, 3.5156229D0, 3.0899424D0,&
	  1.2067429D0, 0.2659732D0, 0.360768D-1, 0.45813D-2 /
      DATA Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9 / 0.39894228D0, 0.1328592D-1, 0.225319D-2,&
	  -0.157565D-2, 0.916281D-2, -0.2057706D-1, 0.2635537D-1, -0.1647633D-1, 0.392377D-2 /

      IF (DABS(X) .LT. 3.75D0) THEN
         Y = (X / 3.75D0) ** 2
         BESSI0 = P1 + Y * (P2 + Y * (P3 + Y * (P4 + Y * (P5 + Y * (P6 + Y * P7)))))
      ELSE
         AX = DABS(X)
         Y = 3.75D0 / AX
         BX = DEXP(AX) / DSQRT(AX)
         AX = Q1 + Y * (Q2 + Y * (Q3 + Y * (Q4 + Y * (Q5 + Y * (Q6 + Y * (Q7 + Y * (Q8 + Y * Q9)))))))
         BESSI0 = AX * BX
      ENDIF
      RETURN
      END FUNCTION BESSI0

!     ----------------------------------------------------------------------
!     FUNÇÃO DE BESSEL DA PRIMEIRA ESPÉCIE DE ORDEM UM.
!     Bessel Function of the 1st kind of order one.
      FUNCTION BESSI1(X)
      IMPLICIT NONE
      DOUBLE PRECISION :: X, BESSI1, Y, AX, BX
      DOUBLE PRECISION :: P1, P2, P3, P4, P5, P6, P7
      DOUBLE PRECISION :: Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9
      DATA P1, P2, P3, P4, P5, P6, P7 / 0.5D0, 0.87890594D0, 0.51498869D0, 0.15084934D0,&
	  0.2658733D-1, 0.301532D-2, 0.32411D-3 /
      DATA Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9 / 0.39894228D0, -0.3988024D-1, -0.362018D-2,&
	  0.163801D-2, -0.1031555D-1, 0.2282967D-1, -0.2895312D-1, 0.1787654D-1, -0.420059D-2 /

      IF (DABS(X) .LT. 3.75D0) THEN
         Y = (X / 3.75D0) ** 2
         BESSI1 = X * (P1 + Y * (P2 + Y * (P3 + Y * (P4 + Y * (P5 + Y * (P6 + Y * P7))))))
      ELSE
         AX = DABS(X)
         Y = 3.75D0 / AX
         BX = DEXP(AX) / DSQRT(AX)
         AX = Q1 + Y * (Q2 + Y * (Q3 + Y * (Q4 + Y * (Q5 + Y * (Q6 + Y * (Q7 + Y * (Q8 + Y * Q9)))))))
         BESSI1 = BX * AX * DSIGN(1.D0, X)
      ENDIF
      RETURN
      END FUNCTION BESSI1

     




















