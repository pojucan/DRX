PROGRAM dDRX
  IMPLICIT NONE

  ! Variáveis reais
  REAL :: erroexp ! Erro experimental do equipamento
  REAL :: errocorr ! Erro corrigido pelo método enviado
  REAL :: errotcom ! Valor do erro combinado do ângulo
  REAL :: errod ! Erro da distância interplanar
  REAL :: thetagrauscorr ! Ângulo em graus corrigido pelo método
  REAL :: trad ! Ângulo em radianos
  REAL :: lambdarx ! Comprimento de onda do raio-x
  REAL :: sentrad ! Cálculo do seno de teta em radianos
  REAL :: sentradquad ! Cálculo do seno de teta em radianos ao quadrado
  REAL :: costrad ! Cálculo de cosseno de teta em radianos
  REAL :: d ! Distância interplanar
  REAL :: a ! Parâmetro de rede da rede cristalina
  REAL :: erroa ! Erro do parâmetro de rede
  REAL :: h, k, l ! Índices de Miller
  REAL :: peso ! Peso do erro do parâmetro de rede
  REAL :: apeso ! Parâmetro de rede x o peso de seu erro
  REAL :: pi ! Valor de pi
  REAL :: teste ! Razão

  ! Valores iniciais:
  erroexp = 0.0075
  lambdarx = 1.540629

  ! Interface com o usuário:
  print*, "Programa - dDRX"
  print*, "-----------------------------------------"
  print*, ""
  print*, "Qual o valor do 2Theta (graus) corrigido:"
  print*, ""
  read (*,*) thetagrauscorr
  print*, ""
  print*, "Qual o valor do erro do 2Theta (graus) corrigido:"
  print*, ""
  read (*,*) errocorr
  print*, ""

  ! Cálculos:
  errotcom = SQRT(erroexp**2 + errocorr**2)
  pi = 2.0 * ASIN(1.0)
  trad = (thetagrauscorr / 2) * (pi / 180)
  sentrad = SIN(trad)
  costrad = COS(trad)
  d = lambdarx / (2 * sentrad)
  sentradquad = sentrad**2
  errod = (lambdarx / (2 * sentradquad)) * costrad * (errotcom * (pi / 180))

  ! Interface com o usuário:
  print*, "Quais o valor de k h l:"
  print*, ""
  read (*,*) h, k, l
  print*, ""

  ! Cálculos:
  a = d * SQRT(h**2 + k**2 + l**2)
  erroa = errod * SQRT(h**2 + k**2 + l**2)
  peso = 1 / (erroa**2)
  apeso = a * peso
  teste = apeso / peso

  ! Saída do programa:
  print*, "Angulo em radianos:", trad
  print*, "Seno de teta em radianos:", sentrad
  print*, "Seno de teta em radianos ao quadrado:", sentradquad
  print*, "Cosseno de teta em radianos:", costrad
  print*, "Erro combinado de teta:", errotcom
  print*, "Distancia interplanar (d):", d
  print*, "Erro da distancia interplanar: +/-", errod
  print*, "Parametro de rede (a):", a
  print*, "Erro do parametro de rede (+/-):", erroa
  print*, "Peso do erro do parametro de rede:", peso
  print*, "Parametro de rede(a) x Peso do erro:", apeso
  print*, teste

  ! Fechando o programa:
  read (*,*)
END PROGRAM dDRX
