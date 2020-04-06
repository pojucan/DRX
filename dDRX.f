      ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      ! Cálculo da distância interplanar em cristalografia
      ! Autores: Marcello Pojucan Magaldi Santos e Carolina Santana
      ! Data:26/10/2019
      ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      program dDRX
      !
      implicit none
      !
      real erroexp !erro experimental do equipamento
      real errocorr !erro corrigido pelo metodo enviado
      real errotcom !valor do erro combinado do angulo
      real errod !erro da distancia interplanar
      real thetagrauscorr !angulo em graus corrigido pelo metodo
      real trad !angulo em radianos
      real lambdarx !comprimento de onda do raio-x
      real sentrad !calculo do seno de teta em radianos
      real sentradquad !calculo do seno de teta em radianos ao quadrado
      real costrad !calculo de cosseno de teta em radianos
      real d !Distancia interplanar
      real a !Parametro de rede da rede cristalina
      real erroa !erro do parametro de rede
      real h, k, l ! indices de miller
      real peso !peso do erro do parametro de rede
      real apeso !parametro de rede x o peso de seu erro
      !
      data erroexp/0.0075/, lambdarx/1.540629/
      !
      write (*,*) "Programa - daDRX"
      write (*,*) "Autores: Santana, C.A. e Pojucan, M.M.S."
      write (*,*) "Data: 26/10/2019"
      write (*,*) "-----------------------------------------"
      write (*,*)
      write (*,*) "Qual o valor do 2Theta (graus) corrigido:"
      write (*,*)
      read (*,*) thetagrauscorr
      write (*,*)
      write (*,*) "Qual o valor do erro do 2Theta (graus) corrigido:"
      write (*,*)
      read (*,*) errocorr
      write (*,*)
      !
      errotcom=sqrt((erroexp**2)+(errocorr**2))
      !
      pi=asin(1.0)*2.0
      !
      trad=(thetagrauscorr/2)*(pi/180)
      sentrad=sin(trad)
      costrad=cos(trad)
      !
      d=lambdarx/(2*sentrad)
      !
      sentradquad=sentrad**2
      errod=(lambdarx/(2*sentradquad))*costrad*(errotcom*(pi/180))
      !
      write (*,*) "Quais o valor de k h l:"
      write (*,*)
      read (*,*) h, k, l
      write (*,*)
      !
      a=d*sqrt((h**2)+(k**2)+(l**2))
      !
      erroa=errod*(sqrt((h**2)+(k**2)+(l**2)))
      !
      peso=1/(erroa**2)
      !
      apeso=a*peso
      !
      teste=apeso/peso
      !
      write (*,*) "Angulo em radianos:",trad
      write (55,*) "Angulo em radianos:",trad
      write (*,*) "Seno de teta em radianos:",sentrad
      write (55,*) "Seno de teta em radianos:",sentrad
      write (*,*) "Seno de teta em radianos ao quadrado:",sentradquad
      write (55,*) "Seno de teta em radianos ao quadrado:",sentradquad
      write (*,*) "Cosseno de teta em radianos:",costrad
      write (55,*) "Cosseno de teta em radianos:",costrad
      write (*,*) "Erro combinado de teta:",errotcom
      write (55,*) "Erro combinado de teta:",errotcom
      write (*,*) "Distancia interplanar (d):",d
      write (55,*) "Distancia interplanar (d):",d
      write (*,*) "Erro da distancia interplanar:"," +/-",errod
      write (55,*) "Erro da distancia interplanar:"," +/-",errod
      write (*,*) "Parametro de rede (a):",a
      write (55,*) "Parametro de rede (a):",a
      write (*,*) "Erro do parametro de rede (+/-):",erroa
      write (55,*) "Erro do parametro de rede(+/-):",erroa
      write (*,*) "Peso do erro do parametro de rede:",peso
      write (55,*) "Peso do erro do parametro de rede:",peso
      write (*,*) "Parametro de rede(a) x Peso do erro:",apeso
      write (55,*) "Parametro de rede(a) x Peso do erro:",apeso
      write (*,*) teste
      write (55,*) teste
      !
      read (*,*)
      end
