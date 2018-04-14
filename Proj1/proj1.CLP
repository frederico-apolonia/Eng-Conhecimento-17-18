; Grupo xx
; Frederico Apolónia
; Tiago Santos
; Renato Gondin

(deftemplate cliente
    (slot id)
    (multislot name)
    (multislot data-nascimento)
    (multislot data-carta)
    (multislot data-validade-carta)
)

(deftemplate pedido-reserva
    (slot id-reserva)
    (slot id-cliente)
    (multislot data-levantamento)
    (multislot data-devolucao)
    (slot classe
        (type SYMBOL)
        (allowed-symbols mini economico compacto familiar executivo suv luxo)
        (default mini))
    (slot modelo)
)

(deftemplate pedidos-reserva-nao-cliente
    (slot id-reserva)
    (multislot data-levantamento)
    (multislot data-devolucao)
    (slot classe
        (type SYMBOL)
        (allowed-symbols mini economico compacto familiar executivo suv luxo)
        (default mini))
    (slot modelo)
    (multislot nome)
    (multislot data-nascimento)
    (multislot data-carta)
    (multislot data-validade-carta)
)

(deftemplate automovel-disponivel
    (slot classe)
    (slot marcamodelo)
    (slot km-percorridos)
    (multislot matricula)
)

(deftemplate automovel-indisponivel
    (slot classe)
    (slot marcamodelo)
    (slot km-percorridos)
    (slot matricula)
)

(deftemplate recepcao-automovel
    (slot id-reserva)
    (slot cliente-pagou
        (type SYMBOL)
        (allowed-symbols sim nao)
        (default nao))
    (slot ha-danos
        (type SYMBOL)
        (allowed-symbols sim nao)
        (default nao))
)

(deffacts carros
	(automovel-disponivel (classe mini) (marcamodelo smart)
	(km-percorridos 154600) (matricula 01-AB-10))
	(automovel-disponivel (classe mini) (marcamodelo smart)
	(km-percorridos 854123) (matricula 12-AB-34))
	(automovel-disponivel (classe suv) (marcamodelo hyundaikona)
	(km-percorridos 854123) (matricula 89-TY-36))
)

(deffunction start ()
    (printout t "Loading factos-grupo16.clp" crlf)
    (load factos-grupo16.clp)
    (reset)
    (printout t "Qual o dia que quer considerar como o primeiro dia? (dd/mm/yyyy) ")
    (bind ?date (read))

    (bind ?day-seperator (str-index "/" ?date))

    (bind ?month-seperator
        (str-index "/"
            (sub-string (+ ?day-seperator 1) (str-length ?date) ?date ))
    )

    (bind ?day
        (sub-string 1 (- ?day-seperator 1) ?date )
    )

    (bind ?month
        (sub-string
            (+ ?day-seperator 1)
            (- (+ ?day-seperator ?month-seperator) 1)
            ?date)
    )

    (bind ?year
        (sub-string
            (+ (+ ?day-seperator ?month-seperator) 1)
            (str-length ?date)
            ?date)
    )

    (assert (dia-corrente (string-to-field ?day) (string-to-field ?month)
    (string-to-field ?year)))
)

(deffunction calcula-anos (?d ?m ?a ?d1 ?m1 ?a1 ?idademinima)

    (if (>= (- ?a ?a1) ?idademinima)
        then
        (if (eq (- ?a ?a1) ?idademinima)
            then
            (if (< (- ?m ?m1) 0)
                then
                (return ?idademinima)
                else
                (if (eq 0 (- ?m ?m1))
                    then
                    (if (> 0 (- ?a ?a1))
                        then
                        (return ?idademinima)
                        else
                        (return (- ?idademinima 1))
                    )
                    else
                    (return (- ?idademinima 1))
                )
            )
            else
            (return (- ?a ?a1))
        )
        else
        (return (- ?a ?a1))
    )

)


;;;;;;;;;;;;
;; REGRAS ;;
;;;;;;;;;;;;

; Regras relacionadas com as reservas

; verifica se a reserva pode ser levantada, isto e, se o cliente nao tem outra reserva
; a decorrer
(defrule verifica-reserva-data
    ?nova-reserva <- (reserva-ativada ?num)
    (pedido-reserva (id-reserva ?num) (id-cliente ?id-c) (data-levantamento ?d ?m ?a))
    (pedido-reserva (id-reserva ?num1&:(< ?num1 ?num)) (id-cliente ?id-c)
    (data-devolucao ?d-d ?m-d ?a-d))

    (test (or (or (< ?d ?d-d ) (< ?m ?m-d) ) (> ?a ?a-d) ) )

    =>
    (retract ?nova-reserva)
    (printout t "Reserva " ?num "nao ativada." crlf "Ja existe uma reserva
    em curso feita por este cliente." crlf)
)

; verifica se o cliente que esta a efetuar a nova reserva esta na lista negra
; (nao pode efetuar reservas)
(defrule verifica-reserva-negra
    ?nova-reserva <- (reserva-ativada ?num)
    (pedido-reserva (id-reserva ?num) (id-cliente ?id-c))
    (lista-negra ?id-c)

    =>
    (retract ?nova-reserva)
    (printout t "Reserva " ?num " nao ativada." crlf "Cliente " ?id-c " esta
    na lista negra." crlf)
)

; regra que testa se uma reserva de um cliente é válida
(defrule cliente-primeira-reserva
    ?nova-reserva <- (reserva-ativada ?num)
    (pedido-reserva (id-reserva ?num) (id-cliente ?id-c) (data-levantamento
        ?dl ?ml ?al))
    (cliente (id ?id-c) (data-nascimento ?dn ?mn ?an) (data-carta ?dc ?mc
        ?ac))

    ; TODO fazer a condicao de 30 dias
=>
    (retract ?nova-reserva)
    (assert (reserva-valida ?num))
    (printout t "O pedido de reserva " ?num " foi aceite." crlf)
)

; regra que testa se uma reserva é válida, sendo que o cliente já tem mais reservas
(defrule cliente-valido-mais-reservas
    ?nova-reserva <- (reserva-ativada ?num)
    (pedido-reserva (id-reserva ?num) (id-cliente ?id-c) (data-levantamento
        ?dl ?ml ?al))
    (pedido-reserva (id-reserva ?num1&:(< ?num1 ?num)) (id-cliente ?id-c)
    (data-devolucao ?dd ?md ?ad))
    (cliente (id ?id-c) (data-nascimento ?dn ?mn ?an) (data-carta ?dc ?mc
        ?ac))

    ; TODO fazer a condicao de 30 dias
=>
    (retract ?nova-reserva)
    (assert (reserva-valida ?num))
    (printout t "O pedido de reserva " ?num " foi aceite." crlf)
)

; regra que testa se um não-cliente tem menos de 25 anos
(defrule nao-cliente-menos-25
    ?nova-reserva <- (reserva-ativada ?num)
    (pedidos-reserva-nao-cliente (id-reserva ?num) (data-levantamento
        ?dl ?ml ?al)  (data-nascimento ?dn ?mn ?an) (data-carta ?dc ?mc ?ac))
    ; teste para verificar se tem menos de 25 anos
    (test (< (calcula-anos ?dl ?ml ?al ?dn ?mn ?an 25) 25))
=>
    (retract ?nova-reserva)
    (printout t "O pedido de reserva " ?num " foi rejeitado." crlf)
    (printout t "Motivo: Nao cliente tem menos de 25 anos" crlf)
)

;regra que testa se um nãocliente tem a carta ha menos de 1 ano
(defrule nao-cliente-menos-1-ano-carta
    ?nova-reserva <- (reserva-ativada ?num)
    (pedidos-reserva-nao-cliente (id-reserva ?num) (data-levantamento
        ?dl ?ml ?al)  (data-nascimento ?dn ?mn ?an) (data-carta ?dc ?mc ?ac))
    ; teste para verificar se tem menos de 1 ano de carta
    (test (< (calcula-anos ?dl ?ml ?al ?dc ?mc ?ac 1) 1))
=>
    (retract ?nova-reserva)
    (printout t "O pedido de reserva " ?num " foi rejeitado." crlf)
    (printout t "Motivo: Nao cliente tem menos de 1 ano de carta" crlf)
)

; verifica se a reserva que foi ativada e ativada por um novo cliente (ou seja,
; cliente nao registado) e se esta reserva ativada é válida
; se for, cria um novo cliente com os dados
(defrule verifica-reserva-nao-cliente
    (reserva-ativada ?num)
    ?reserva-nao-cliente <- (pedidos-reserva-nao-cliente
        (id-reserva ?num) (data-levantamento ?dl ?ml ?al) (data-devolucao
            $?data-dev)
        (classe ?classe) (modelo ?modelo)
        (nome $?nome)
        (data-nascimento ?dn ?mn ?an) (data-carta ?dc ?mc ?ac)
        (data-validade-carta $?data-val-carta-cond) )

        (test (and (>= (calcula-anos ?dl ?ml ?al ?dn ?mn ?an 25) 25)
        (>= (calcula-anos ?dl ?ml ?al ?dc ?mc ?ac 1) 1)))

        ; TODO regra dos 30 dias

    =>
    (bind ?id-novo-c (random 1000000 9999999))

    (assert (cliente (id ?id-novo-c) (name ?nome)
            (data-nascimento ?dn ?mn ?an) (data-carta ?dc ?mc ?ac)
            (data-validade-carta ?data-val-carta-cond) ))
    (assert (pedido-reserva (id-reserva ?num) (id-cliente ?id-novo-c)
            (data-levantamento ?dl ?ml ?al) (data-devolucao ?data-dev)
            (classe ?classe) (modelo ?modelo) ))
    (retract ?reserva-nao-cliente) ; cliente deixa de ser nao cliente

    (printout t "O pedido de reserva " ?num " foi aceite." crlf)
    (printout t "Foi criado um novo cliente com o numero " ?id-novo-c crlf)
)

; Regras relacionadas com associacao de carros a reservas
; afeta um carro cujo pedido inclui marca e modelo
(defrule afeta-carro-reserva-modelo
    ?associa-carro <- (associa-carro-res ?numpedido)
    ?pedidos-reserva <- (pedido-reserva
        (id-reserva ?numpedido) (classe ?classe) (modelo ?modelo) )
    ?carro-disponivel <- (automovel-disponivel
        (classe ?classe) (marcamodelo ?modelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) )

    ?reserva-valida <- (reserva-valida ?numpedido)



    =>
    (assert (reserva-com-carro ?numpedido ?matricula))
    (assert (automovel-indisponivel
        (classe ?classe) (marcamodelo ?modelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) ))
    (retract ?carro-disponivel)
    (retract ?associa-carro)
    (printout t "Foi afetado o carro "?matricula" ao pedido de reserva "
?numpedido crlf)


)

;afeta um carro cujo pedido nao inclui modelo
(defrule afeta-carro-reserva

    ?associa-carro <- (associa-carro-res ?numpedido)
    ?pedidos-reserva <- (pedido-reserva
        (id-reserva ?numpedido) (classe ?classe) (modelo ?modelo&:(eq
?modelo nil) ) )

    ?carro-disponivel <- (automovel-disponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) )

    ?reserva-valida <- (reserva-valida ?numpedido)


    =>
    (assert (reserva-com-carro ?numpedido ?matricula))
    (assert (automovel-indisponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) ))
    (retract ?carro-disponivel)
    (retract ?associa-carro)
    (printout t "Foi afetado o carro "?matricula" ao pedido de reserva "
?numpedido crlf)

)


;afeta um carro cujo modelo e classe estao especificados na reserva
; mas tal nao esta disponivel no momento da reserva, portanto e afetado um
; carro aleatorio da mesma classe

(defrule afeta-carro-aleatorio-indisp

    ?associa-carro <- (associa-carro-res ?numpedido)
    ?pedidos-reserva <- (pedido-reserva
        (id-reserva ?numpedido) (classe ?classe) (modelo ?modelo) )
    (not (automovel-disponivel (classe ?classe) (marcamodelo ?modelo) ) )
    ?carro-disponivel <- (automovel-disponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) )
    ?reserva-valida <- (reserva-valida ?numpedido)
    =>
    (assert (reserva-com-carro ?numpedido ?matricula))
    (assert (automovel-indisponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) ))
    (retract ?carro-disponivel)
    (retract ?associa-carro)
    (printout t "Foi afetado o carro "?matricula" ao pedido de reserva "
?numpedido crlf)

)

; Regras relacionadas com o inicio de um novo dia
; no inicio de cada dia os carros sao levados para aluguer
(defrule levantamento-carros
    (levanta-carros ?dia ?mes ?ano)
    ?reserva <- (pedido-reserva (id-reserva ?numpedido) (id-cliente ?id-c)
                (data-levantamento ?dia ?mes ?ano))
    ?reserva-com-carro <- (reserva-com-carro ?numpedido ?matricula)
    =>
    (printout t "Reserva " ?numpedido ": " ?matricula crlf)
)

; Regra relacionada com inicio de um novo dia
(defrule inicia-dia
    ?inicia-dia <- (inicia-dia)
    (dia-corrente ?dia ?mes ?ano)
    =>
    (printout t "Dia que se inicia: " ?dia "/" ?mes "/" ?ano crlf)
    (printout t "Foram levantadas as seguintes encomendas:" crlf)
    (assert (levanta-carros ?dia ?mes ?ano))
    (retract ?inicia-dia)
)

; Regras recepcao de carros
; Recebe um carro em que existiu pagamento mas veio com danos
(defrule recebe-carro-com-danos
    (carro-recebido ?numpedido)
    (recepcao-automovel (id-reserva ?numpedido) (cliente-pagou ?pagou&:(eq ?pagou sim))
        (ha-danos ?danos&:(eq ?danos sim)))
    (pedido-reserva (id-reserva ?numpedido) (id-cliente ?id-c))
    (reserva-com-carro ?numpedido ?matricula)
    ?automovel <- (automovel-indisponivel (classe ?classe) (marcamodelo ?marcamodelo)
        (matricula ?matricula))
    =>
    (assert (lista-negra ?id-c))
    (assert (automovel-disponivel (classe ?classe) (marcamodelo ?marcamodelo)
        (matricula ?matricula)))
    (retract ?automovel)
    (printout t "O carro " ?matricula " foi recebido com danos." crlf)
    (printout t "O cliente " ?id-c " foi adicionado a lista negra." crlf)
)

; Recebe um carro sem danos, mas sem pagamento
(defrule recebe-carro-sem-pagamento
    (carro-recebido ?numpedido)
    (recepcao-automovel (id-reserva ?numpedido) (cliente-pagou ?pagou&:(eq ?pagou nao))
        (ha-danos ?danos&:(eq ?danos nao)))
    (pedido-reserva (id-reserva ?numpedido) (id-cliente ?id-c))
    (reserva-com-carro ?numpedido ?matricula)
    ?automovel <- (automovel-indisponivel (classe ?classe) (marcamodelo
        ?marcamodelo) (matricula ?matricula))
    =>
    (assert (lista-negra ?id-c))
    (assert (automovel-disponivel (classe ?classe) (marcamodelo ?marcamodelo)
        (matricula ?matricula)))
    (retract ?automovel)
    (printout t "O carro " ?matricula " foi recebido, mas o pagamento nao
    foi efetuado." crlf)
    (printout t "O cliente " ?id-c " foi adicionado a lista negra.")
)

; Recebe um carro com danos e sem pagamento
(defrule recebe-carro-sem-pagamento-com-danos
    (carro-recebido ?numpedido)
    (recepcao-automovel (id-reserva ?numpedido) (cliente-pagou ?pagou&:(eq
        ?pagou nao))
        (ha-danos ?danos&:(eq ?danos sim)))
    (pedido-reserva (id-reserva ?numpedido) (id-cliente ?id-c))
    (reserva-com-carro ?numpedido ?matricula)
    ?automovel <- (automovel-indisponivel (classe ?classe) (marcamodelo
        ?marcamodelo) (matricula ?matricula))
    =>
    (assert (lista-negra ?id-c))
    (assert (automovel-disponivel (classe ?classe) (marcamodelo ?marcamodelo)
        (matricula ?matricula)))
    (retract ?automovel)
    (printout t "O carro " ?matricula " foi recebido com danos e o pagamento
    nao foi efetuado." crlf)
    (printout t "O cliente " ?id-c " foi adicionado a lista negra." crlf)
)

; Recebe um carro sem danos e com pagamento
(defrule recebe-carro-sucesso
    (carro-recebido ?numpedido)
    (recepcao-automovel (id-reserva ?numpedido) (cliente-pagou ?pagou&:(eq
        ?pagou sim))
        (ha-danos ?danos&:(eq ?danos nao)))
    (reserva-com-carro ?numpedido ?matricula)
    ?automovel <- (automovel-indisponivel (classe ?classe) (marcamodelo
        ?marcamodelo) (matricula ?matricula))
    =>
    (assert (automovel-disponivel (classe ?classe) (marcamodelo ?marcamodelo)
        (matricula ?matricula)))
    (retract ?automovel)
    (printout t "O carro " ?matricula " foi recebido com sucesso." crlf)
)

; Regra relacionada com o término do dia
(defrule fecho-dia
    ?fechar-dia <- (fechar-dia)
    ?dia-atual <- (dia-corrente ?dia ?mes ?ano)
    =>
    (printout t "Dia de hoje: " ?dia "/" ?mes "/" ?ano crlf)
    (assert (falta-carros ?dia ?mes ?ano))

    (if (or (eq ?mes 1)
        (eq ?mes 3)
        (eq ?mes 5)
        (eq ?mes 7)
        (eq ?mes 8)
        (eq ?mes 10)
        (eq ?mes 12))
        then
        (if (eq ?dia 31)
            then
            (if (eq ?mes 12)
                then
                (assert (dia-corrente 1 1 (+ ?ano 1)))
                (retract ?dia-atual)
                else
                (assert (dia-corrente 1 (+ ?mes 1) ?ano))
                (retract ?dia-atual)
            )
            else
            (assert (dia-corrente (+ ?dia 1) ?mes ?ano))
            (retract ?dia-atual)
        )
        else
        (if (and (eq ?dia 28) (eq ?mes 2))
            then
            (assert (dia-corrente 1 (+ ?mes 1) ?ano))
            (retract ?dia-atual)
            else
            (if (eq ?dia 30)
                then
                (assert (dia-corrente 1 (+ ?mes 1) ?ano))
                (retract ?dia-atual)
                else
                (assert (dia-corrente (+ ?dia 1) ?mes ?ano ))
                (retract ?dia-atual)
            )
        )
    )

    (retract ?fechar-dia)
)

;carro nao foi entregue na data estipulada, portanto deve se contactar o cliente
(defrule prazo-nao-cumprido
	?dia-corrente <- (dia-corrente ?dia ?mes ?ano)
	?reserva <- (pedido-reserva (id-reserva ?numpedido) (id-cliente ?id-c)
    (data-devolucao ?dia ?mes ?ano)	)
	?reserva-com-carro <- (reserva-com-carro ?numpedido ?matricula)

	(not (recepcao-automovel (id-reserva ?numpedido) (cliente-pagou ?pagou)
    (ha-danos ?danos)))
	=>
	(assert (lista-negra ?numpedido))
	(printout t "O carro da reserva " ?numpedido " deveria ter sido entregue
    hoje: contactar cliente" crlf)

)

;;;;;;;;;;;;;
;; FUNCOES ;;
;;;;;;;;;;;;;

; insere um novo pedido de reserva com o numero ?numpedido
(deffunction pedido-reserva (?numpedido)
    (assert (reserva-ativada ?numpedido))
    (run)
)

;afeta um carro
(deffunction afecta-carro (?numpedido)
	(assert (associa-carro-res ?numpedido))
    (run)
)

; inicia um novo dia
(deffunction aluga-carros ()
    (assert (inicia-dia))
    (run)
)

; recepcao de um automovel previamente alugado
(deffunction recebe-carro (?numpedido)
    (assert (carro-recebido ?numpedido))
    (run)
)

;
(deffunction verifica-faltas ()
	(assert (fechar-dia))
	(run)
)

; guarda os factos num ficheiro
(deffunction guarda-factos (?filename)
    (save-facts ?filename)
)

; carrega os factos de um ficheiro
(deffunction carrega-factos (?filename)
    (load-facts ?filename)
)
