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

(deffacts clientes
    (cliente (id 1) (name Renato Maudim) (data-nascimento 26 06 1986)
    (data-carta 12 5 2005) (data-validade-carta 12 5 2050))
)

(deffacts pedidos-reserva
    (pedido-reserva (id-reserva 21) (id-cliente 1)
    (data-levantamento 1 1 2018) (data-devolucao 16 1 2018)
    (classe mini) (modelo smart))
    (pedido-reserva (id-reserva 22) (id-cliente 1)
    (data-levantamento 17 1 2018) (data-devolucao 20 1 2018)
    (classe suv) (modelo smart))

    (pedidos-reserva-nao-cliente (id-reserva 23)
    (data-levantamento 1 1 2018) (data-devolucao 15 1 2018)
    (nome Ze Pascal) (data-nascimento 15 02 1996)
    (data-carta 15 01 2015) (data-validade-carta 15 01 2040) )
)

(deffacts carros
	(automovel-disponivel (classe mini) (marcamodelo smart)
	(km-percorridos 154600) (matricula 01-AB-10))
	(automovel-disponivel (classe mini) (marcamodelo smart)
	(km-percorridos 854123) (matricula 12-AB-34))
	(automovel-disponivel (classe suv) (marcamodelo hyundaikona)
	(km-percorridos 854123) (matricula 89-TY-36))
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
    (pedido-reserva (id-reserva ?num1&:(< ?num1 ?num)) (id-cliente ?id-c) (data-devolucao ?d-d ?m-d ?a-d))

    (test (or (or (< ?d ?d-d ) (< ?m ?m-d) ) (> ?a ?a-d) ) )

    =>
    (retract ?nova-reserva)
    (printout t "Reserva " ?num "nao ativada." crlf "Ja existe uma reserva em curso feita por este cliente." crlf)
)

; verifica se o cliente que esta a efetuar a nova reserva esta na lista negra
; (nao pode efetuar reservas)
(defrule verifica-reserva-negra
    ?nova-reserva <- (reserva-ativada ?num)
    (pedido-reserva (id-reserva ?num) (id-cliente ?id-c))
    (lista-negra ?id-c)

    =>
    (retract ?nova-reserva)
    (printout t "Reserva " ?num " nao ativada." crlf "Cliente " ?id-c " esta na lista negra." crlf)
)

;regra que testa se um cliente é válido
;ou seja, se tem mais de 25 anos e se
;tem carta de condução há mais de 1 ano
(defrule cliente-valido-primeira-reserva
?nova-reserva <- (reserva-ativada ?num)
(pedido-reserva (id-reserva ?num) (id-cliente ?id-c) (data-levantamento ?dl ?ml ?al))
(cliente (id ?id-c) (data-nascimento ?dn ?mn ?an) (data-carta ?dc ?mc ?ac))
(test (and (>= (calcula-anos ?dl ?ml ?al ?dn ?mn ?an 25) 25)
(>= (calcula-anos ?dl ?ml ?al ?dc ?mc ?ac 1) 1)))

; TODO fazer a condicao de 30 dias
=>
(retract ?nova-reserva)
(assert (reserva-valida ?num))
(printout t "O pedido de reserva " ?num " foi aceite." crlf)
)

;regra que testa se um cliente é válido de acordo com reservas já feitas
(defrule cliente-valido-mais-reservas
?nova-reserva <- (reserva-ativada ?num)
(pedido-reserva (id-reserva ?num) (id-cliente ?id-c) (data-levantamento ?dl ?ml ?al))
(pedido-reserva (id-reserva ?num1&:(< ?num1 ?num)) (id-cliente ?id-c) (data-devolucao ?d ?m ?a))
(cliente (id ?id-c) (data-nascimento ?dn ?mn ?an) (data-carta ?dc ?mc ?ac))
(test (and (>= (calcula-anos ?dl ?ml ?al ?dn ?mn ?an 25) 25)
(>= (calcula-anos ?dl ?ml ?al ?dc ?mc ?ac 1) 1)))
(test (or (or (>= ?dl ?d) (>= ?ml ?m)) (>= ?al ?a)))

; TODO fazer a condicao de 30 dias
=>
(retract ?nova-reserva)
(assert (reserva-valida ?num))
(printout t "O pedido de reserva " ?num " foi aceite." crlf)
)


; verifica se a reserva que foi ativada e ativada por um novo cliente (ou seja,
; cliente nao registado)
; se for, cria um novo cliente com os dados
(defrule verifica-reserva-nao-cliente
    (reserva-ativada ?num)
    ?reserva-nao-cliente <- (pedidos-reserva-nao-cliente
        (id-reserva ?num) (data-levantamento $?data-lev) (data-devolucao $?data-dev)
        (classe ?classe) (modelo ?modelo)
        (nome $?nome)
        (data-nascimento $?data-nasc) (data-carta $?data-carta)
        (data-validade-carta $?data-val-carta-cond) )
    =>
    (bind ?id-novo-c (random 1111111 9999999))

    (assert (cliente (id ?id-novo-c) (name ?nome)
            (data-nascimento ?data-nasc) (data-carta ?data-carta)
            (data-validade-carta ?data-val-carta-cond) ))
    (assert (pedido-reserva (id-reserva ?num) (id-cliente ?id-novo-c)
            (data-levantamento ?data-lev) (data-devolucao ?data-dev)
            (classe ?classe) (modelo ?modelo) ))
    (retract ?reserva-nao-cliente) ; cliente deixa de ser nao cliente
)

; Regras relacionadas com associacao de carros a reservas
;afeta um carro cujo pedido inclui marca e modelo
(defrule afeta-carro-reserva-modelo

    ?associa-carro <- (associa-carro-res ?numpedido)

    ?pedidos-reserva <- (pedido-reserva
        (id-reserva ?numpedido) (classe ?classe) (modelo ?modelo) )

    ?carro-disponivel <- (automovel-disponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) )

    ?reserva-ativada <- (reserva-ativada ?numpedido)


    =>
    (assert (reserva-com-carro ?numpedido ?matricula))
    (assert (automovel-indisponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) ))
    (retract ?carro-disponivel)
    (retract ?associa-carro)
    (retract ?reserva-ativada)
    (printout t "Foi afetado o carro "?matricula" ao pedido de reserva " ?numpedido crlf)


)

;afeta um carro cujo pedido nao inclui modelo
(defrule afeta-carro-reserva

    ?associa-carro <- (associa-carro-res ?numpedido)

    ?pedidos-reserva <- (pedido-reserva
        (id-reserva ?numpedido) (classe ?classe) (modelo ?modelo&:(eq ?modelo nil) ) )

    ?carro-disponivel <- (automovel-disponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) )

    ?reserva-ativada <- (reserva-ativada ?numpedido)


    =>
    (assert (reserva-com-carro ?numpedido ?matricula))
    (assert (automovel-indisponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) ))
    (retract ?carro-disponivel)
    (retract ?associa-carro)
    (retract ?reserva-ativada)
    (printout t "Foi afetado o carro "?matricula" ao pedido de reserva " ?numpedido crlf)

)


;afeta um carro cujo modelo e classe estao especificados na reserva
; mas tal nao esta disponivel no momento da reserva, portanto e afetado um carro aleatorio da mesma classe

(defrule afeta-carro-aleatorio-indisp

    ?associa-carro <- (associa-carro-res ?numpedido)

    ?pedidos-reserva <- (pedido-reserva
        (id-reserva ?numpedido) (classe ?classe) (modelo ?modelo) )

    (not (automovel-disponivel (classe ?classe) (marcamodelo ?modelo) ) )

    ?carro-disponivel <- (automovel-disponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) )


    ?reserva-ativada <- (reserva-ativada ?numpedido)


    =>
    (assert (reserva-com-carro ?numpedido ?matricula))
    (assert (automovel-indisponivel
        (classe ?classe) (marcamodelo ?marcamodelo) (matricula ?matricula)
        (km-percorridos ?km-percorridos) ))
    (retract ?carro-disponivel)
    (retract ?associa-carro)
    (retract ?reserva-ativada)
    (printout t "Foi afetado o carro "?matricula" ao pedido de reserva " ?numpedido crlf)

)

; Regras relacionadas com o inicio de um novo dia
; no inicio de cada dia os carros sao levados para aluguer
(defrule inicio-dia
    (dia-corrente ?dia ?mes ?ano)
    ?reserva <- (pedido-reserva (id-reserva ?numpedido) (id-cliente ?id-c)
                (data-levantamento ?dia ?mes ?ano))
    ?reserva-com-carro <- (reserva-com-carro ?numpedido ?matricula)
    =>
    (printout t "Reserva " ?numpedido ": " ?matricula crlf)
)

; Regras recepcao de carros
; Recebe um carro em que existiu pagamento mas veio com danos
(defrule recebe-carro-com-danos
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
    (recepcao-automovel (id-reserva ?numpedido) (cliente-pagou ?pagou&:(eq ?pagou nao))
        (ha-danos ?danos&:(eq ?danos nao)))
    (pedido-reserva (id-reserva ?numpedido) (id-cliente ?id-c))
    (reserva-com-carro ?numpedido ?matricula)
    ?automovel <- (automovel-indisponivel (classe ?classe) (marcamodelo ?marcamodelo)
        (matricula ?matricula))
    =>
    (assert (lista-negra ?id-c))
    (assert (automovel-disponivel (classe ?classe) (marcamodelo ?marcamodelo)
        (matricula ?matricula)))
    (retract ?automovel)
    (printout t "O carro " ?matricula " foi recebido, mas o pagamento nao foi efetuado." crlf)
    (printout t "O cliente " ?id-c " foi adicionado a lista negra.")
)

; Recebe um carro com danos e sem pagamento
(defrule recebe-carro-sem-pagamento-com-danos
    (recepcao-automovel (id-reserva ?numpedido) (cliente-pagou ?pagou&:(eq ?pagou nao))
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
    (printout t "O carro " ?matricula " foi recebido com danos e o pagamento nao foi efetuado." crlf)
    (printout t "O cliente " ?id-c " foi adicionado a lista negra." crlf)
)

; Recebe um carro sem danos e com pagamento
(defrule recebe-carro-sucesso
    (recepcao-automovel (id-reserva ?numpedido) (cliente-pagou ?pagou&:(eq ?pagou sim))
        (ha-danos ?danos&:(eq ?danos nao)))
    (reserva-com-carro ?numpedido ?matricula)
    ?automovel <- (automovel-indisponivel (classe ?classe) (marcamodelo ?marcamodelo)
        (matricula ?matricula))
    =>
    (assert (automovel-disponivel (classe ?classe) (marcamodelo ?marcamodelo)
        (matricula ?matricula)))
    (retract ?automovel)
    (printout t "O carro " ?matricula " foi recebido com sucesso." crlf)
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
    (printout t "Indique qual o dia/mes/ano que se inicia (dd/mm/aaaa): ")
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

    (assert (dia-corrente (string-to-field ?day) (string-to-field ?month) (string-to-field ?year)))
    (printout t "Foram levantadas as seguintes encomendas:" crlf)
    (run)
)

; recepcao de um automovel previamente alugado
(deffunction recebe-carro (?numpedido)
    (printout t "O carro tem danos? (sim / nao)" crlf)
    (bind ?danos (read))

    (printout t "O pagamento foi efetuado? (sim / nao)" crlf)
    (bind ?pagou (read))

    (assert (recepcao-automovel (id-reserva ?numpedido) (cliente-pagou ?pagou)
        (ha-danos ?danos)))
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
