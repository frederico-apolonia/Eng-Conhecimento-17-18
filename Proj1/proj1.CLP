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

(deftemplate automoveis-disponiveis
    (slot classe)
    (slot marcamodelo)
    (slot km-percorridos)
    (multislot matricula)
)

(deftemplate recepcao-automovel
    (slot id-reserva)
    (slot cliente-pagou)
    (slot ha-danos)
)

(deffacts pedidos-reserva
    (pedido-reserva (id-reserva 21) (id-cliente 1) (data-levantamento 1 1 2018) (data-devolucao 16 1 2018) (classe mini) (modelo smart))
    (pedido-reserva (id-reserva 22) (id-cliente 1) (data-levantamento 15 1 2018) (data-devolucao 20 1 2018) (classe mini) (modelo smart))
)

; verifica se a reserva pode ser levantada, isto e, se o cliente nao tem outra reserva
; a decorrer
(defrule verifica-reserva-data
    ?nova-reserva <- (reserva-ativada ?num)
    (pedido-reserva (id-reserva ?num) (id-cliente ?id-c) (data-levantamento ?d ?m ?a)) 
    (pedido-reserva (id-reserva ?num1&:(< ?num1 ?num)) (id-cliente ?id-c) (data-devolucao ?d-d ?m-d ?a-d))

    (test (or (or (< ?d ?d-d ) (< ?m ?m-d) ) (> ?a ?a-d) ) )

    =>
    (retract ?nova-reserva)
    (printout t "Reserva nao ativada." crlf "Ja existe uma reserva em curso feita por este cliente." crlf)
)

; verifica se o cliente que esta a efetuar a nova reserva esta na lista negra
; (nao pode efetuar reservas)
(defrule verifica-reserva-negra
    ?nova-reserva <- (reserva-ativada ?num)
    (pedido-reserva (id-reserva ?num) (id-cliente ?id-c))
    (lista-negra ?id-c)
    
    =>
    (retract ?nova-reserva)
    (printout t "Reserva nao ativada." crlf "Cliente " ?id-c " esta na lista negra." crlf)
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

    ?novo-id-c <- (random)
    =>
    (assert (cliente (id ?novo-id-c) (name ?nome)
            (data-nascimento ?data-nasc) (data-carta ?data-carta) 
            (data-validade-carta ?data-val-carta-cond) ))
    (assert (pedido-reserva (id-reserva ?num) (id-cliente ?novo-id-c)
            (data-levantamento ?data-lev) (data-devolucao ?data-dev)
            (classe ?classe) (modelo ?modelo) ))
    (retract ?reserva-nao-cliente) ; cliente deixa de ser nao cliente
)

; insere um novo pedido de reserva com o numero ?numpedido
(deffunction pedido-reserva (?numpedido)
    (assert (reserva-ativada ?numpedido))
)