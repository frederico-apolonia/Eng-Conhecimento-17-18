; Grupo xx
; Frederico Apolónia
; Tiago Santos
; Renato Gondin

(deftemplate clientes
    (slot id
        (type INTEGER))
    (multislot name)
    (multislot data-nascimento)
    (multislot data-carta)
    (multislot data-validade-carta)
)

(deftemplate pedido-reserva
    (slot id-reserva
        (type INTEGER))
    (slot id-cliente
        (type INTEGER))
    (multislot data-levantamento)
    (multislot data-devolucao)
    (slot classe
        (type SYMBOL)
        (allowed-symbol mini economico compacto familiar executivo suv luxo)
        (default mini)
    (slot modelo)
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

; insere um novo pedido de reserva com o numero ?numpedido
(deffunction pedido-reserva (?numpedido)
    (assert (reserva-ativada ?numpedido))
)