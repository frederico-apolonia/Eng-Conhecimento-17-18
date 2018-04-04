; Grupo xx
; Frederico Apolónia
; Tiago Santos
; Renato Gondin

(deftemplate clientes
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
    (slot classe)
    (slot modelo)
)

(deftemplate pedidos-reserva-nao-cliente
    (slot id-reserva)
    (multislot data-levantamento)
    (multislot data-devolucao)
    (slot classe)
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
    ?pedido <- (pedido-reserva (id-reserva ?num) (id-cliente ?id-c) (data-levantamento ?d ?m ?a)) 
    (pedido-reserva (id-reserva ?num1&:(< ?num1 ?num)) (id-cliente ?id-c) (data-devolucao ?d-d ?m-d ?a-d))

    (test (or (or (< ?d ?d-d ) (< ?m ?m-d) ) (> ?a ?a-d) ) )

    =>
    (retract ?nova-reserva ?pedido)
    (printout t "Reserva removida. Ja existe uma reserva em curso feita por este cliente." crlf)
)

; insere um novo pedido de reserva com o numero ?numpedido
(deffunction pedido-reserva (?numpedido)
    (assert (reserva-ativada ?numpedido))
)

; Compara o dia/mes/ano de levantamento com o dia/mes/ano de levantamento
; e recepcao da reserva anterior.
; ?l e a data de levantamento da reserva atual
; ?r-a e a data de recepcao da reserva anterior
; True se esta bom False se nao
(deffunction compare-date-entry (?l ?r-a)

    (if (>= ?l ?r-a)
        then
        (return 1)
        else
        (return 0)
    )

)