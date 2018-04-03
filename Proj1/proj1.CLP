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

(deftemplate pedidos-reserva
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

(deffunction pedido-reserva (?numpedido)

    ; (filme (titulo "Raiders of the Lost Ark") (realizador "Steven Spielberg")
    ;    (ano 1981) (genero ação aventura))

    (assert (reserva-ativada ?numpedido)

    ; if ?decisao == true
    ; printout t Decisao: "O pedido " ?numpedido " foi aceite" crlf
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