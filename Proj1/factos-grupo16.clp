; Grupo 16
; Frederico Apolónia
; Tiago Santos
; Renato Gondin

(deffacts clientes
    (cliente (id 1111111) (name Renato Maudim) (data-nascimento 26 06 1986)
    (data-carta 12 5 2005) (data-validade-carta 12 5 2050))
    (cliente (id 2222222) (name Daniel Silva) (data-nascimento 3 4 1992)
    (data-carta 12 3 2016) (data-validade-carta 12 3 2045))
    (cliente (id 5555555) (name Maria Silva) (data-nascimento 6 8 1961)
    (data-carta 13 2 1985) (data-validade-carta 6 8 2021))
)

(deffacts pedidos-reserva
    (pedido-reserva (id-reserva 21) (id-cliente 1111111)
    (data-levantamento 1 1 2018) (data-devolucao 16 1 2018)
    (classe mini) (modelo smart))
    (pedido-reserva (id-reserva 22) (id-cliente 1111111)
    (data-levantamento 17 1 2018) (data-devolucao 20 1 2018)
    (classe suv) (modelo smart))
    (pedido-reserva (id-reserva 24) (id-cliente 5555555)
    (data-levantamento 19 3 2018) (data-devolucao 30 4 2018) )

    (pedidos-reserva-nao-cliente (id-reserva 23)
    (data-levantamento 1 1 2018) (data-devolucao 15 1 2018)
    (nome Ze Pascal) (data-nascimento 15 02 1996)
    (data-carta 15 01 2015) (data-validade-carta 15 01 2040) )
)

(deffacts recepcao-carros
    (assert (recepcao-automovel (id-reserva 21) (cliente-pagou sim) (ha-danos nao)))
)
