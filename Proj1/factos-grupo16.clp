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
