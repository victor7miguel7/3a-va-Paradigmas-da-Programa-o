import Data.List (groupBy)

type NomePessoa = String
type NomeProduto = String
type Email = String
type Preco = Float
type Quantidade = Int

data Cliente = CL NomePessoa Email
  deriving (Show)

data Produto = P NomeProduto Preco Quantidade
  deriving (Show)

data Venda = CO NomePessoa NomeProduto Int
  deriving (Show)

type Loja = ([Cliente],[Produto],[Venda])

bd :: Loja
bd = ([CL "Joao" "joao@ufrpe.br", CL "Carlos" "carlos@ufrpe.br"],
      [P "Bola Nike" 199.9 5, P "Bola Adidas" 235.67 10],
      [CO "Joao" "Bola Nike" 2, CO "Carlos" "Bola Adidas" 1])

-- só impacta as novas vendas, as vendas efetuadas não são impactadas
atualizaQuantidadeProduto :: Loja -> NomeProduto -> Quantidade -> Loja
atualizaQuantidadeProduto (clientes, estoque, vendas) nomeProduto novaQuantidade =
  let novoEstoque = map (\(P nome preco qtd) ->
                           if nome == nomeProduto
                           then P nome preco (qtd - novaQuantidade)
                           else P nome preco qtd) estoque
  in (clientes, novoEstoque, vendas)

-- insere uma nova venda, apenas se o cliente e o produto existem.
-- Atualiza no estoque a quantidade disponível que resta do produto.
-- Se o estoque não tem quantidade suficiente, não realiza a venda.
vender :: Loja -> NomePessoa -> NomeProduto -> Quantidade -> Loja
vender (clientes, estoque, vendas) nomeCliente nomeProduto qtdVendida =
  let clienteExiste = any (\(CL nome _) -> nome == nomeCliente) clientes
      produtoExiste = any (\(P nome _ _) -> nome == nomeProduto) estoque
      estoqueSuficiente = any (\(P nome _ qtd) ->
                                 nome == nomeProduto && qtd >= qtdVendida) estoque
  in if clienteExiste && produtoExiste && estoqueSuficiente
       then let novaVenda = CO nomeCliente nomeProduto qtdVendida
                novoEstoque = map (\(P nome preco qtd) ->
                                     if nome == nomeProduto
                                     then P nome preco (qtd - qtdVendida)
                                     else P nome preco qtd) estoque
            in (clientes, novoEstoque, novaVenda:vendas)
       else (clientes, estoque, vendas)

-- retorna lista com o nome do produto e o valor total vendido
-- lista mostra uma única ocorrência de cada produto
totalVendasProduto :: Loja -> [(NomeProduto, Float)]
totalVendasProduto (_, _, vendas) =
  let vendasAgrupadas = groupBy (\(CO _ produto1 _) (CO _ produto2 _) -> produto1 == produto2) vendas
      calcularTotal vendasGrupo = (case head vendasGrupo of CO _ produto _ -> produto,
                                    fromIntegral $ sum [qtd | CO _ _ qtd <- vendasGrupo])
  in map calcularTotal vendasAgrupadas

-- Função main para permitir a compilação do código e testes
main :: IO ()
main = do
  putStrLn "Loja Inicial:"
  print bd

  putStrLn "\nAtualizando a quantidade do produto 'Bola Nike' para 3:"
  let lojaAtualizada1 = atualizaQuantidadeProduto bd "Bola Nike" 3
  print lojaAtualizada1

  putStrLn "\nRealizando uma venda de 1 'Bola Nike' para 'Joao':"
  let lojaVenda1 = vender lojaAtualizada1 "Joao" "Bola Nike" 1
  print lojaVenda1

  putStrLn "\nTentando vender 10 'Bola Nike' para 'Carlos' (sem estoque suficiente):"
  let lojaVenda2 = vender lojaVenda1 "Carlos" "Bola Nike" 10
  print lojaVenda2

  putStrLn "\nListando o total de vendas por produto:"
  let totalVendas = totalVendasProduto lojaVenda2
  print totalVendas

