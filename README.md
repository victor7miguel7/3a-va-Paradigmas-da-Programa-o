#Resolução da seguinte questão:

#Implemente as seguintes funções:

-- só impacta as novas vendas, as vendas efetuadas não são impactadas
-- atualizaQuantidadeProduto :: Loja -> NomeProduto -> Quantidade -> Loja

-- insere uma nova venda, apenas se o cliente e o produto existem. 
-- Atualiza no estoque a quantidade disponível que resta do produto. Se
-- o estoque não tem quantidade suficiente, não realiza a venda. 
-- vender :: Loja -> NomePessoa -> NomeProduto -> Quantidade -> Loja

-- retorna lista com o nome do produto e o valor total vendido
-- lista mostra uma única ocorrência de cada produto
-- totalVendasProduto :: Loja -> [(NomeProduto, Float)]


#Para os tipos haskell

type NomePessoa = String
type NomeProduto = String
type Email = String
type Preco = Float
type Quantidade = Int

data Cliente = CL NomePessoa Email
  deriving (Show)

-- Nome, preço e quantidade no estoque do produto
data Produto = P NomeProduto Preco Quantidade
  deriving (Show)

-- Nome do cliente, nome do produto e quantidade
data Venda = CO NomePessoa NomeProduto Int
  deriving (Show)

-- Exemplo de Cliente
cliente1 :: Cliente
cliente1 = CL "Joao" "joao@ufrpe.br"

-- Exemplo de Produto
produto1 :: Produto
produto1 = P "Bola Nike" 199.9 5

-- Exemplo de Venda
venda1 :: Venda
venda1 = CO "Joao" "Bola Nike" 2

clientes :: [Cliente] 
clientes = [cliente1, CL "Carlos" "carlos@ufrpe.br"]

estoque :: [Produto] 
estoque = [produto1, P "Bola Adidas" 235.67 10]

vendas :: [Venda]
vendas = [venda1, CO "Carlos" "Bola Adidas" 1]

type Loja = ([Cliente],[Produto],[Venda])

bd :: Loja
bd = (clientes,estoque,vendas)




