1. Tornar o processo mais facil: 
A ideia e disponibiliza-lo em um website que a familia possa acessar usando um login e senha, ou permitindo o acesso somente a certos emails, que serao os membros da familia. penso em fazer isso no ambiente de google cloud, que e onde tenho um pouco mais de familiaridade. O csv base que alimenta o app viria de um google sheets, que eu poderia editar e entao o site se atualizaria com base no novo source. Imagens poderiam ser guardadas tb no google drive, ou entao num bucket GCS. CSV tb poderia ir para o bucket se for o caso.

2. Ajustar a visualizacao
Dependendo da tela, a ligacao entre bolas fica estreita demais ou afastada demais, e pode haver ausencia de conexao de uma ramificacao com uma pessoa
Casos de uso: Claiton e Janaina e Rodrigo tem maes diferentes da Catarina, entao poderiamos representar como linha tracejada o nome de pedro para cada par que gerou esses filhos.
Outra situacao e o Beto, que tambem nao fica com tracejado para a mae da Rafa.

3. Incluir forma de guardar imagens privadamente que sao source para o site.
Tanto imagens quanto dados devem ficar armazenadas de forma privada e segura para alimentar esse site.
Os dados idealmente deveriam ser lidos a partir de google sheets, e relativamente simples com python e key.

4. Tamanho da letra deveria ser variavel com zoom da pagina
Verificar se existe alguma forma de condicionar esse parametro ao tamanho do navegador.

5. Remover specs do csv.
Como o csv vai passar a ser integrado, podemos remover essas specs e colocar ali alguma ferramenta mais util.
Por exemplo, buscador que retorna ali do lado mesmo o nome inteiro e aniversario + foto.

---

6. Incluir analises de dados sobre a familia
Media de idade por geracao, numero de pessoas com quais profissoes, etc.
Somente em uma segunda etapa, apos 1 a 5 estarem implementados.
