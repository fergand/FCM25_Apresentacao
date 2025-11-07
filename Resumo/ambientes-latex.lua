-- ambientes-customizados.lua

function Div(el)
  -- Ambiente Questão, que corresponde ao \newtheorem{questao}
  if el.classes:includes("questao") then
    table.insert(
      el.content, 1,
      pandoc.RawBlock("latex", "\\begin{questao}")
    )
    table.insert(
      el.content,
      pandoc.RawBlock("latex", "\\end{questao}")
    )
    return el.content
  end

  -- Ambiente Solução, que corresponde ao \newtheorem*{solucao}
  if el.classes:includes("solucao") then
    table.insert(
      el.content, 1,
      pandoc.RawBlock("latex", "\\begin{solucao}")
    )
    table.insert(
      el.content,
      pandoc.RawBlock("latex", "\\end{solucao}")
    )
    return el.content
  end
  -- Ambiente Exemplo, correspondente ao \newtheorem{example}
  if el.classes:includes("exemplo") then
    table.insert(
      el.content, 1,
      pandoc.RawBlock("latex", "\\begin{exemplo}")
    )
    table.insert(
      el.content,
      pandoc.RawBlock("latex", "\\end{exemplo}")
    )
    return el.content
  end
end