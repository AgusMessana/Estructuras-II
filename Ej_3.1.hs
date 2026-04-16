data Color = Color Float Float Float deriving Show
mezclar::Color->Color->Color
mezclar(Color r1 g1 b1) (Color r2 g2 b2) =
  Color((r1 + r2) / 2) ((g1 + g2) / 2) ((b1 + b2) / 2)