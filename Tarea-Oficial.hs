-- | Nombres: Johanna Chan
-- |          Carlos Rodríguez
-- |
-- |  Carnés: 08-10218
-- |          07-XXXX
-- |
-- |--------------------------

--Laboratorio de Lenguajes de Programación
--USB / CI-3661 / Sep-Dic 2015 (Programación Funcional – 35 puntos)

--Funciones de Orden Superior (6 puntos)

--	Considere la función filter discutida en clase 
--		filter :: (a -> Bool) -> [a] -> [a]

--	Si bien la implantación de filter es directamente recursiva, 
--	y conveniente según la estrategia de evaluación de Haskell, 
--	se desea que Ud. implemente filter de maneras diferentes:

--		1. Usando listas por comprensión.
--			filterC = undefined 

			filterC :: (a->Bool) -> [a] -> [a]
			filterC f ls = [ x | x <- ls, f x]

--		2. Usando un map.
--			filterM = undefined

			--filterM :: (a->Bool) -> [a] -> []
			filterM f [] = []
			filterM f ls = getTrues (zip (map f ls) ls)

			getTrues :: [(Bool,a)] -> [a]
			getTrues [] = []
			getTrues (x:xs) = if (fst x) 
				then ((snd x) : (getTrues xs))
				else getTrues xs


--	Posiblemente tenga que apoyarse en funciones auxiliares vía
--	composición, pero no puede haber recursión directa.

--		3. Usando un foldr:

			--foldr :: (a -> b -> b) -> b -> [a] -> b

			--foldr :: (a -> [b] -> [b])  -> [b] -> [a] -> [b]

			filterF :: (a -> Bool) -> [a] -> [a]
			filterF f [] = []
			filterF f ls = foldr (\ a base -> if f a then a:base else base) [] ls

--Verificador de Tautologias (12 puntos)